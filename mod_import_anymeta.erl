%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011-2015 Marc Worrell
%% @doc Import data from an Anymeta website. Check mod_import_anymeta_dispatch for
%% redirecting old Anymeta urls to the new imported locations.

%% Copyright 2011-2015 Marc Worrell
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% TODO: Add all original anymeta types as resources of the category "anymeta_type".
%%       Make the resources of the form: 
%%       [ {name, "anytype_<name>"}, {title, "<<original title>>"} ]
%%       Links all imported resources to the original anymeta types w/ predicate 'hasanytype'
%%       This enables later correction of the imported data.
%%
%%       creator + created of edges
%%       creator, modifier, created, modified of resources (when needed make stubs)

-module(mod_import_anymeta).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Import Anymeta Site").
-mod_description("Import data from an Anymeta web site.").
-mod_depends([mod_import_anymeta_dispatch]).
-mod_prio(300).

-export([
    init/1,

    observe_admin_menu/3,
    event/2,
    
    find_any_id/3,
    do_import/2,
    import_single/2,
    import_referring_edge/4,
    import_thing/5,
    test_host/2,
    get_thing/3
]).

-include_lib("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").
-include("include/mod_import_anymeta.hrl").

-define(RETRIES, 4).


init(Context) ->
    ensure_anymeta_type(Context),
    case z_db:table_exists(import_anymeta, Context) of
        true ->
            % Conversion: Add host and stub
            Cols = z_db:columns(import_anymeta, Context),
            HasHost = lists:keyfind(host, #column_def.name, Cols),
            case HasHost of
                false ->
                    lager:info("Updating table import_anymeta."),
                    [] = z_db:q("alter table import_anymeta ADD COLUMN host character varying(255), ADD COLUMN stub boolean", Context),
                    z_db:flush(Context),
                    ok;
                _ ->
                    ok
            end;
        false ->
            [] = z_db:q("
                create table import_anymeta (
                    rsc_id int not null,
                    rsc_uri character varying(255) not null,
                    anymeta_id int,
                    host character varying(255),
                    stub boolean,
                    imported timestamp with time zone not null,
                    
                    constraint import_anymeta_pkey primary key (rsc_uri, host),
                    constraint fk_import_anymeta_rsc_id foreign key (rsc_id)
                        references rsc(id)
                        on update cascade
                        on delete cascade
                )
            ", Context),
            [] = z_db:q("
                create index import_anymeta_anymeta_id on import_anymeta(anymeta_id)
            ", Context),
            [] = z_db:q("
                create index import_anymeta_rsc_id on import_anymeta(rsc_id)
            ", Context),
            [] = z_db:q("
                create index import_anymeta_host_key on import_anymeta(host)
            ", Context),
            ok
    end,
    case z_db:table_exists(import_anymeta_edge, Context) of
        true ->
            ok;
        false ->
            [] = z_db:q("
                create table import_anymeta_edge (
                    id int not null,
                    props bytea,
                    
                    constraint import_anymeta_edge_pkey primary key (id),
                    constraint fk_import_anymeta_edge_id foreign key (id)
                        references edge(id)
                        on update cascade
                        on delete cascade
                )
            ", Context),
            ok
    end.


observe_admin_menu(admin_menu, Acc, Context) -> [
     #menu_item{id=import_anymeta,
                parent=admin_modules,
                label=?__("Import from Anymeta", Context),
                url={admin_import_anymeta},
                visiblecheck={acl, use, ?MODULE}}

     |Acc].


event(#submit{message=find_imported}, Context) ->
    AnymetaId = z_convert:to_integer(z_string:trim(z_context:get_q_validated("imported_id", Context))),
    AnymetaHost = z_string:trim(z_context:get_q("imported_host", Context)),
    case z_db:q1("select rsc_id from import_anymeta where anymeta_id = $1 and host = $2", [AnymetaId, AnymetaHost], Context) of
        undefined ->
            z_render:wire({fade_in, [{target, "find-error"}]}, Context);
        RscId ->
            z_render:wire({redirect, [{dispatch, admin_edit_rsc}, {id, RscId}]}, Context)
    end;

%% @doc Accept the id range to be imported.  Starts the import process, progress is posted to the user-agent.
event(#submit{message=import_anymeta, form=Form}, Context) ->
    case z_acl:is_allowed(use, mod_import_anymeta, Context) of
        false ->
            z_render:growl_error(?__("Sorry, you are not allowed to use the Anymeta import module.", Context), Context);
        true ->
            Secret   = z_string:trim(z_context:get_q("sysadmin-pw", Context)),
            From     = z_convert:to_integer(z_context:get_q_validated("start-id", Context)),
            To       = z_convert:to_integer(z_context:get_q_validated("end-id", Context)),
            Host     = z_string:trim(z_context:get_q("host", Context)),
            HostOrg0 = z_string:trim(z_context:get_q("host_original", Context)),
            CGId = z_convert:to_integer(z_context:get_q("content-group", Context)),
            IsOnlyAuthoritative = z_convert:to_bool(z_context:get_q("only-authoritative", Context)),
            Blobs = case z_convert:to_list(z_context:get_q("blobs", Context)) of
                            "e" -> edgesonly;
                            "t" -> tagsonly;
                            "n" -> no;
                            "y" -> yes;
                            "b" -> blobsonly
                       end,
            HostOriginal = case z_string:trim(HostOrg0) of
                              "" -> Host;
                              _ -> HostOrg0
                           end,

            lager:info("Anymeta import started."),

            z_context:set_session(anymeta_host, Host, Context),
            z_context:set_session(anymeta_host_original, HostOriginal, Context),
            
            Opt = #opt{
                host = Host,
                host_original = HostOriginal,
                from = From,
                to = To,
                blobs = Blobs,
                is_only_authoritative = IsOnlyAuthoritative,
                content_group = CGId,
                secret = Secret
            },

            case do_import(Opt, Context) of
                ok ->
                    z_render:wire([{fade_in, [{target, "import-started"}]}, {hide, [{target, Form}]}], Context);
                {error, nxdomain} ->
                    z_render:wire([{fade_in, [{target, "import-nxdomain"}]}, {hide, [{target, "import-error"}]}], Context);
                {error, _Other} ->
                    z_render:wire([{fade_in, [{target, "import-error"}]}, {hide, [{target, "import-nxdomain"}]}], Context)
            end
    end.


find_any_id(AnyId, Host, Context) when is_list(AnyId) ->
    find_any_id(z_convert:to_binary(AnyId), Host, Context);
find_any_id(AnyId, Host, Context) when is_binary(AnyId) ->
    case z_utils:only_digits(AnyId) of
        true ->
            % anyMeta id
            case z_db:q1("select rsc_id from import_anymeta where anymeta_id = $1 and host = $2", 
                         [z_convert:to_integer(AnyId), Host], 
                         Context)
            of
                undefined -> undefined;
                RscId -> {ok, RscId}
            end;
        false ->
            % rsc uri or name
            case is_http_uri(AnyId) of
                true ->
                    case z_db:q1("select rsc_id from import_anymeta where rsc_uri = $1", [AnyId], Context) of
                        undefined -> undefined;
                        RscId -> {ok, RscId}
                    end;
                false ->
                    case m_rsc:name_to_id(AnyId, Context) of
                        {ok, RscId} -> {ok, RscId};
                        {error, _} -> undefined
                    end
            end
    end;
find_any_id(AnyId, Host, Context) when is_integer(AnyId)->
    case z_db:q1("select rsc_id from import_anymeta where anymeta_id = $1 and host = $2", 
                 [z_convert:to_integer(AnyId), Host], 
                 Context)
    of
        undefined -> undefined;
        RscId -> {ok, RscId}
    end.


is_http_uri(<<"http://", _/binary>>) -> true;
is_http_uri(<<"https://", _/binary>>) -> true;
is_http_uri(_) -> false.


import_single(Opt, Context) ->
    import_single(Opt, Context, 1).

import_single(#opt{from=AnyId} = Opt, Context, Retries) ->
    case get_thing(Opt, AnyId, Context) of
        {ok, Thing} ->
            progress(io_lib:format("~w: importing", [AnyId]), Context),
            _Stats1 = import_thing(Opt, AnyId, Thing, #stats{}, Context),
            {ok, Thing};
        {error, no_service} ->
            progress(io_lib:format("~w: got 503 - waiting 10 seconds before retry.", [AnyId]), Context),
            % Anymeta servers give a 503 when they are overloaded.
            % Sleep for 10 seconds and then retry our request.
            timer:sleep(10000),
            import_single(Opt, Context);
        {error, timeout} ->
            progress(io_lib:format("~w: got timeout - waiting 5 seconds before retry.", [AnyId]), Context),
            timer:sleep(5000),
            import_single(Opt, Context);
        {error, not_found} ->
            progress(io_lib:format("~w: not found, skipping to next", [AnyId]), Context),
            {error, not_found};
        {error, Reason} = Error when Reason =:= gone; Reason =:= unauthorized ->
            progress(io_lib:format("~w: error, skipping to next (error: ~p)", [AnyId, Reason]), Context),
            Error;
        {error, Reason} = Error ->
            case Retries < ?RETRIES of
                true ->
                    progress(io_lib:format("~w: error, will retry in 1 second (error: ~p)", [AnyId, Reason]), Context),
                    timer:sleep(1000),
                    import_single(Opt, Context, Retries+1);
                false ->
                    progress(io_lib:format("~w: error, skipping to next (error: ~p)", [AnyId, Reason]), Context),
                    Error
            end
    end.

import_referring_edge(Opt, AnyId, ObjectId, Context) ->
    case get_thing(Opt#opt{blobs=no}, AnyId, Context) of
        {ok, Thing} ->
            import_referring_edge_1(Opt, ObjectId, Thing, Context);
        {error, no_service} ->
            progress(io_lib:format("~w: got 503 - waiting 10 seconds before retry.", [AnyId]), Context),
            % Anymeta servers give a 503 when they are overloaded.
            % Sleep for 10 seconds and then retry our request.
            timer:sleep(10000),
            import_referring_edge(Opt, AnyId, ObjectId, Context);
        {error, timeout} ->
            progress(io_lib:format("~w: got timeout - waiting 5 seconds before retry.", [AnyId]), Context),
            timer:sleep(5000),
            import_referring_edge(Opt, AnyId, ObjectId, Context);
        {error, not_found} ->
            progress(io_lib:format("~w: not found, skipping referring edges", [AnyId]), Context),
            {error, not_found};
        {error, _Reason} = Error ->
            progress(io_lib:format("~w: error, skipping referring edges", [AnyId]), Context),
            Error
    end.

import_referring_edge_1(Opt, ObjectId, Thing, Context) ->
    RscUri = resource_uri(Thing),
    case check_previous_import(RscUri, Context) of
        {ok, RscId} ->
            Edges = filter_on_object(proplists:get_value(<<"edge">>, Thing), ObjectId),
            Stats1 = import_edges(Opt, RscId, Edges, #stats{}, Context),
            Stats2 = import_keywords(Opt, RscId, proplists:get_value(<<"keyword">>, Thing), Stats1, Context),
            Stats3 = import_keywords(Opt, RscId, proplists:get_value(<<"tag">>, Thing), Stats2, Context),
            handle_delayed(Stats3#stats.delayed, Opt, Stats3#stats{delayed=[]}, Context);
        _ ->
            ok
    end.

filter_on_object(null, _ObjectId) ->
    [];
filter_on_object(Edges, ObjectId) ->
    lists:filter(fun({struct, Edge}) ->
                    {struct, Props} = proplists:get_value(<<"edge">>, Edge),
                    ObjUrl = proplists:get_value(<<"object_id">>, Props),
                    z_convert:to_binary(ObjectId) == lists:last(binary:split(ObjUrl, <<"/">>, [global]))
                 end,
                 Edges).


do_import(#opt{from=undefined} = Opt, Context) ->
    do_import(Opt#opt{from=1}, Context);
do_import(Opt, Context) ->
    case test_host(Opt, Context) of
        ok ->
            start_import(Opt, Context);
        Err ->
            Err
    end.

    start_import(Opt, Context) ->
        ContextPruned = z_context:prune_for_async(Context),
        spawn(fun() ->
                    import_loop(Opt, #stats{}, ContextPruned)
              end),
        ok.
    
    test_host(Opt, Context) ->
        Url = get_url(Opt#opt.from, Opt#opt{blobs=no}),
        progress(io_lib:format("TEST HOST: pinging ~p ...", [Url]), Context),
        case get_request(get, Url) of
            {ok, {{_, 200, _},Hs, _}} ->
                progress("TEST HOST: 200 OK", Context),
                case proplists:get_value("content-type", Hs) of
                    "application/json" ++ _ -> ok;
                    _ -> {error, content_type}
                end;
            {ok, {{_, 404, _},Hs, _}} -> 
                progress("TEST HOST: 404 Not Found", Context),
                case z_string:to_lower(proplists:get_value("x-powered-by", Hs, [])) of
                    "anymeta" ++ _ = PoweredBy->
                        progress(io_lib:format("TEST HOST: Is Anymeta server (~p)", [PoweredBy]), Context),
                        ok;
                    PoweredBy ->
                        progress(io_lib:format("TEST HOST: FAIL! Not an Anymeta server (~p)", [PoweredBy]), Context),
                        {error, not_anymeta}
                end;
            {ok, {{_, 401, _},Hs, _}} -> 
                progress("TEST HOST: 401 Unauthorized", Context),
                case z_convert:to_list(z_string:to_lower(proplists:get_value("x-powered-by", Hs, []))) of
                    "anymeta" ++ _ = PoweredBy->
                        progress(io_lib:format("TEST HOST: Is Anymeta server (~p)", [PoweredBy]), Context),
                        {error, unauthorized};
                    PoweredBy ->
                        progress(io_lib:format("TEST HOST: FAIL! Not an Anymeta server (~p)", [PoweredBy]), Context),
                        {error, not_anymeta}
                end;
            {ok, {{_, 503, _},Hs, _}} -> 
                progress("TEST HOST: 503 Service Not Available", Context),
                case z_convert:to_list(z_string:to_lower(proplists:get_value("x-powered-by", Hs, []))) of
                    "anymeta" ++ _ = PoweredBy -> 
                        progress(io_lib:format("TEST HOST: Is Anymeta server (~p)", [PoweredBy]), Context),
                        ok;
                    PoweredBy -> 
                        progress(io_lib:format("TEST HOST: FAIL! Not an Anymeta server (~p)", [PoweredBy]), Context),
                        {error, not_anymeta}
                end;
            {error, _Reason} = Err ->
                progress(io_lib:format("TEST HOST: FAIL! Unexpected Error (~p)", [Err]), Context),
                Err;
            {ok, Result} ->
                progress(io_lib:format("TEST HOST: FAIL! Unexpected Result<br/>~p", [Result]), Context),
                {error, unexpected_result}
        end.

get_url(Id, Opt) ->
    "http://"++Opt#opt.host++"/thing/"++integer_to_list(Id)++"/json?secret="++Opt#opt.secret++blobs_arg(Opt#opt.blobs).

blobs_arg(edgesonly) -> "&skip-blob=1";
blobs_arg(tagsonly) -> "&skip-blob=1";
blobs_arg(no) -> "&skip-blob=1";
blobs_arg(yes) -> "";
blobs_arg(blobsonly) -> "".

get_thing(Opt, Id, Context) ->
    Url = get_url(Id, Opt),
    case get_request(get, Url) of
        {ok, {
            {_HTTP, 200, _OK},
            Headers,
            Body
        }} ->
            case proplists:get_value("content-type", Headers) of
                "application/json" ++ _ ->
                    case catch mochijson2:decode(Body) of
                        {struct, Props} -> {ok, Props};
                        invalid_utf8 -> 
                            case catch mochijson2:decode(z_string:sanitize_utf8(Body)) of
                                {struct, Props} -> {ok, Props};
                                invalid_utf8 -> {error, invalid_utf8}; 
                                _ -> {error, no_json_struct}
                            end;
                        _ -> {error, no_json_struct}
                    end;
                CT ->
                    {error, {unexpected_content_type, CT}}
            end;
        {error, _Reason} = Err ->
            Err;
        {ok, {{_, 503, _}, _, _}} ->
            {error, no_service};
        {ok, {{_, 410, _}, _, _}} ->
            {error, gone};
        {ok, {{_, 404, _}, _, _}} ->
            {error, not_found};
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, Other} ->
            progress(io_lib:format("FAIL! ~p Unexpected Result<br/>~p", [Url, Other]), Context),
            {error, unexpected_result}
    end.

get_request(Method, Url) ->
    Headers = [],
    httpc:request(Method, {Url, Headers}, [{autoredirect, true}, {relaxed, true}, {timeout, 60000}], []).


%% @doc Fetch all items from the given host
import_loop(#opt{to=undefined} = Opt, #stats{consequetive_notfound=NF, delayed=Delayed} = Stats, Context) when NF > 500 ->
    % Signal the end of the import to the UI
    progress("STOP - found more than 500 consequetive not founds.<br/>", Context),
    handle_delayed(Delayed, Opt, Stats#stats{delayed=[]}, Context);
import_loop(#opt{from=From, to=To} = Opt, Stats, Context) when is_integer(To), From > To ->
    % Signal the end of the import to the UI
    progress(io_lib:format("STOP - At last import anymeta id (~w).<br/>", [To]), Context),
    handle_delayed(Stats#stats.delayed, Opt, Stats#stats{delayed=[]}, Context);
import_loop(Opt, Stats, Context) ->
    import_loop_1(Opt, Stats, Context, 1).


import_loop_1(Opt, Stats, Context, Retries) ->
    progress(io_lib:format("~w: fetching from ~p", [Opt#opt.from, Opt#opt.host]), Context),
    case get_thing(Opt, Opt#opt.from, Context) of
        {ok, Thing} ->
            progress(io_lib:format("~w: importing", [Opt#opt.from]), Context),
            Stats1 = import_thing(Opt, Opt#opt.from, Thing, Stats, Context),
            Stats2 = Stats1#stats{
                found=Stats#stats.found+1, 
                consequetive_notfound=0
            },
            import_loop(Opt#opt{from=Opt#opt.from+1}, Stats2, Context);
        {error, no_service} ->
            progress(io_lib:format("~w: got 503 - waiting 10 seconds before retry.", [Opt#opt.from]), Context),
            % Anymeta servers give a 503 when they are overloaded.
            % Sleep for 10 seconds and then retry our request.
            timer:sleep(10000),
            import_loop(Opt, Stats, Context);
        {error, timeout} ->
            progress(io_lib:format("~w: got timeout - waiting 5 seconds before retry.", [Opt#opt.from]), Context),
            timer:sleep(5000),
            import_loop(Opt, Stats, Context);
        {error, not_found} ->
            progress(io_lib:format("~w: not found, skipping to next", [Opt#opt.from]), Context),
            Stats1 = Stats#stats{
                notfound=Stats#stats.notfound+1, 
                consequetive_notfound=Stats#stats.consequetive_notfound+1,
                error=[{Opt#opt.from, notfound} | Stats#stats.error]
            },
            import_loop(Opt#opt{from=Opt#opt.from+1}, Stats1, Context);
        {error, Reason} when Reason =:= gone; Reason =:= unauthorized ->
            progress(io_lib:format("~w: error, skipping to next (error: ~p)", [Opt#opt.from, Reason]), Context),
            Stats1 = Stats#stats{
                error=[{Opt#opt.from, Reason} | Stats#stats.error]
            },
            import_loop(Opt#opt{from=Opt#opt.from+1}, Stats1, Context);
        {error, Reason} ->
            case Retries < ?RETRIES of
                true ->
                    progress(io_lib:format("~w: error, will retry in 1 second (error: ~p)", [Opt#opt.from, Reason]), Context),
                    timer:sleep(1000),
                    import_loop_1(Opt, Stats, Context, Retries+1);
                false ->
                    progress(io_lib:format("~w: error, skipping to next (error: ~p)", [Opt#opt.from, Reason]), Context),
                    Stats1 = Stats#stats{
                        error=[{Opt#opt.from, Reason} | Stats#stats.error]
                    },
                    import_loop(Opt#opt{from=Opt#opt.from+1}, Stats1, Context)
            end
    end.


    handle_delayed([], _Opt, #stats{delayed=[]} = Stats, _Context) ->
        {ok, Stats};
    handle_delayed([], Opt, #stats{delayed=Delayed} = Stats, Context) ->
        handle_delayed(Delayed, Opt, Stats#stats{delayed=[]}, Context);
    handle_delayed([{keyword, RscId, AnymetaId}|Ds], Opt, Stats, Context) ->
        AnyId = z_convert:to_integer(AnymetaId),
        FoundId = case find_any_id(AnyId, Opt#opt.host, Context) of
                    undefined ->
                        {ok, Stats1} = import_loop(Opt#opt{from=AnyId,to=AnyId}, Stats, Context),
                        find_any_id(AnyId, Opt#opt.host, Context);
                    FndId ->
                        Stats1 = Stats,
                        FndId
                  end,
        case FoundId of
            {ok, KwId} ->
                progress(io_lib:format("    Edge ~w -[~p]-> ~w", [RscId,subject,KwId]), Context),
                {ok, _} = m_edge:insert(RscId, subject, KwId, [no_touch], Context);
            undefined ->
                progress(io_lib:format("~w: not imported, skipping as keyword of ~w", [AnyId, RscId]), Context)
        end,
        handle_delayed(Ds, Opt, Stats1, Context).


import_thing(#opt{blobs=tagsonly} = Opt, _AnymetaId, Thing, Stats, Context) ->
    RscUri = resource_uri(Thing),
    case check_previous_import(RscUri, Context) of
        {ok, RscId} ->
            Stats1 = import_keywords(Opt, RscId, proplists:get_value(<<"keyword">>, Thing), Stats, Context),
            import_keywords(Opt, RscId, proplists:get_value(<<"tag">>, Thing), Stats1, Context);
        _ ->
            Stats
    end;
import_thing(#opt{blobs=edgesonly} = Opt, _AnymetaId, Thing, Stats, Context) ->
    RscUri = resource_uri(Thing),
    case check_previous_import(RscUri, Context) of
        {ok, RscId} ->
            Stats1 = import_edges(Opt, RscId, proplists:get_value(<<"edge">>, Thing), Stats, Context),
            Stats2 = import_keywords(Opt, RscId, proplists:get_value(<<"keyword">>, Thing), Stats1, Context),
            import_keywords(Opt, RscId, proplists:get_value(<<"tag">>, Thing), Stats2, Context);
        _ ->
            Stats
    end;
import_thing(#opt{blobs=blobsonly} = _Opt, AnymetaId, Thing, Stats, Context) ->
    RscUri = resource_uri(Thing),
    case check_previous_import(RscUri, Context) of
        {ok, RscId} ->
            maybe_import_file(AnymetaId, RscId, Thing, Stats, Context);
        _ ->
            Stats
    end;
import_thing(#opt{blobs=Blobs} = Opt, AnymetaId, Thing, Stats, Context) when Blobs =:= no; Blobs =:= yes ->
    % Check if the <<"lang">> section is available, if so then we had read acces, otherwise skip
    case skip(Opt, Thing, Context) of
        false ->
            maybe_stub_origin(Opt, Thing, Context),

            {struct, Texts} = proplists:get_value(<<"lang">>, Thing),
            Rights = proplists:get_value(<<"rights">>, Thing),
            Findable = proplists:get_value(<<"findable">>, Thing),
            Matchable = proplists:get_value(<<"matchable">>, Thing),
            Texts1 = [ map_texts(map_language(Lang), T) || {Lang, {struct, T}} <- Texts ],
            Texts2 = group_by_lang(Texts1),
            Websites = proplists:get_value(website, Texts2),
            Texts3 = proplists:delete(website, Texts2),
            {trans, TR} = proplists:get_value(title, Texts3),
            Langs = [ Iso || {Iso, _} <- TR ],
            OtherFields = fix_pubstart(map_fields(Thing)),
            Fields0 = [
                        fix_media_category(convert_category(Thing, Context), Thing),
                        {anymeta_id, AnymetaId},
                        {anymeta_rsc_uri, resource_uri(Thing)},
                        {anymeta_host, Opt#opt.host},
                        {anymeta_is_user, proplists:is_defined(<<"auth">>, Thing)},
                        {language, Langs},
                        {rights, Rights},
                        {is_unfindable, not z_convert:to_bool(Findable)},
                        % {is_unfindable, not (       z_convert:to_bool(Findable) 
                        %                     andalso z_convert:to_bool(Matchable))},
                        {alternative_uris, proplists:get_value(<<"alt_uri">>, Thing)}
                        |OtherFields
                     ]
                     ++ fetch_authoritative(Thing)
                     ++ fetch_content_group(Opt, Thing, Context)
                     ++ fetch_creator_modifier(Opt, Thing, Context)
                     ++ fetch_media(Thing)
                     ++ maybe_set_redirect_uri(Thing, fetch_address(Thing))
                     ++ fetch_name(Thing)
                     ++ Texts3,

            Fields = case fetch_website(Websites) of
                        undefined -> Fields0;
                        <<>> -> Fields0;
                        WS -> 
                            case proplists:get_value(website, Fields0) of
                                <<>> -> [ {website, WS} | proplists:delete(website, Fields0) ];
                                undefined -> [ {website, WS} | proplists:delete(website, Fields0) ];
                                _ -> Fields0
                            end
                     end,
            
            Fields1 = fix_html_summary(map_sections(Fields)),
            Fields2 = convert_query(Fields1, Thing, Context),
            Fields3 = fix_rsc_name(Fields2),
            Fields4 = maybe_add_allday(Fields3, Context),
            FieldsFinal = z_notifier:foldl(import_anymeta_fields, Fields4, Context),

            case write_rsc(Opt#opt.host, Opt#opt.host_original, AnymetaId, FieldsFinal, Stats, Context) of
                {ok, RscId, Stats1} ->
                    % Import all edges
                    Stats2 = import_edges(Opt, RscId, proplists:get_value(<<"edge">>, Thing), Stats1, Context),
                    Stats3 = import_keywords(Opt, RscId, proplists:get_value(<<"keyword">>, Thing), Stats2, Context),
                    Stats4 = import_keywords(Opt, RscId, proplists:get_value(<<"tag">>, Thing), Stats3, Context),
                    maybe_add_identity(RscId, proplists:get_value(<<"auth">>, Thing), Context),
                    case Blobs of
                        no -> Stats;
                        yes -> maybe_import_file(AnymetaId, RscId, Thing, Stats4, Context)
                    end;
                {error, Stats1} ->
                    Stats1
            end;
        true ->
            % Import was denied, skip this thing but import outgoing edges
            Stats1 = Stats#stats{error=[{AnymetaId, skipped} | Stats#stats.error]},
            case check_previous_import(resource_uri(Thing), Context) of
                {ok, RscId} ->
                    Stats2 = import_edges(Opt, RscId, proplists:get_value(<<"edge">>, Thing), Stats1, Context),
                    Stats3 = import_keywords(Opt, RscId, proplists:get_value(<<"keyword">>, Thing), Stats2, Context),
                    import_keywords(Opt, RscId, proplists:get_value(<<"tag">>, Thing), Stats3, Context);
                none ->
                    Stats1
            end
    end.

    maybe_add_allday(Fields, Context) ->
        Cat = proplists:get_value(category, Fields),
        case lists:member(person, m_category:is_a(Cat, Context)) of
            true ->
                [{date_is_all_day, true} | Fields];
            false ->
                Fields
        end.

    maybe_add_identity(1, _, _Context) ->
        ok;
    maybe_add_identity(RscId, {struct, Auth}, Context) ->
        case m_identity:get_username(RscId, Context) of
            undefined ->
                case z_convert:to_bool(proplists:get_value(<<"enabled">>, Auth)) of
                    true ->
                        m_identity:set_username_pw(RscId, proplists:get_value(<<"email">>, Auth), z_ids:id(), Context);
                    false ->
                        ok
                end;
            _Username ->
                ok
        end;
    maybe_add_identity(_RscId, undefined, _Context) ->
        ok.

    maybe_import_file(AnymetaId, RscId, Thing, Stats, Context) ->
        case proplists:get_value(<<"file">>, Thing) of
            undefined ->
                Stats;
            {struct, File} -> 
                Filename = proplists:get_value(<<"original_file">>, File),
                case proplists:get_value(<<"file_blob">>, File) of
                    undefined ->
                        case proplists:get_value(<<"uri">>, File) of
                            undefined ->
                                Stats;
                            _ ->
                                case proplists:get_value(<<"mime">>, File) of
                                    <<"application/x-youtube">> ->
                                        Stats;
                                    _ ->
                                        Url = proplists:get_value(<<"uri">>, File),
                                        m_media:replace_url(Url, RscId, [], Context),
                                        Stats
                                end
                        end;
                    {struct, Fileblob} -> 
                        {struct, Fileblob} = proplists:get_value(<<"file_blob">>, File),
                        case proplists:get_value(<<"encode">>, Fileblob) of
                            <<"base64">> ->
                                Data = base64:decode(proplists:get_value(<<"data">>, Fileblob)),
                                erlang:garbage_collect(),
                                write_file(AnymetaId, RscId, Filename, Data, Stats, Context);
                            Encoding ->
                                Stats#stats{error=[{AnymetaId, {unknown_file_encoding, Encoding}} | Stats#stats.error]}
                        end
                end
        end.

    skip(Opt, Thing, Context) ->
        case is_authoritative(Thing) or not Opt#opt.is_only_authoritative of
            true ->
                case proplists:get_value(<<"lang">>, Thing) of
                    undefined ->
                        true;
                    _ ->
                        case is_empty(proplists:get_value(<<"lang">>, Thing)) of
                            true ->
                                true;
                                _ ->
                                case proplists:get_value(<<"uri">>, Thing) of
                                    <<"javascript:any_action", _/binary>> -> true;
                                    <<"../admin.php", _/binary>> -> true;
                                    _ ->
                                        % TODO: add a callback for skippable kinds
                                        case proplists:get_value(<<"kind">>, Thing) of
                                            <<"UNKNOWN">> -> true;
                                            <<"ROLE">> -> true;
                                            <<"TYPE">> -> false;
                                            <<"LANGUAGE">> -> true;
                                            _ -> false
                                        end
                                end
                        end
                end;
            false ->
                % If this is a resource with an 'origin' then we must register it.
                % It should be known on both its origin uri and the resource uri
                maybe_stub_origin(Opt, Thing, Context),
                true
        end.

    fetch_website(undefined) -> 
        undefined;
    fetch_website({trans, Tr}) ->
        case [ X || {_Iso,X} <- Tr, X /= <<>> ] of
            [] -> undefined;
            [Url|_] -> Url
        end.

    fetch_media(Thing) ->
        case proplists:get_value(<<"file">>, Thing) of
            {struct, File} ->
                case proplists:get_value(<<"mime">>, File) of
                    <<"application/x-youtube">> ->
                        [{oembed_url, proplists:get_value(<<"uri">>, File)}];
                    _ ->
                        []
                end;
            _ ->
                []
        end.

    fetch_content_group(Opt, Thing, Context) ->
        {<<"trust">>, {struct, Trust}} = proplists:lookup(<<"trust">>, Thing),
        {<<"view">>, {struct, View}} = proplists:lookup(<<"view">>, Trust),
        case proplists:get_value(<<"level">>, View) of
            <<"ME">> ->
                [{content_group, private_content_group}];
            <<"PREDICATE">> ->
                [{content_group, member_content_group}];
            <<"MEMBERS">> ->
                [{content_group, member_content_group}];
            _ ->
                fetch_content_group_theme(Opt, Thing, Context)
        end.

    fetch_content_group_theme(Opt, Thing, Context) ->
        case proplists:get_value(<<"theme">>, Thing) of
            none ->
                [{content_group_id, Opt#opt.content_group}];
            undefined ->
                [{content_group_id, Opt#opt.content_group}];
            [AnyId|_] ->
                % find the content group or create one
                CurrRscUri = binary_to_list(resource_uri(Thing)),
                BaseUri = string:substr(CurrRscUri, 1, string:str(CurrRscUri, "/id/") + 3),
                RscUri = BaseUri ++ binary_to_list(AnyId),
                
                case check_previous_import(RscUri, Context) of
                    {ok, ZotonicId} ->
                        [{content_group_id, ZotonicId}];
                    none ->
                        Ps = [
                            {category, content_group},
                            {is_published, false},
                            {visible_for, 1},
                            {title, iolist_to_binary(["Content group stub: ",AnyId])},
                            {anymeta_id, AnyId},
                            {anymeta_host, Opt#opt.host}
                        ],
                        {ok, RscId} = m_rsc:insert(Ps, Context),
                        register_import_content_group(Opt#opt.host, RscId, z_convert:to_integer(AnyId), RscUri, Context),
                        progress(io_lib:format("    Content group stub for ~p (zotonic id ~w)", [AnyId, RscId]), Context),
                        [{content_group_id, RscId}]
                end
        end.

    fetch_address(Thing) ->
        case proplists:get_value(<<"address">>, Thing) of
            {struct, As} when is_list(As) ->
                % TODO, handle subsection 'work' etc.
                lists:foldl(fun adr_part/2, [], As);
            _ ->
                []
        end.

        adr_part({<<"email">>, V}, Acc) -> [{email, V}|Acc];
        adr_part({<<"phone">>, V}, Acc) -> [{phone, V}|Acc];
        adr_part({<<"mobile">>, V}, Acc) -> [{phone_alt, V}|Acc];
        adr_part({<<"fax">>, V}, Acc) -> [{fax, V}|Acc];
        adr_part({<<"line1">>, V}, Acc) -> [{address_street_1, V}|Acc];
        adr_part({<<"line2">>, V}, Acc) -> [{address_street_2, V}|Acc];
        adr_part({<<"postcode">>, V}, Acc) -> [{address_postcode, V}|Acc];
        adr_part({<<"city">>, V}, Acc) -> [{address_city, V}|Acc];
        adr_part({<<"state">>, V}, Acc) -> [{address_state, V}|Acc];
        adr_part({<<"country">>, V}, Acc) -> [{address_country, V}|Acc];
        adr_part({<<"website">>, V}, Acc) -> [{website, V}|Acc];
        adr_part(_, Acc) -> Acc.

    maybe_set_redirect_uri(Thing, Props) ->
        {struct, Text} = proplists:get_value(<<"text">>, Thing),
        case proplists:get_value(<<"redirect_uri">>, Text) of
            null -> Props;
            <<>> -> Props;
            undefined -> Props;
            RedirectUri ->
                [
                    {website, RedirectUri},
                    {is_website_redirect, true}
                    | proplists:delete(website, Props)
                ]
        end.


    fetch_name(Thing) ->
        case proplists:get_value(<<"name">>, Thing) of
            {struct, As} when is_list(As) ->
                lists:foldl(fun nam_part/2, [], As);
            _ ->
                []
        end.

        nam_part({<<"last">>, V}, Acc) -> [{name_surname, V}|Acc];
        nam_part({<<"middle">>, V}, Acc) -> [{name_middle, V}|Acc];
        nam_part({<<"voorvoegsel">>, V}, Acc) -> [{name_surname_prefix, V}|Acc];
        nam_part({<<"first">>, V}, Acc) -> [{name_first, V}|Acc];
        nam_part({<<"suffix">>, V}, Acc) -> [{name_suffix, V}|Acc];
        nam_part({<<"prefix">>, V}, Acc) -> [{name_prefix, V}|Acc];
        nam_part({<<"gender">>, Gender}, Acc) -> [{gender, z_string:to_lower(Gender)}|Acc];
        nam_part({<<"birth_city">>, V}, Acc) -> [{birth_city, V}|Acc];
        nam_part({<<"birth_country">>, V}, Acc) -> [{birth_country, V}|Acc];
        nam_part({<<"decease_city">>, V}, Acc) -> [{decease_city, V}|Acc];
        nam_part({<<"decease_country">>, V}, Acc) -> [{decease_country, V}|Acc];
        nam_part(_, Acc) -> Acc.


maybe_stub_origin(Opt, Thing, Context) ->
    ResourceUri = resource_uri(Thing),
    case origin_resource_uri(Thing) of
        ResourceUri ->
            ok;
        OriginUri ->
            % This resource has a local authoritative version and
            % a non-local authoritative origin.
            % Ensure that we have a stub for the origin and insert
            % entries with both the origin and resource uri.
            HostOriginal = Opt#opt.host_original,
            AnymetaId = z_convert:to_integer(proplists:get_value(<<"thg_id">>, Thing)),
            OriginHost = uri_host(OriginUri),
            PrevOriginId = check_previous_import(OriginUri, Context),
            PrevRscId = check_previous_import(ResourceUri, Context),
            case {PrevOriginId, PrevRscId} of
                {none, none} ->
                    RscIdNew = ensure_rsc_uri_id(OriginHost, OriginUri, AnymetaId, Context),
                    register_import(HostOriginal, RscIdNew, AnymetaId, ResourceUri, Context);
                {none, {ok, RscId}} ->
                    register_import(OriginHost, RscId, AnymetaId, OriginUri, Context);
                {{ok, OriginId}, none} ->
                    register_import(HostOriginal, OriginId, AnymetaId, ResourceUri, Context);
                {{ok, RscId}, {ok, RscId}} ->
                    ok;
                {{ok, OriginId}, {ok, RscId}} ->
                    % Delete the origin id, move all edges to the rsc id
                    Incoming = z_db:q("select id, subject_id, predicate_id 
                                       from edge
                                       where object_id = $1",
                                       [RscId],
                                       Context),
                    lists:foreach(fun({EdgeId, SubjectId, PredId}) ->
                                      case m_edge:get_id(SubjectId, PredId, OriginId, Context) of
                                          undefined ->
                                            z_db:q("update edge set object_id = $1 where id = $2",
                                                   [OriginId, EdgeId],
                                                   Context),
                                            z_depcache:flush(SubjectId, Context),
                                            z_edge_log_server:check(Context);
                                          _ ->
                                            ok
                                      end
                                  end,
                                  Incoming),
                    Outgoing = z_db:q("select id, object_id, predicate_id 
                                       from edge
                                       where subject_id = $1",
                                       [RscId],
                                       Context),
                    lists:foreach(fun({EdgeId, ObjectId, PredId}) ->
                                      case m_edge:get_id(OriginId, PredId, ObjectId, Context) of
                                          undefined ->
                                            z_db:q("update edge set subject_id = $1 where id = $2",
                                                   [OriginId, EdgeId],
                                                   Context),
                                            z_edge_log_server:check(Context);
                                          _ ->
                                            ok
                                      end
                                  end,
                                  Outgoing),
                    m_rsc:delete(RscId, Context),
                    z_depcache:flush(OriginId, Context),
                    register_import(HostOriginal, OriginId, AnymetaId, ResourceUri, Context)
            end
    end.

fetch_authoritative(Thing) ->
    case is_authoritative(Thing) of
        true ->
            [
                {is_authoritative, true},
                {uri, undefined}
            ];
        false ->
            [
                {is_authoritative, false},
                {uri, resource_uri(Thing)}
            ]
    end.

is_authoritative(Thing) ->
    case z_convert:to_bool(proplists:get_value(<<"authoritative">>, Thing, true)) of
        false ->
            false;
        true ->
            case proplists:get_value(<<"origin">>, Thing) of
                undefined -> true;
                null -> true;
                <<>> -> true;
                Origin when is_binary(Origin) -> false
            end
    end.

uri_host(Uri) ->
    {_Protocol, Host, _Path, _, _} = mochiweb_util:urlsplit(z_convert:to_list(Uri)),
    % Hack for a specific conversion with dirty data
    case z_convert:to_binary(Host) of
        <<"www.jewishmonument.", _/binary>> -> <<"www.joodsmonument.nl">>;
        <<"jewishmonument.", _/binary>> -> <<"www.joodsmonument.nl">>;
        <<"www.joodsmonument.", _/binary>> -> <<"www.joodsmonument.nl">>;
        <<"joodsmonument.", _/binary>> -> <<"www.joodsmonument.nl">>;
        HostBin -> HostBin
    end.

resource_uri(Thing) ->
    fix_uri(proplists:get_value(<<"resource_uri">>, Thing)).

origin_resource_uri(Thing) ->
    case fix_uri(proplists:get_value(<<"origin">>, Thing)) of
        undefined -> resource_uri(Thing);
        null -> resource_uri(Thing);
        <<>> -> resource_uri(Thing);
        Origin when is_binary(Origin) -> Origin
    end.

fix_uri(<<"http://www.jewishmonument.nl/", Path/binary>>) -> <<"http://www.joodsmonument.nl/", Path/binary>>;
fix_uri(<<"http://jewishmonument.nl/", Path/binary>>) -> <<"http://www.joodsmonument.nl/", Path/binary>>;
fix_uri(<<"http://joodsmonument.nl/", Path/binary>>) -> <<"http://www.joodsmonument.nl/", Path/binary>>;
fix_uri(Uri) -> Uri.


map_texts(Lang, Ts) ->
    Ts1 = lists:foldl(
            fun({F,T}, Acc) ->
                case map_text_field(F,T) of
                    skip -> Acc;
                    Prop when is_tuple(Prop) -> [ Prop | Acc ];
                    Prop when is_list(Prop) -> Prop ++ Acc
                end
            end,
            [],
            Ts),
    {z_convert:to_atom(Lang), Ts1}.
    

    %% TODO: save these fields somewhere, as they might have additional information
    %%       in the stripped markup.
    map_text_field(<<"title">>, T) -> {title, z_string:trim(z_html:strip(T))};
    map_text_field(<<"title_short">>, T) -> {short_title, z_string:trim(z_html:strip(T))};
    map_text_field(<<"chapeau">>, T) -> {chapeau, z_string:trim(z_html:strip(T))};
    map_text_field(<<"subtitle">>, T) -> {subtitle, z_string:trim(z_html:strip(T))};
    map_text_field(<<"intro">>, T) -> [
                {summary, z_string:trim(z_html:strip(T))},
                {summary_html, z_string:trim(T)}
            ];

    % TODO: map the wiki refs to Zotonic html
    map_text_field(<<"body_wikified">>, T) ->
        {body, T};

    map_text_field(<<"redirect_uri">>, T) ->
        {website, T};

    % TODO: translate to separate sections with headline/body
    map_text_field(<<"label">>, {struct, []}) -> 
        skip;
    map_text_field(<<"label">>, {struct, Ts}) -> 
        Sections = lists:foldr(fun({Name, {struct, T}}, Acc) ->
                                    Name1 = z_string:to_lower(Name),
                                    Text = get_non_empty_value(<<"text">>, T),
                                    SubHead = get_non_empty_value(<<"subhead">>, T),
                                    case {Text, SubHead} of
                                        {undefined, undefined} ->
                                            Acc;
                                        {_, undefined} ->
                                            [ {{block, 0-length(Acc), Name1, text}, Text} | Acc];
                                        {undefined, _} ->
                                            [ {{block, 0-length(Acc), Name1, header}, SubHead} | Acc];
                                        {_, _} ->
                                            Acc1 = [ {{block, 0-length(Acc), Name1, text}, Text} | Acc],
                                            [ {{block, 0-length(Acc1), <<Name1/binary, "_header">>, header}, SubHead} | Acc1]
                                    end
                               end,
                               [],
                               Ts),
        Sections;
        %{anymeta_label, Ts};

    map_text_field(_Skipped, _) ->
        skip.

get_non_empty_value(Key, Props) ->
    case proplists:get_value(Key, Props) of
        undefined -> undefined;
        null -> undefined;
        <<>> -> undefined;
        {struct, []} -> undefined;
        [] -> undefined;
        Text -> Text
    end.


map_sections(Fs) ->
    case lists:partition(fun({{block, _, _, _}, _}) -> true; (_) -> false end, Fs) of
        {[], _} ->
            Fs;
        {Sections, Fs1} ->
            % Combine the sections in a hierarchical structure
            [{blocks, [
                    case Type of
                        header ->
                            [ {type, <<"header">>}, {text, Texts} ];
                        text ->
                            [ {type, <<"text">>}, {name, z_convert:to_binary(z_string:to_lower(Name))}, {body, Texts} ]
                    end
                || {{block, _Nr, Name, Type}, Texts} <- lists:sort(Sections)
            ]
          } | Fs1]
    end.
    

group_by_lang(Ts) ->
    group_by_lang(Ts, dict:new()).

group_by_lang([], D) ->
    [ {Prop, {trans, Ts}} || {Prop,Ts} <- dict:to_list(D) ];
group_by_lang([{Iso,Texts}|Ts], D) ->
    D1 = lists:foldl(fun({L,null}, Acc) -> dict:append(L, {Iso, <<>>}, Acc);
                        ({L,undefined}, Acc) -> dict:append(L, {Iso, <<>>}, Acc);
                        ({L,T}, Acc) -> dict:append(L, {Iso, T}, Acc)
                     end,
                     D,
                     Texts),
    group_by_lang(Ts, D1).



map_fields(Thing) ->
    map_fields(Thing, Thing, []).

map_fields([], _Orig, Acc) ->
    Acc;
map_fields([{<<"pubstate">>, <<"1">>}|T], _Orig, Acc) ->
    map_fields(T, _Orig, [{is_published, true}|Acc]);
map_fields([{<<"pubstate">>, _}|T], _Orig, Acc) ->
    map_fields(T, _Orig, [{is_published, false}|Acc]);
map_fields([{<<"pub_date_start">>, Date}|T], _Orig, Acc) ->
    map_fields(T, _Orig, [{publication_start, convert_datetime(Date)}|Acc]);
map_fields([{<<"pub_date_end">>, Date}|T], _Orig, Acc) ->
    map_fields(T, _Orig, [{publication_end, convert_datetime(Date)}|Acc]);
map_fields([{<<"org_pubdate">>, Date}|T], _Orig, Acc) ->
    map_fields(T, _Orig, [{org_pubdate, convert_datetime(Date)}|Acc]);
map_fields([{<<"create_date">>, Date}|T], _Orig, Acc) ->
    map_fields(T, _Orig, [{created, convert_datetime(Date)}|Acc]);
map_fields([{<<"modify_date">>, Date}|T], _Orig, Acc) ->
    map_fields(T, _Orig, [{modified, convert_datetime(Date)}|Acc]);
map_fields([{<<"coverage">>, {struct, Cs}}|T], _Orig, Acc) ->
    % Time & location
    Acc1 = lists:foldl(
        fun({<<"date_start">>, D}, A) -> [ {date_start, convert_datetime(D)} | A ];
           ({<<"date_end">>, D}, A) -> [ {date_end, convert_datetime(D)} | A ];
           ({<<"org_pubdate">>, D}, A) -> [ {org_pubdate, convert_datetime(D)} | A ];
           ({<<"geometry_long">>, D}, A) -> [ {location_lng, D} | A ];
           ({<<"geometry_lat">>, D}, A) -> [ {location_lat, D} | A ];
           (_, A) -> A
        end,
        Acc,
        Cs),
    map_fields(T, _Orig, Acc1);
map_fields([{<<"symbolic_name">>, <<"">>}|T], _Orig, Acc) ->
    map_fields(T, _Orig, [{name, undefined}|Acc]);
map_fields([{<<"symbolic_name">>, <<"NULL">>}|T], _Orig, Acc) ->
    map_fields(T, _Orig, [{name, undefined}|Acc]);
map_fields([{<<"symbolic_name">>, Name}|T], Orig, Acc) ->
    %% skip symbolic names of attachment
    case proplists:get_value(<<"kind">>, Orig) of
        <<"ATTACHMENT">> ->
            map_fields(T, Orig, Acc);
        _ ->
            map_fields(T, Orig, [{name, z_string:to_lower(Name)}|Acc])
    end;
map_fields([{<<"axo">>, Axo}|T], _Orig, Acc) ->
    case z_convert:to_list(z_string:to_lower(Axo)) of
        "public" -> map_fields(T, _Orig, [{visible_for, 0}|Acc]);
        "system" -> map_fields(T, _Orig, [{visible_for, 0}|Acc]);
        "metadata" -> map_fields(T, _Orig, [{visible_for, 0}|Acc]);
        "persons" -> map_fields(T, _Orig, [{visible_for, 0}|Acc]);
        _ -> map_fields(T, _Orig, [{visible_for, 1}|Acc])
    end;
map_fields([{<<"rights">>, <<"">>}|T], _Orig, Acc) ->
    map_fields(T, _Orig, Acc);
map_fields([{<<"rights">>, <<"CR">>}|T], _Orig, Acc) ->
    map_fields(T, _Orig, Acc);
map_fields([{<<"rights">>, Rights}|T], _Orig, Acc) ->
    map_fields(T, _Orig, [{cc_rights, Rights}|Acc]);
map_fields([_|T], _Orig, Acc) ->
    map_fields(T, _Orig, Acc).


convert_datetime(undefined) ->
    undefined;
convert_datetime(null) ->
    undefined;
convert_datetime(<<>>) ->
    undefined;
convert_datetime(DateTime) ->
    [YMD,HIS] = string:tokens(binary_to_list(DateTime), " "),
    [Y,M,D,H,I,S] = lists:map(fun list_to_integer/1, string:tokens(YMD, "-") ++ string:tokens(HIS, ":")),
    case {{Y,M,D}, {H,I,S}} of
        {{2037,12,31},{0,0,0}} -> ?ST_JUTTEMIS;
        DT -> DT
    end.

convert_category(Thing, Context) ->
    Kind = proplists:get_value(<<"kind">>, Thing),
    {struct, Types} = proplists:get_value(<<"type">>, Thing),
    TypesSymbolic = proplists:get_value(<<"symbolic">>, Types),
    case z_notifier:first({import_anymeta_kind_type, Kind, TypesSymbolic}, Context) of
        undefined -> {category, map_kind_type(Kind, TypesSymbolic, Context)};
        {ok, Category} -> {category, Category} 
    end.
    
fix_media_category({category, media} = C, Thing) ->
    case proplists:get_value(<<"file">>, Thing) of
        undefined ->
            C;
        {struct, File} -> 
            case proplists:get_value(<<"is_picture">>, File) of
                <<"1">> -> {category, image};
                _ ->
                    case proplists:get_value(<<"mime">>, File) of
                        <<"application/x-youtube">> ->
                            {category, video};
                        _ ->
                            {category, document}
                    end
            end
    end;
fix_media_category(Cat, _Thing) ->
    Cat.

fix_rsc_name(Props) ->
    case proplists:get_value(category, Props) of
        anymeta_type ->
            Name = z_convert:to_binary(proplists:get_value(name, Props, "")),
            [ {name, <<"anytype_", Name/binary>>} | proplists:delete(name, Props) ];
        _ ->
            Props
    end.


fix_pubstart(Props) ->
    case proplists:get_value(publication_start, Props) of
        undefined -> fix_pubstart1(Props);
        {{1970, _, _}, _} -> fix_pubstart1(Props);
        _ -> Props
    end.
    
    fix_pubstart1(Props) ->
        case proplists:get_value(created, Props) of
            undefined -> 
                Props;
            Created ->
                [{publication_start, Created} | proplists:delete(publication_start, Props) ]
        end.

%% @TODO: make this a check if the summary contains HTML (other than <p>)
%%        If so, then move the body to a block and the summary_html to the body.
fix_html_summary(Props) ->
    case is_non_p_html(proplists:get_value(summary_html, Props)) of
        true ->
            case is_empty(proplists:get_value(body, Props)) of
                true ->
                    [ {summary, <<>>}, {body, proplists:get_value(summary_html, Props)}
                    | proplists:delete(body, proplists:delete(summary, Props)) ];
                false ->
                    % Move the body to a block
                    Blocks = [ 
                            [
                                {name, <<"body">>}, 
                                {type, <<"text">>}, 
                                {body, proplists:get_value(body, Props)}
                            ]
                            | proplists:get_value(blocks, Props, [])
                         ],
                    [ 
                        {summary, <<>>},
                        {body, proplists:get_value(summary_html, Props)},
                        {blocks, Blocks}
                    | proplists:delete(blocks, 
                        proplists:delete(body, 
                          proplists:delete(summary, Props)))
                    ]
            end;
        false ->
            Props
    end.

    is_empty(undefined) -> true;
    is_empty({trans, Trs}) -> not lists:any(fun({_,T}) -> not z_utils:is_empty(T) end, Trs);
    is_empty(A) -> z_utils:is_empty(A).

    is_non_p_html(undefined) -> false;
    is_non_p_html(<<>>) -> false;
    is_non_p_html([]) -> false;
    is_non_p_html({trans, Tr}) -> lists:any(fun is_non_p_html/1, Tr);
    is_non_p_html({_, V}) -> is_non_p_html(V);
    is_non_p_html(V) when is_list(V); is_binary(V) ->
        % <<"<([ac-oq-z]|b[^r])">>
        case re:run(V, <<"<[^/p]">>) of
            nomatch -> false;
            {match, _} -> true
        end;
    is_non_p_html(_) -> false.

map_kind_type(<<"ARTEFACT">>, _, _Context) -> artifact;
map_kind_type(<<"ARTICLE">>, _, _Context) -> article;
map_kind_type(<<"ATTACHMENT">>, _, _Context) -> media;
map_kind_type(<<"INSTITUTION">>, _, _Context) -> organization;
map_kind_type(<<"KEYWORD">>, _, _Context) -> keyword;
map_kind_type(<<"LANGUAGE">>, _, _Context) -> language;
map_kind_type(<<"LISTEDIT">>, _, _Context) -> 'query';
map_kind_type(<<"LISTPUBLISH">>, _, _Context) -> 'query';
map_kind_type(<<"LOCATION">>, _, _Context) -> location;
map_kind_type(<<"NOTE">>, _, _Context) -> comment;
map_kind_type(<<"PERSON">>, _, _Context) -> person;
map_kind_type(<<"ROLE">>, _, _Context) -> predicate;
map_kind_type(<<"SET">>, _, _Context) -> collection;
map_kind_type(<<"TAG">>, _, Context) -> 
    case m_category:name_to_id(tag, Context) of
        {ok, _} -> tag;
        {error, _} -> keyword
    end;
map_kind_type(<<"THEME">>, _, _Context) -> content_group;
map_kind_type(<<"TOPIC">>, _, _Context) -> anymeta_topic;
map_kind_type(<<"TYPE">>, _, _Context) -> anymeta_type;
map_kind_type(_, _, _Context) -> other.

map_predicate(<<"SETMEMBER">>) -> haspart;
map_predicate(<<"FIGURE">>) -> depiction;
map_predicate(<<"DOCUMENT">>) -> hasdocument;
map_predicate(<<"ICON">>) -> hasicon;
map_predicate(<<"RELATED">>) -> relation;
map_predicate(<<"ERR404">>) -> relation;
map_predicate(<<"HOME_SET">>) -> hascollection;
map_predicate(<<>>) -> relation;
map_predicate(P) -> z_convert:to_atom(z_string:to_lower(P)).

map_language("jp") -> "ja";
map_language(<<"jp">>) -> <<"ja">>;
map_language(Code) -> Code.


write_rsc(_Host, HostOriginal, AnymetaId, Fields, Stats, Context) ->
    RscUri = proplists:get_value(anymeta_rsc_uri, Fields),
    ensure_category(proplists:get_value(category, Fields), Context),
    case check_previous_import(RscUri, Context) of
        {ok, RscId} ->
            Fields1 = case m_rsc:get(proplists:get_value(name, Fields), Context) of
                          undefined -> Fields;
                          _FoundRsc -> proplists:delete(name, Fields)
                       end,
            progress(io_lib:format("~w: updating (zotonic id: ~w)", [AnymetaId, RscId]), Context),
            {ok, RscId} = rsc_update(RscId, Fields1, Context),
            register_import_update(HostOriginal, RscId, AnymetaId, RscUri, Context),
            {ok, RscId, Stats};
        none -> 
            Name = proplists:get_value(name, Fields),
            {ok, RscId} = case check_existing_rsc(Name, Context) of
                numeric ->
                    Fields1 = proplists:delete(name, Fields),
                    progress(io_lib:format("~w: Inserted", [AnymetaId]), Context),
                    rsc_insert(Fields1, Context);
                clash ->
                    Fields1 = proplists:delete(name, Fields),
                    NewName = integer_to_list(AnymetaId)++"_"++z_convert:to_list(Name),
                    progress(io_lib:format("~w: NAME CLASH, renamed ~p to ~p", [AnymetaId, Name, NewName]), Context),
                    case m_rsc:rid(NewName, Context) of
                        undefined ->
                            Fields2 = [{name,NewName}|Fields1],
                            rsc_insert(Fields2, Context);
                        ExistingId ->
                            {ok, ExistingId}
                    end;
                none ->
                    progress(io_lib:format("~w: Inserted", [AnymetaId]), Context),
                    rsc_insert(Fields, Context)
            end,
            register_import(HostOriginal, RscId, AnymetaId, RscUri, Context),
            {ok, RscId, Stats}
    end.

ensure_category(Category, Context) ->
    case m_category:name_to_id(Category, Context) of
        {ok, _Id} ->
            ok;
        {error, _} ->
            progress(io_lib:format("Insert category ~w", [Category]), Context),
            Name = z_string:to_name(z_convert:to_binary(Category)),
            maybe_move_named_rsc(Name, category, Context),
            m_rsc:insert([
                    {name, Name},
                    {category, category},
                    {title, z_convert:to_binary(Category)}
                ], Context)
    end.

ensure_predicate(Predicate, Context) ->
    case m_predicate:name_to_id(Predicate, Context) of
        {ok, _Id} ->
            ok;
        {error, _} ->
            Name = z_string:to_name(z_convert:to_binary(Predicate)),
            maybe_move_named_rsc(Name, predicate, Context),
            m_rsc:insert([
                    {name, Name},
                    {category, predicate},
                    {title, z_convert:to_binary(Predicate)}
                ], Context),
            z_depcache:flush(Context)
    end.

maybe_move_named_rsc(Name, Cat, Context) ->
    case m_rsc:rid(Name, Context) of
        undefined ->
            ok;
        Id ->
            case m_rsc:is_a(Id, meta, Context) of
                true ->
                    lager:error("Resource ~p with name '~s' is in the way of the ~p",
                                [Id, Name, Cat]);
                false ->
                    CatId = m_rsc:p_no_acl(Id, category_id, Context),
                    Name1 = iolist_to_binary([
                                m_rsc:p_no_acl(CatId, name, Context),
                                "_",
                                Name]),
                    lager:info("Renaming resource ~p to '~s' to make way for ~p",
                                [Id, Name1, Cat]),
                    m_rsc:update(Id, [{name, Name1}], [no_touch], Context)
            end
    end.

check_previous_import(RscUri, Context) ->
    case z_db:q1("select rsc_id from import_anymeta where rsc_uri = $1", [RscUri], Context) of
        undefined ->
            none;
        RscId -> 
            {ok, RscId}
    end.

register_import(Host, RscId, undefined, RscUri, Context) ->
    % Used for edge stubs, the Anymeta id is still unknown
    z_db:q("insert into import_anymeta (rsc_uri, rsc_id, anymeta_id, host, stub, imported)
            values ($1, $2, NULL, $3, true, now())",
           [RscUri, RscId, Host],
           Context);
register_import(Host, RscId, AnymetaId, RscUri, Context) ->
    % Used for the resource import
    z_db:q("delete from import_anymeta 
            where stub = true and rsc_uri = $1",
           [RscUri],
           Context),
    z_db:q("insert into import_anymeta (rsc_uri, rsc_id, anymeta_id, host, stub, imported)
            values ($1, $2, $3, $4, false, now())",
           [RscUri, RscId, AnymetaId, Host],
           Context).
           
register_import_content_group(Host, RscId, AnymetaId, RscUri, Context) ->
    z_db:q("insert into import_anymeta (rsc_uri, rsc_id, anymeta_id, host, stub, imported)
            values ($1, $2, $3, $4, true, now())",
           [RscUri, RscId, AnymetaId, Host],
           Context).

register_import_update(Host, _RscId, AnymetaId, RscUri, Context) ->
    z_db:q("update import_anymeta set stub = false, anymeta_id = $1
            where host = $2 and rsc_uri = $3",
           [AnymetaId, Host, RscUri],
           Context).

check_existing_rsc(undefined, _Context) ->
    none;
check_existing_rsc(Name, Context) ->
    case z_utils:only_digits(Name) of
        true  ->
            numeric;
        false -> 
            case m_rsc:name_to_id(Name, Context) of
                {ok, _RscId} -> clash;
                _ -> none
            end
    end.

ensure_rsc_uri(_HostOriginal, undefined, _Context) ->
    undefined;
ensure_rsc_uri(_HostOriginal, null, _Context) ->
    undefined;
ensure_rsc_uri(_HostOriginal, <<>>, _Context) ->
    undefined;
ensure_rsc_uri(HostOriginal, RscUri0, Context) ->
    RscUri = fix_uri(RscUri0),
    case check_previous_import(RscUri, Context) of
        {ok, RscId} ->
            RscId;
        none ->
            Ps = [
                {category, other},
                {is_published, false},
                {visible_for, 1},
                {title, iolist_to_binary(["Edge object stub: ",RscUri])},
                {anymeta_rsc_uri, RscUri},
                {anymeta_host, HostOriginal}
            ],
            {ok, RscId} = m_rsc:insert(Ps, Context),
            register_import(HostOriginal, RscId, undefined, RscUri, Context),
            progress(io_lib:format("    Edge stub for ~p (zotonic id ~w)", [RscUri, RscId]), Context),
            RscId
    end.

ensure_rsc_uri_id(HostOriginal, RscUri, AnymetaId, Context) ->
    case check_previous_import(RscUri, Context) of
        {ok, RscId} ->
            RscId;
        none ->
            Ps = [
                {category, other},
                {is_published, false},
                {visible_for, 1},
                {title, iolist_to_binary(["Stub: ",RscUri])},
                {anymeta_rsc_uri, RscUri},
                {anymeta_host, HostOriginal}
            ],
            {ok, RscId} = m_rsc:insert(Ps, Context),
            register_import(HostOriginal, RscId, AnymetaId, RscUri, Context),
            progress(io_lib:format("    Stub for ~p (zotonic id ~w)", [RscUri, RscId]), Context),
            RscId
    end.


rsc_update(RscId, Props, Context) ->
    Options = [{escape_texts, false}, is_import],
    try
        m_rsc_update:update(RscId, Props, Options, Context)
    catch
        error:{error, {badmatch, {unknown_predicate, Pred}}} ->
            lager:warning("[~p] Anymeta importer: Rescource ~p has invalid query ~p, unknown predicate ~p.",
                          [z_context:site(Context), RscId, proplists:lookup('query', Props), Pred]),
            Props1 = proplists:delete('query', Props),
            m_rsc_update:update(RscId, Props1, Options, Context);
        throw:{error, invalid_query} ->
            lager:warning("[~p] Anymeta importer: Rescource ~p has invalid query ~p, dropping query.",
                          [z_context:site(Context), RscId, proplists:lookup('query', Props)]),
            Props1 = proplists:delete('query', Props),
            m_rsc_update:update(RscId, Props1, Options, Context)
    end.

rsc_insert(Props, Context) ->
    Options = [{escape_texts, false}, is_import],
    try
        m_rsc_update:insert(Props, Options, Context)
    catch
        error:{error, {badmatch, {unknown_predicate, Pred}}} ->
            lager:warning("[~p] Anymeta importer: Rescource has invalid query ~p, unknown predicate ~p.",
                          [z_context:site(Context), proplists:lookup('query', Props), Pred]),
            Props1 = proplists:delete('query', Props),
            m_rsc_update:insert(Props1, Options, Context);
        throw:{error, invalid_query} ->
            lager:warning("[~p] Anymeta importer: Rescource has invalid query ~p, dropping query.",
                          [z_context:site(Context), proplists:lookup('query', Props)]),
            Props1 = proplists:delete('query', Props),
            m_rsc_update:insert(Props1, Options, Context)
    end.

% TODO: check if the file is changed, only import when it is changed.
write_file(AnymetaId, RscId, Filename, Data, Stats, Context) ->
    case is_file_changed(RscId, Data, Context) of
        true ->
            progress(io_lib:format("    Add file ~p (~w bytes)", [Filename,size(Data)]), Context),
            case m_media:replace_file(#upload{
                                        filename=Filename,
                                        data=Data,
                                        tmpfile=undefined
                                      }, 
                                      RscId,
                                      [],
                                      [is_import,no_touch],
                                      Context)
            of
                {ok, _Id} -> 
                    Stats;
                {error, Reason} ->
                    Stats#stats{error=[{AnymetaId, {replace_file, Reason}} | Stats#stats.error]}
            end;
        false ->
            progress(io_lib:format("    Add file ~p (~w bytes) [not changed]", [Filename,size(Data)]), Context),
            % file not changed, do not re-import
            Stats
    end.

    % Fetch the old file and compare the contents to the new file.
    % We don't have a hash available so we do it brute force.
    is_file_changed(RscId, Data, Context) ->
        case m_media:get_file_data(RscId, Context) of
            #upload{data=Data} -> false;
            _ -> true
        end.

fetch_creator_modifier(Opt, Thing, Context) ->
    [
        {creator_id, fetch_uri_id([<<"owner_id">>, <<"creator_id">>], Opt, Thing, Context)},
        {modifier_id, fetch_uri_id([<<"modifier_id">>], Opt, Thing, Context)}
    ].

fetch_uri_id([], _Opt, _Thing, _Context) ->
    undefined;
fetch_uri_id([Prop|Ps], Opt, Thing, Context) ->
    case proplists:get_value(Prop, Thing) of
        null -> fetch_uri_id(Ps, Opt, Thing, Context);
        undefined -> fetch_uri_id(Ps, Opt, Thing, Context);
        <<>> -> fetch_uri_id(Ps, Opt, Thing, Context);
        Url ->
            % Url is always the non-informational uri of the resource
            ensure_rsc_uri(Opt#opt.host_original, Url, Context)
    end.


import_edges(_Opt, _RscId, undefined, Stats, _Context) ->
    Stats;
import_edges(_Opt, _RscId, null, Stats, _Context) ->
    Stats;
import_edges(Opt, RscId, Edges, Stats, Context) when is_list(Edges) ->
    lists:foldl(fun(Edge, St) ->
                    import_edge(Opt, RscId, Edge, St, Context)
                end,
                Stats,
                Edges).

% TODO: do something with the date on the edge (if any)
%       especially for events w/ presented_at predicate
import_edge(Opt, SubjectId, {struct, Props}, Stats, Context) ->
    {struct, Edge} = proplists:get_value(<<"edge">>, Props),
    Predicate = map_predicate(proplists:get_value(<<"predicate">>, Edge)),
    case Predicate of
        menu_editorial -> 
            Stats;
        _ ->
            P1 = case z_notifier:first({import_anymeta_map_predicate, Predicate}, Context) of
                undefined -> Predicate;
                P -> P
            end,
            ensure_predicate(P1, Context),
            ObjectRscUri = proplists:get_value(<<"object_id">>, Edge),
            ObjectId = ensure_rsc_uri(Opt#opt.host_original, ObjectRscUri, Context),
            CreatorRscUri = proplists:get_value(<<"creator_id">>, Edge),
            CreatorId = ensure_rsc_uri(Opt#opt.host_original, CreatorRscUri, Context),
            Created = convert_datetime(proplists:get_value(<<"modify_date">>, Edge)),
            Seq = case proplists:get_value(<<"order">>, Edge) of
                <<"9999">> -> 1000000;
                OrderBin -> z_convert:to_integer(OrderBin)
            end,
            progress(io_lib:format("    Edge ~w -[~p]-> ~w", [SubjectId,P1,ObjectId]), Context),
            {ok, EdgeId} = m_edge:insert(
                                    SubjectId, P1, ObjectId, 
                                    [
                                        no_touch,
                                        {seq, Seq},
                                        {created, Created},
                                        {creator_id, CreatorId}
                                    ],
                                    Context),
            case edge_props(Edge) of
                [] ->
                    z_db:q("delete from import_anymeta_edge where id = $1",
                           [EdgeId],
                           Context);
                EPs ->
                    case z_db:q("update import_anymeta_edge
                                 set props = $1
                                 where id = $2",
                                [?DB_PROPS(EPs),EdgeId],
                                Context)
                    of
                        0 ->
                            z_db:q("insert into import_anymeta_edge (id,props) values ($1,$2)",
                                   [EdgeId, ?DB_PROPS(EPs)],
                                   Context);
                        1 ->
                            ok
                    end
            end,
            Stats
    end.

edge_props(Props) ->
    lists:flatten([
            edge_prop_date_start(Props),
            edge_prop_date_end(Props),
            edge_prop_caption(Props)
        ]).

edge_prop_date_start(Props) ->
    case proplists:get_value(<<"date_start">>, Props) of
        {struct, Ps} ->
            {<<"date">>, Date} = proplists:lookup(<<"date">>, Ps),
            [{date_start, convert_datetime(Date)}];
        Date when is_binary(Date) ->
            [{date_start, convert_datetime(Date)}];
        undefined ->
            []
    end.

edge_prop_date_end(Props) ->
    case proplists:get_value(<<"date_end">>, Props) of
        {struct, Ps} ->
            {<<"date">>, Date} = proplists:lookup(<<"date">>, Ps),
            [{date_end, convert_datetime(Date)}];
        Date when is_binary(Date) ->
            [{date_end, convert_datetime(Date)}];
        undefined ->
            []
    end.

edge_prop_caption(Props) ->
    case proplists:get_value(<<"lang">>, Props) of
        {struct, Ps} ->
            Captions = [
                begin
                    Intro = proplists:get_value(<<"intro">>, Ls),
                    {z_convert:to_atom(map_language(Lang)), Intro}
                end
                || {Lang, {struct, Ls}} <- Ps
            ],
            Captions1 = lists:filter(
                            fun
                                ({_, <<>>}) -> false;
                                ({_, undefined}) -> false;
                                ({_, null}) -> false;
                                (_) -> true
                            end,
                            Captions),
            case Captions1 of
                [] -> [];
                _ -> [{caption, {trans, Captions1}}]
            end;
        undefined ->
            []
    end.


import_keywords(_Opt, _RscId, undefined, Stats, _Context) ->
    Stats;
import_keywords(_Opt, _RscId, null, Stats, _Context) ->
    Stats;
import_keywords(Opt, RscId, KeywordIds, Stats, Context) when is_list(KeywordIds) ->
    lists:foldl(fun(KwId, St) ->
                    import_keyword(Opt, RscId, maybe_integer(KwId), St, Context)
                end,
                Stats,
                KeywordIds).
    
    import_keyword(Opt, SubjectId, KeywordId, Stats, Context) ->
        case find_any_id(KeywordId, Opt#opt.host_original, Context) of
            {ok, ZotonicId} ->
                progress(io_lib:format("    Edge ~w -[~p]-> ~w", [SubjectId,subject,ZotonicId]), Context),
                {ok, _} = m_edge:insert(SubjectId, subject, ZotonicId, [no_touch], Context),
                Stats;
            undefined ->
                progress(io_lib:format("    Keyword ~w (not yet imported, delayed)", [KeywordId]), Context),
                Stats#stats{delayed=[{keyword, SubjectId, KeywordId}|Stats#stats.delayed]}
        end.
    

maybe_integer(Id) ->
    case z_utils:only_digits(Id) of
        true -> z_convert:to_integer(Id);
        false -> Id
    end.

convert_query(Fields, Thing, Context) ->
    case proplists:get_value(<<"serialize">>, Thing) of
        {struct, Ser} ->
            AnymetaQuery = case proplists:get_value(<<"listpublish">>, Ser) of
                                undefined -> proplists:get_value(<<"listedit">>, Ser);
                                QQ -> QQ
                           end,
            case AnymetaQuery of
                {struct, Query} ->
                    Kinds = filter_on(proplists:get_value(<<"kinds">>, Query)),
                    Types = filter_on(proplists:get_value(<<"types">>, Query)),
                    Exclude = filter_on(proplists:get_value(<<"exclude">>, Query)),
                    HasObject = filter_rel(proplists:get_value(<<"haschild">>, Query)),
                    HasSubject = filter_rel(proplists:get_value(<<"hasparent">>, Query)),
                    Sort = filter_sort(proplists:get_value(<<"sort">>, Query)),
                    Limit = proplists:get_value(<<"limit">>, Query),

                    % Time:
                    % {<<"time">>,{struct,[{<<"req">>,{struct,[{<<"match_date">>,<<>>},{<<"from">>,{struct,[{<<"dab">>,{struct,[{<<"days">>,<<>>},{<<"afterbefore">>,<<>>}]}},{<<"np">>,{struct,[{<<"nextprev">>,<<"next">>},{<<"daymonth">>,<<"monday">>}]}},{<<"aday">>,{struct,[{<<"day">>,<<>>},{<<"nextcurprev">>,<<"next">>},{<<"wmy">>,<<"week">>}]}},{<<"dmy">>,{struct,[{<<"day">>,<<>>},{<<"month">>,<<>>},{<<"year">>,<<>>}]}}]}},{<<"to">>,{struct,[{<<"duration">>,{struct,[{<<"hours">>,<<>>},{<<"days">>,<<>>},{<<"months">>,<<>>},{<<"years">>,<<>>}]}},{<<"until">>,{struct,[{<<"dab">>,{struct,[{<<"days">>,<<>>},{<<"afterbefore">>,<<>>}]}},{<<"np">>,{struct,[{<<"nextprev">>,<<"next">>},{<<"daymonth">>,<<"monday">>}]}},{<<"aday">>,{struct,[{<<"day">>,<<>>},{<<"nextcurprev">>,<<"next">>},{<<"wmy">>,<<"week">>}]}},{<<"dmy">>,{struct,[{<<"day">>,<<>>},{<<"month">>,<<>>},{<<"year">>,<<>>}]}}]}}]}}]}}]}}

                    % Place:
                    % {<<"place">>,{struct,[{<<"req">>,{struct,[{<<"location">>,{struct,[{<<"lat">>,<<>>},{<<"long">>,<<>>}]}},{<<"maxdistance">>,<<>>},{<<"maxpoints">>,<<>>}]}}]}}

                    % Map to: http://zotonic.com/documentation/761/the-query-search-model

                    ?DEBUG({Kinds, Types, Exclude, HasObject, HasSubject, Sort, Limit}),
                    Kind = case Kinds of 
                               [K] -> K;
                               _ -> undefined
                           end,
                    Cats = lists:foldl(fun(T,Acc) ->
                                            case z_notifier:first({import_anymeta_kind_type, Kind, [T]}, Context) of
                                                undefined -> [ z_string:to_lower(T) | Acc ];
                                                {ok, Category} -> [Category|Acc]
                                            end
                                        end,
                                        [],
                                        Types),
                    Cats1 = case Cats of 
                                [] -> [ map_kind_type(K, [], Context) || K <- Kinds ];
                                _ -> Cats
                           end,
                    Fs = [
                        [ {"cat", z_convert:to_list(C)} || C <- Cats1 ],
                        [ {"cat_exclude", z_convert:to_list(C)} || C <- Exclude ],
                        [ {"hassubject", iolist_to_binary(R)} || R <- HasSubject ],
                        [ {"hasobject", iolist_to_binary(R)} || R <- HasObject ],
                        case Sort of undefined -> []; _ -> [{"sort", z_convert:to_binary(Sort)}] end
                    ],
                    [ 
                        {'query', z_string:trim(iolist_to_binary([[K,$=,V,10] || {K,V} <- lists:flatten(Fs)]))},
                        {'query_limit', Limit}
                        | Fields
                    ];
                _ ->
                    Fields
            end;
        _ -> 
            Fields
    end.
    
    filter_on({struct, Ps}) -> [ P || {P,<<"on">>} <- Ps ];
    filter_on(_) -> [].

    filter_rel({struct, Rs}) ->
        % Want = proplists:get_value(<<"want">>, Rs, <<>>),
        OneOf = z_convert:to_list(z_string:trim(proplists:get_value(<<"oneof">>, Rs, <<>>))),
        All = z_convert:to_list(z_string:trim(proplists:get_value(<<"all">>, Rs, <<>>))),
        TsOneOf = [ string:tokens(z_string:trim(A), ":") || A <- string:tokens(OneOf, ",") ],
        TsAll = [ string:tokens(z_string:trim(A), ":") || A <- string:tokens(All, ",") ],
        [
         case Rel of
                [A] -> z_string:to_lower(A);
                [A,Pred] -> [$[, z_string:to_lower(A), $,, z_convert:to_list(map_predicate(Pred)), $]];
                _ -> []
            end
            || Rel <- TsOneOf ++ TsAll
        ];
    filter_rel(_) -> 
        [].

    filter_sort({struct, [{struct, First}|_]}) ->
        % {<<"sort">>,[{struct,[{<<"by">>,<<"create_date">>},{<<"order">>,<<"-">>}]},{struct,[{<<"by">>,<<>>},{<<"order">>,<<"-">>}]}]}
        By = proplists:get_value(<<"by">>, First),
        Order = proplists:get_value(<<"order">>, First, <<"+">>),
        A = case By of
                <<"title">> -> [Order, "rsc.pivot_title"];
                <<"distance">> -> undefined;
                <<"pub_date_start">> -> [Order,"rsc.publication_start"];
                <<"pub_date_end">> -> [Order,"rsc.publication_end"];
                <<"modify_date">> -> [Order,"rsc.modified"];
                <<"create_date">> -> [Order,"rsc.created"];
                <<"date_start">> -> [Order,"rsc.pivot_date_start"];
                <<"date_end">> -> [Order,"rsc.pivot_date_end"];
                <<"org_pubdate">> -> undefined;
                <<"textscore">> -> undefined;
                <<"activity">> -> undefined;
                <<"views">> -> undefined;
                <<"random">> -> undefined;
                _ -> undefined
            end,
        case A of undefined -> undefined; _ -> iolist_to_binary(A) end;
    filter_sort(_) ->
        undefined.

%% @doc Show progress in the progress area of the import window.
progress(Message, Context) ->
    ?zInfo(Message, Context),
    mod_signal:emit_script({import_anymeta_progress, []}, 
                           z_render:wire({insert_top, [{target, "progress"}, {text, [Message,"<br/>"]}]}, Context)).


ensure_anymeta_type(Context) ->
    case m_category:name_to_id(anymeta_type, Context) of
        {ok, _} -> 
            skip;
        _ ->
            {ok, ParentId} = m_category:name_to_id(meta, Context),
            m_category:insert(ParentId, 
                              anymeta_type, 
                              [
                                {title, "anyMeta type"}, 
                                {summary, "Used for the anymeta data import."}
                              ],
                              Context)
    end.


