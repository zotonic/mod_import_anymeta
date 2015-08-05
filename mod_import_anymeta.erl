%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011-2013 Marc Worrell
%% @doc Import data from an Anymeta website

%% Copyright 2011-2013 Marc Worrell
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

-module(mod_import_anymeta).
-author("Marc Worrell <marc@worrell.nl>").

-mod_title("Import Anymeta Site").
-mod_description("Import data from an Anymeta web site.").
-mod_prio(300).

-export([
    init/1,

    observe_admin_menu/3,
    observe_dispatch/2,
    event/2,
    
    find_any_id/3,
    do_import/5,
    import_thing/5,
    test_host/4,
    get_thing/4
]).

-include_lib("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").
-include_lib("include/mod_import_anymeta.hrl").

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
            ok
    end,
    
    case z_db:table_exists(anymeta_redirects, Context) of
        true ->
            ok;
        false ->
            [] = z_db:q("
                create table anymeta_redirects (
                    redirect_uri character varying(255) not null,
                    rsc_id int not null,
                    rsc_uri character varying(255) not null,
                    host character varying(255)
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


%% @doc Map anyMeta URLs to Zotonic resources, uses a permanent redirect
observe_dispatch(#dispatch{path=Path}, Context) ->
    % URIs matched: 
    % (...)/(article|artefact|...)-<id>-<language>.html
    % (...)/(article|artefact|...)-<id>.html
    % (...)/id.php/(uuid|id|name)
    % index.php
    
    % TODO match anymeta 4.19 paths /id/lang/slug
    %      Handle multiple old domains to 1 zotonic site
    
    Parts = string:tokens(Path, "/"),
    case lists:reverse(Parts) of
        ["index.php"] ->
            ContextQs = z_context:ensure_qs(Context),
            redirect_rsc(m_rsc:rid(page_home, ContextQs), z_context:get_q("lang", ContextQs), ContextQs);
        [AnyId,"id.php"|_] ->
            redirect(AnyId, undefined, Context);
        [Rsc|_] ->
            case filename:extension(Rsc) of
                ".html" ->
                    case string:tokens(filename:rootname(Rsc), "-") of
                        [_Kind,AnyId,[_,_] = Lang] ->
                            redirect(AnyId, Lang, Context);
                        [_Kind,AnyId] ->
                            redirect(AnyId, undefined, Context);
                        _ ->
                            undefined
                    end;
                _ ->
                    undefined
            end;
        [] ->
            undefined
    end.

    redirect(AnyId, Lang, Context) ->
        case z_db:q1("select rsc_id from import_anymeta where anymeta_id = $1", [z_convert:to_integer(AnyId)], Context) of
            undefined -> 
                undefined;
            {ok, RscId} ->
                redirect_rsc(RscId, Lang, Context)
        end.

    redirect_rsc(undefined, _Lang, _Context) ->
        undefined;
    redirect_rsc(RscId, Lang, Context) ->
        Lang1 = map_language(Lang),
        Context1 = case z_trans:is_language(Lang1) of
                     true ->
                         % Add language
                         z_context:set_language(list_to_atom(Lang1), Context);
                     false ->
                         % Ignore language
                         Context
                   end,
        case m_rsc:p(RscId, page_url, Context1) of
            undefined ->
                undefined;
            URL ->
                {ok, #dispatch_match{
                    mod=controller_redirect,
                    mod_opts=[{url, URL}, {is_permanent, true}],
                    bindings=[]
                }}
        end.

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

            lager:info("Anymeta import started."),

            z_context:set_session(anymeta_host, Host, Context),
            
            case do_import(Host, From, To, Secret, Context) of
                ok ->
                    z_render:wire([{fade_in, [{target, "import-started"}]}, {hide, [{target, Form}]}], Context);
                {error, nxdomain} ->
                    z_render:wire([{fade_in, [{target, "import-nxdomain"}]}, {hide, [{target, "import-error"}]}], Context);
                {error, _Other} ->
                    z_render:wire([{fade_in, [{target, "import-error"}]}, {hide, [{target, "import-nxdomain"}]}], Context)
            end
    end.


find_any_id(AnyId, Host, Context) when is_binary(AnyId) ->
    find_any_id(z_convert:to_list(AnyId), Host, Context);
find_any_id(AnyId, Host, Context) when is_list(AnyId) ->
    case z_utils:only_digits(AnyId) of
        true ->
            % anyMeta id
            case z_db:q1("select rsc_id from import_anymeta where anymeta_id = $1 and host = $2", [z_convert:to_integer(AnyId), Host], Context) of
                undefined -> undefined;
                RscId -> {ok, RscId}
            end;
        false ->
            % rsc uri or name
            case m_rsc:name_to_id(AnyId, Context) of
                {ok, RscId} -> {ok, RscId};
                {error, _} ->
                    case z_db:q1("select rsc_id from import_anymeta where rsc_uri = $1 and host = $2", [AnyId, Host], Context) of
                        undefined -> undefined;
                        RscId -> {ok, RscId}
                    end
            end
    end;
find_any_id(AnyId, Host, Context) when is_integer(AnyId)->
    case z_db:q1("select rsc_id from import_anymeta where anymeta_id = $1 and host = $2", [z_convert:to_integer(AnyId), Host], Context) of
        undefined -> undefined;
        RscId -> {ok, RscId}
    end.


do_import(Host, undefined, To, Secret, Context) ->
    do_import(Host, 1, To, Secret, Context);
do_import(Host, From, To, Secret, Context) ->
    case test_host(From, Host, Secret, Context) of
        ok ->
            start_import(Host, From, To, Secret, Context);
        Err ->
            Err
    end.

    start_import(Host, From, To, Secret, Context) ->
        ContextPruned = z_context:prune_for_async(Context),
        spawn(fun() ->
                    import_loop(Host, From, To, Secret, #stats{}, ContextPruned)
              end),
        ok.
    
    test_host(From, Host, Secret, Context) ->
        Url = get_url(From, Host, Secret),
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


get_url(Id, Host, Secret) ->
    "http://"++Host++"/thing/"++integer_to_list(Id)++"/json?secret="++Secret.

get_thing(Id, Hostname, Secret, Context) ->
    Url = get_url(Id, Hostname, Secret),
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
import_loop(Host, _From, undefined, Secret, #stats{consequetive_notfound=NF, delayed=Delayed} = Stats, Context) when NF > 500 ->
    % Signal the end of the import to the UI
    progress("STOP - found more than 500 consequetive not founds.<br/>", Context),
    handle_delayed(Delayed, Host, Secret, Stats#stats{delayed=[]}, Context);
import_loop(Host, From, To, Secret, Stats, Context) when is_integer(To), From > To ->
    % Signal the end of the import to the UI
    progress(io_lib:format("STOP - At last import anymeta id (~w).<br/>", [To]), Context),
    handle_delayed(Stats#stats.delayed, Host, Secret, Stats#stats{delayed=[]}, Context);
import_loop(Host, From, To, Secret, Stats, Context) ->
    progress(io_lib:format("~w: fetching from ~p", [From, Host]), Context),
    case get_thing(From, Host, Secret, Context) of
        {ok, Thing} ->
            progress(io_lib:format("~w: importing", [From]), Context),
            Stats1 = import_thing(Host, From, Thing, Stats, Context),
            Stats2 = Stats1#stats{
                found=Stats#stats.found+1, 
                consequetive_notfound=0
            },
            import_loop(Host, From+1, To, Secret, Stats2, Context);
        {error, no_service} ->
            progress(io_lib:format("~w: got 503 - waiting 10 seconds before retry.", [From]), Context),
            % Anymeta servers give a 503 when they are overloaded.
            % Sleep for 10 seconds and then retry our request.
            timer:sleep(10000),
            import_loop(Host, From, To, Secret, Stats, Context);
        {error, timeout} ->
            progress(io_lib:format("~w: got timeout - waiting 5 seconds before retry.", [From]), Context),
            timer:sleep(5000),
            import_loop(Host, From, To, Secret, Stats, Context);
        {error, not_found} ->
            progress(io_lib:format("~w: not found, skipping to next", [From]), Context),
            Stats1 = Stats#stats{
                notfound=Stats#stats.notfound+1, 
                consequetive_notfound=Stats#stats.consequetive_notfound+1,
                error=[{From, notfound} | Stats#stats.error]
            },
            import_loop(Host, From+1, To, Secret, Stats1, Context);
        {error, Reason} ->
            progress(io_lib:format("~w: error, skipping to next (error: ~p)", [From, Reason]), Context),
            Stats1 = Stats#stats{
                error=[{From, Reason} | Stats#stats.error]
            },
            import_loop(Host, From+1, To, Secret, Stats1, Context)
    end.


    handle_delayed([], _Host, _Secret, #stats{delayed=[]} = Stats, _Context) ->
        {ok, Stats};
    handle_delayed([], Host, Secret, #stats{delayed=Delayed} = Stats, Context) ->
        handle_delayed(Delayed, Host, Secret, Stats#stats{delayed=[]}, Context);
    handle_delayed([{keyword, RscId, AnymetaId}|Ds], Host, Secret, Stats, Context) ->
        AnyId = z_convert:to_integer(AnymetaId),
        FoundId = case find_any_id(AnyId, Host, Context) of
                    undefined ->
                        {ok, Stats1} = import_loop(Host, AnyId, AnyId, Secret, Stats, Context),
                        find_any_id(AnyId, Host, Context);
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
        handle_delayed(Ds, Host, Secret, Stats1, Context).


import_thing(Host, AnymetaId, Thing, Stats, Context) ->
    % Check if the <<"lang">> section is available, if so then we had read acces, otherwise skip
    case skip(Thing) of
        false ->
            {struct, Texts} = proplists:get_value(<<"lang">>, Thing),
            RscUri = proplists:get_value(<<"resource_uri">>, Thing),
            {struct, Text} = proplists:get_value(<<"text">>, Thing),
            RedirectUri = proplists:get_value(<<"redirect_uri">>, Text),
            Authoritative = proplists:get_value(<<"authoritative">>, Thing),
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
                        {anymeta_rsc_uri, RscUri},
                        {anymeta_host, Host},
                        {language, Langs},
                        {is_authoritative, Authoritative},
                        {rights, Rights},
                        {findable, Findable},
                        {matchable, Matchable}
                        |OtherFields
                     ]
                     ++ fetch_content_group(Thing, Host, Context)
                     ++ fetch_media(Thing)
                     ++ fetch_address(Thing)
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
            FieldsFinal = z_notifier:foldl(import_anymeta_fields, Fields3, Context),
            
            case write_rsc(Host, AnymetaId, RedirectUri, FieldsFinal, Stats, Context) of
                {ok, RscId, Stats1} ->
                    % Import all edges
                    Stats2 = import_edges(Host, RscId, proplists:get_value(<<"edge">>, Thing), Stats1, Context),
                    Stats3 = import_keywords(Host, RscId, proplists:get_value(<<"keyword">>, Thing), Stats2, Context),
                    Stats4 = import_keywords(Host, RscId, proplists:get_value(<<"tag">>, Thing), Stats3, Context),

                    case proplists:get_value(<<"file">>, Thing) of
                        undefined ->
                            Stats4;
                        {struct, File} -> 
                            Filename = proplists:get_value(<<"original_file">>, File),
                            case proplists:get_value(<<"file_blob">>, File) of
                                undefined ->
                                    case proplists:get_value(<<"uri">>, File) of
                                        undefined ->
                                            Stats4;
                                        _ ->
                                            case proplists:get_value(<<"mime">>, File) of
                                                <<"application/x-youtube">> ->
                                                    Stats4;
                                                _ ->
                                                    Url = proplists:get_value(<<"uri">>, File),
                                                    m_media:replace_url(Url, RscId, [], Context),
                                                    Stats4
                                            end
                                    end;
                                {struct, Fileblob} -> 
                                    {struct, Fileblob} = proplists:get_value(<<"file_blob">>, File),
                                    case proplists:get_value(<<"encode">>, Fileblob) of
                                        <<"base64">> ->
                                            Data = base64:decode(proplists:get_value(<<"data">>, Fileblob)),
                                            write_file(AnymetaId, RscId, Filename, Data, Stats4, Context);
                                        Encoding ->
                                        Stats4#stats{error=[{AnymetaId, {unknown_file_encoding, Encoding}} | Stats4#stats.error]}
                                    end
                            end
                    end;
                {error, Stats1} ->
                    Stats1
            end;
        true ->
            % Access was denied, skip this thing
            Stats#stats{error=[{AnymetaId, skipped} | Stats#stats.error]}
    end.

    skip(Thing) ->
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

    fetch_content_group(Thing, Host, Context) ->
        case proplists:get_value(<<"theme">>, Thing) of
            none ->
                [];
            undefined ->
                [];
            [AnyId|_] ->
                % find the content group or create one
                CurrRscUri = binary_to_list(proplists:get_value(<<"resource_uri">>, Thing)),
                BaseUri = string:substr(CurrRscUri, 1, string:str(CurrRscUri, "/id/") + 3),
                RscUri = BaseUri ++ binary_to_list(AnyId),
                
                case check_previous_import(Host, RscUri, Context) of
                    {ok, ZotonicId} ->
                        [{content_group_id, ZotonicId}];
                    none ->
                        Ps = [
                            {category, content_group},
                            {is_published, false},
                            {visible_for, 1},
                            {title, iolist_to_binary(["Content group stub: ",AnyId])},
                            {anymeta_id, AnyId},
                            {anymeta_host, Host}
                        ],
                        {ok, RscId} = m_rsc:insert(Ps, Context),
                        register_import_content_group(Host, RscId, z_convert:to_integer(AnyId), RscUri, Context),
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
        nam_part(_, Acc) -> Acc.


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
                                    Acc1 = case proplists:get_value(<<"text">>, T) of
                                        <<>> -> Acc;
                                        Text -> [ {{block, 0-length(Acc), Name, text}, Text} | Acc]
                                    end,
                                    case proplists:get_value(<<"subhead">>, T) of
                                        <<>> -> Acc1;
                                        SubHead -> [ {{block, 0-length(Acc1), Name, header}, SubHead} | Acc1]
                                    end
                               end,
                               [],
                               Ts),
        Sections;
        %{anymeta_label, Ts};

    map_text_field(_Skipped, _) ->
        skip.

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
        undefined -> {category, map_kind_type(Kind, TypesSymbolic)};
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

map_kind_type(<<"ARTEFACT">>, _) -> artifact;
map_kind_type(<<"ARTICLE">>, _) -> article;
map_kind_type(<<"ATTACHMENT">>, _) -> media;
map_kind_type(<<"INSTITUTION">>, _) -> organization;
map_kind_type(<<"KEYWORD">>, _) -> keyword;
map_kind_type(<<"LANGUAGE">>, _) -> language;
map_kind_type(<<"LISTEDIT">>, _) -> 'query';
map_kind_type(<<"LISTPUBLISH">>, _) -> 'query';
map_kind_type(<<"LOCATION">>, _) -> location;
map_kind_type(<<"NOTE">>, _) -> text;
map_kind_type(<<"PERSON">>, _) -> person;
map_kind_type(<<"ROLE">>, _) -> predicate;
map_kind_type(<<"SET">>, _) -> collection;
map_kind_type(<<"TAG">>, _) -> keyword;
map_kind_type(<<"THEME">>, _) -> content_group;
map_kind_type(<<"TOPIC">>, _) -> anymeta_topic;
map_kind_type(<<"TYPE">>, _) -> anymeta_type;
map_kind_type(_, _) -> other.

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


write_rsc(Host, AnymetaId, RedirectUri, Fields, Stats, Context) ->
    RscUri = proplists:get_value(anymeta_rsc_uri, Fields),
    ensure_category(proplists:get_value(category, Fields), Context),
    case check_previous_import(Host, RscUri, Context) of
        {ok, RscId} ->
            case m_rsc:get(proplists:get_value(name, Fields), Context) of
              undefined ->
                Fields1 = Fields;
              _FoundRsc ->
                Fields1 = proplists:delete(name, Fields)
            end,
            progress(io_lib:format("~w: updating (zotonic id: ~w)", [AnymetaId, RscId]), Context),
            {ok, RscId} = m_rsc_update:update(RscId, Fields1, [{escape_texts, false}, is_import], Context),
            register_import_update(Host, RscId, AnymetaId, RscUri, Context),
            register_redirect(RedirectUri, RscUri, RscId, Host, Context),
            {ok, RscId, Stats};
        none -> 
            Name = proplists:get_value(name, Fields),
            case check_existing_rsc(Fields, Context) of
                {ok, RscId} ->
                    progress(io_lib:format("~w: exists, updating (name: ~p, zotonic id: ~w)", [AnymetaId, Name, RscId]), Context),
                    {ok, RscId} = m_rsc_update:update(RscId, Fields, [{escape_texts, false}, is_import], Context);
                numeric ->
                    Fields1 = proplists:delete(name, Fields),
                    progress(io_lib:format("~w: Inserted", [AnymetaId]), Context),
                    {ok, RscId} = m_rsc_update:insert(Fields1, [{escape_texts, false}, is_import], Context);
                clash ->
                    Name = proplists:get_value(name, Fields),
                    Fields1 = proplists:delete(name, Fields),
                    NewName = integer_to_list(AnymetaId)++"_"++z_convert:to_list(Name),
                    Fields2 = [{name,NewName}|Fields1],
                    progress(io_lib:format("~w: NAME CLASH, renamed ~p to ~p", [AnymetaId, Name, NewName]), Context),
                    {ok, RscId} = m_rsc_update:insert(Fields2, [{escape_texts, false}, is_import], Context);
                none ->
                    progress(io_lib:format("~w: Inserted", [AnymetaId]), Context),
                    {ok, RscId} = m_rsc_update:insert(Fields, [{escape_texts, false}, is_import], Context)
            end,
            register_import(Host, RscId, AnymetaId, RscUri, Context),
            register_redirect(RedirectUri, RscUri, RscId, Host, Context),
            {ok, RscId, Stats}
    end.

    ensure_category(Category, Context) ->
        case m_category:name_to_id(Category, Context) of
            {ok, _Id} ->
                ok;
            {error, _} ->
                progress(io_lib:format("Insert category ~w", [Category]), Context),
                m_rsc:insert([
                        {name, z_convert:to_binary(Category)},
                        {category, category},
                        {title, z_convert:to_binary(Category)}
                    ], Context)
        end.

    ensure_predicate(Predicate, Context) ->
        case m_predicate:name_to_id(Predicate, Context) of
            {ok, _Id} ->
                ok;
            {error, _} ->
                m_rsc:insert([
                        {name, z_convert:to_binary(Predicate)},
                        {category, predicate},
                        {title, z_convert:to_binary(Predicate)}
                    ], Context),
                z_depcache:flush(Context)
        end.


    check_previous_import(Host, RscUri, Context) ->
        case z_db:q1("select rsc_id from import_anymeta where rsc_uri = $1 and host = $2", [RscUri, Host], Context) of
            undefined ->
                none;
            RscId -> 
                {ok, RscId}
        end.

    register_import(Host, RscId, undefined, RscUri, Context) ->
        z_db:q("insert into import_anymeta (rsc_uri, rsc_id, anymeta_id, host, stub, imported)
                values ($1, $2, $3, $4, true, now())",
               [RscUri, RscId, null, Host],
               Context);
    register_import(Host, RscId, AnymetaId, RscUri, Context) ->
        z_db:q("delete from import_anymeta 
                where stub = true and host = $1 and rsc_uri = $2",
               [Host, RscUri],
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

    register_redirect(RedirectUri, RscUri, RscId, Host, Context) ->
        case RedirectUri of
          undefined ->
            false;
          <<>> ->
            false;
          _ ->
            z_db:q("insert into anymeta_redirects (redirect_uri, rsc_uri, rsc_id, host)
                    values ($1, $2, $3, $4)",
                   [RedirectUri, RscUri, RscId, Host],
                   Context)
        end.

    check_existing_rsc(Fields, Context) ->
        case proplists:get_value(name, Fields) of
            undefined ->
                none;
            Name ->
                case z_utils:only_digits(Name) of
                    true  -> numeric;
                    false -> 
                        case m_rsc:name_to_id(Name, Context) of
                            {ok, RscId} ->
                                Category = z_convert:to_binary(proplists:get_value(category, Fields)),
                                case z_convert:to_binary(proplists:get_value(name, m_rsc:p(RscId, category, Context))) of
                                    Category ->
                                        {ok, RscId};
                                    _Other ->
                                        clash
                                end;
                            _ ->
                                none
                        end
                end
        end.

    ensure_rsc_uri(Host, RscUri, Context) ->
        case check_previous_import(Host, RscUri, Context) of
            {ok, RscId} ->
                RscId;
            none ->
                Ps = [
                    {category, other},
                    {is_published, false},
                    {visible_for, 1},
                    {title, iolist_to_binary(["Edge object stub: ",RscUri])},
                    {anymeta_rsc_uri, RscUri},
                    {anymeta_host, Host}
                ],
                {ok, RscId} = m_rsc:insert(Ps, Context),
                register_import(Host, RscId, undefined, RscUri, Context),
                progress(io_lib:format("    Edge stub for ~p (zotonic id ~w)", [RscUri, RscId]), Context),
                RscId
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


import_edges(_Host, _RscId, undefined, Stats, _Context) ->
    Stats;
import_edges(_Host, _RscId, null, Stats, _Context) ->
    Stats;
import_edges(Host, RscId, Edges, Stats, Context) when is_list(Edges) ->
    lists:foldl(fun(Edge, St) ->
                    import_edge(Host, RscId, Edge, St, Context)
                end,
                Stats,
                Edges).

% TODO: do something with the date on the edge (if any)
%       especially for events w/ presented_at predicate
import_edge(Host, SubjectId, {struct, Props}, Stats, Context) ->
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
            ObjectId = ensure_rsc_uri(Host, ObjectRscUri, Context),
            progress(io_lib:format("    Edge ~w -[~p]-> ~w", [SubjectId,P1,ObjectId]), Context),
            {ok, EdgeId} = m_edge:insert(SubjectId, P1, ObjectId, [no_touch], Context),
            case proplists:get_value(<<"order">>, Edge) of
                <<"9999">> ->
                    nop; %% default
                OrderBin ->
                    z_db:update(edge, EdgeId, [{seq, z_convert:to_integer(OrderBin)}], Context)
            end,
            Stats
    end.


import_keywords(_Host, _RscId, undefined, Stats, _Context) ->
    Stats;
import_keywords(_Host, _RscId, null, Stats, _Context) ->
    Stats;
import_keywords(Host, RscId, KeywordIds, Stats, Context) when is_list(KeywordIds) ->
    lists:foldl(fun(KwId, St) ->
                    import_keyword(Host, RscId, KwId, St, Context)
                end,
                Stats,
                KeywordIds).
    
    import_keyword(Host, SubjectId, KeywordId, Stats, Context) ->
        case find_any_id(KeywordId, Host, Context) of
            {ok, ZotonicId} ->
                progress(io_lib:format("    Edge ~w -[~p]-> ~w", [SubjectId,subject,ZotonicId]), Context),
                {ok, _} = m_edge:insert(SubjectId, subject, ZotonicId, [no_touch], Context),
                Stats;
            undefined ->
                progress(io_lib:format("    Keyword ~w (not yet imported, delayed)", [KeywordId]), Context),
                Stats#stats{delayed=[{keyword, SubjectId, KeywordId}|Stats#stats.delayed]}
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
                                [] -> [ map_kind_type(K, []) || K <- Kinds ];
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


