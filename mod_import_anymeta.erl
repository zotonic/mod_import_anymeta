%% @author Marc Worrell <marc@worrell.nl>
%% @copyright 2011-2012 Marc Worrell
%% @doc Import data from an Anymeta website

%% Copyright 2011-2012 Marc Worrell
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

%% TODO: should we allow <br/> in the summary?  We can enter it.

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

    get_thing/5,
    test_host/5
]).

-include_lib("zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").

-record(stats, {
    found = 0,
    notfound = 0,
    error = [],
    consequetive_notfound = 0,
    start_time = now()
}).

init(Context) ->
    case z_db:table_exists(import_anymeta, Context) of
        true ->
            ok;
        false ->
            [] = z_db:q("
                create table import_anymeta (
                    uuid character varying(40) not null,
                    anymeta_id int not null,
                    rsc_id int not null,
                    imported timestamp not null,
                    
                    constraint import_anymeta_pkey primary key (uuid),
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
    Parts = string:tokens(Path, "/"),
    case lists:reverse(Parts) of
        [AnyId,"id.php"|_] ->
            redirect(AnyId, undefined, Context);
        [Rsc|_] ->
            case filename:extension(Rsc) of
                ".html" ->
                    case string:tokens(filename:rootname(Rsc), "-") of
                        [_Kind,AnyId,[_,_] = Lang] ->
                            redirect(AnyId, Lang, Context);
                        [_Kind,AnyId] ->
                            redirect(AnyId, undefined, Context)
                    end;
                _ ->
                    undefined
            end
    end.

    redirect(AnyId, Lang, Context) ->
        case find_any_id(AnyId, Context) of
            undefined -> 
                undefined;
            {ok, RscId} -> 
                case z_trans:is_language(Lang) of
                    true ->
                        % Add language
                        Context1 = z_context:set_language(list_to_atom(Lang), Context);
                    false ->
                        % Ignore language
                        Context1 = Context
                end,
                case m_rsc:p(RscId, page_url, Context1) of
                    undefined ->
                        undefined;
                    URL ->
                        {ok, #dispatch_match{
                            mod=resource_redirect,
                            mod_opts=[{url, URL}, {is_permanent, true}],
                            bindings=[]
                        }}
                end
        end.
    
    
    find_any_id(AnyId, Context) ->
        case z_utils:only_digits(AnyId) of
            true ->
                % anyMeta id
                case z_db:q1("select rsc_id from import_anymeta where anymeta_id = $1", [AnyId], Context) of
                    undefined -> undefined;
                    RscId -> {ok, RscId}
                end;
            false ->
                % UUID or name
                case m_rsc:name_to_id(AnyId, Context) of
                    {ok, RscId} -> {ok, RscId};
                    {error, _} ->
                        case z_db:q1("select rsc_id from import_anymeta where uuid = $1", [AnyId], Context) of
                            undefined -> undefined;
                            RscId -> {ok, RscId}
                        end
                end
        end.

event(#submit{message=find_imported}, Context) ->
    AnymetaId = z_convert:to_integer(z_string:trim(z_context:get_q_validated("imported_id", Context))),
    case z_db:q1("select rsc_id from import_anymeta where anymeta_id = $1", [AnymetaId], Context) of
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
            Password = z_string:trim(z_context:get_q("sysadmin-pw", Context)),
            Username = case Password of [] -> []; _ -> "sysadmin" end,
            From     = z_convert:to_integer(z_context:get_q_validated("start-id", Context)),
            To       = z_convert:to_integer(z_context:get_q_validated("end-id", Context)),
            Host     = z_string:trim(z_context:get_q("host", Context)),
            
            z_context:set_session(anymeta_host, Host, Context),
            
            case do_import(Host, From, To, Username, Password, Context) of
                ok ->
                    z_render:wire([{fade_in, [{target, "import-started"}]}, {hide, [{target, Form}]}], Context);
                {error, nxdomain} ->
                    z_render:wire([{fade_in, [{target, "import-nxdomain"}]}, {hide, [{target, "import-error"}]}], Context);
                {error, _Other} ->
                    z_render:wire([{fade_in, [{target, "import-error"}]}, {hide, [{target, "import-nxdomain"}]}], Context)
            end
    end.


do_import(Host, undefined, To, Username, Password, Context) ->
    do_import(Host, 1, To, Username, Password, Context);
do_import(Host, From, To, Username, Password, Context) ->
    case test_host(From, Host, Username, Password, Context) of
        ok ->
            start_import(Host, From, To, Username, Password, Context);
        Err ->
            Err
    end.

    start_import(Host, From, To, Username, Password, Context) ->
        ContextPruned = z_context:prune_for_async(Context),
        spawn(fun() ->
                    import_loop(Host, From, To, Username, Password, #stats{}, ContextPruned)
              end),
        ok.
    
    test_host(From, Host, Username, Password, Context) ->
        Url = get_url(From, Host),
        progress(io_lib:format("TEST HOST: pinging ~p ...", [Url]), Context),
        case get_request(head, Url, Username, Password) of
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
            {ok, {{_, 503, _},Hs, _}} -> 
                progress("TEST HOST: 503 Service Not Available", Context),
                case z_string:to_lower(proplists:get_value("x-powered-by", Hs, [])) of
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


get_url(Id, Host) ->
    "http://"++Host++"/thing/"++integer_to_list(Id)++"?format=json".

get_thing(Id, Hostname, User, Pass, Context) ->
    Url = get_url(Id, Hostname),
    case get_request(get, Url, User, Pass) of
        {ok, {
            {_HTTP, 200, _OK},
            Headers,
            Body
        }} ->
            case proplists:get_value("content-type", Headers) of
                "application/json" ++ _ ->
                    {struct, Props} = mochijson2:decode(Body),
                    {ok, Props};
                CT ->
                    {error, {unexpected_content_type, CT}}
            end;
        {error, _Reason} = Err ->
            Err;
        {ok, {{_, 503, _}, _, _}} ->
            {error, no_service};
        {ok, {{_, 404, _}, _, _}} ->
            {error, not_found};
        {ok, Other} ->
            progress(io_lib:format("FAIL! ~p Unexpected Result<br/>~p", [Url, Other]), Context),
            {error, unexpected_result}
    end.

get_request(Method, Url, User, Pass) ->
    Headers = auth_header(User, Pass),
    httpc:request(Method, {Url, Headers}, [{autoredirect, true}, {relaxed, true}, {timeout, 10000}], []).

    auth_header(undefined, undefined) ->
        [];
    auth_header([], []) ->
        [];
    auth_header(User, Pass) ->
        Encoded = base64:encode_to_string(lists:append([User,":",Pass])),
        [{"Authorization","Basic " ++ Encoded}].


%% @doc Fetch all items from the given host
import_loop(_, _From, undefined, _Username, _Password, #stats{consequetive_notfound=NF}, Context) when NF > 100 ->
    % Signal the end of the import to the UI
    progress("STOP - found more than 100 consequetive not founds.<br/>", Context),
    ok;
import_loop(_, From, To, _Username, _Password, _Stats, Context) when is_integer(To), From > To ->
    % Signal the end of the import to the UI
    progress(io_lib:format("STOP - At last import anymeta id (~w).<br/>", [To]), Context),
    ok;
import_loop(Host, From, To, Username, Password, Stats, Context) ->
    progress(io_lib:format("~w: fetching from ~p", [From, Host]), Context),
    case get_thing(From, Host, Username, Password, Context) of
        {ok, Thing} ->
            progress(io_lib:format("~w: importing", [From]), Context),
            Stats1 = import_thing(Host, From, Thing, Stats, Context),
            Stats2 = Stats1#stats{
                found=Stats#stats.found+1, 
                consequetive_notfound=0
            },
            import_loop(Host, From+1, To, Username, Password, Stats2, Context);
        {error, no_service} ->
            progress(io_lib:format("~w: got 503 - waiting 10 seconds before retry.", [From]), Context),
            % Anymeta servers give a 503 when they are overloaded.
            % Sleep for 10 seconds and then retry our request.
            timer:sleep(10000),
            import_loop(Host, From, To, Username, Password, Stats, Context);
        {error, not_found} ->
            progress(io_lib:format("~w: not found, skipping to next", [From]), Context),
            Stats1 = Stats#stats{
                notfound=Stats#stats.notfound+1, 
                consequetive_notfound=Stats#stats.consequetive_notfound+1,
                error=[{From, notfound} | Stats#stats.error]
            },
            import_loop(Host, From+1, To, Username, Password, Stats1, Context);
        {error, Reason} ->
            progress(io_lib:format("~w: error, skipping to next (error: ~p)", [From, Reason]), Context),
            Stats1 = Stats#stats{
                error=[{From, Reason} | Stats#stats.error]
            },
            import_loop(Host, From+1, To, Username, Password, Stats1, Context)
    end.


import_thing(Host, AnymetaId, Thing, Stats, Context) ->
    % Check if the <<"lang">> section is available, if so then we had read acces, otherwise skip
    case skip(Thing) of
        false ->
            {struct, Texts} = proplists:get_value(<<"lang">>, Thing),
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
                        {anymeta_uuid, proplists:get_value(<<"thg_uuid">>, Thing)},
                        {anymeta_host, Host},
                        {language, Langs}
                        |OtherFields
                     ]
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
            Fields2 = z_notifier:foldl(import_anymeta_fields, Fields1, Context),
            
            case write_rsc(AnymetaId, Fields2, Stats, Context) of
                {ok, RscId, Stats1} ->
                    Stats2 = import_edges(Host, RscId, proplists:get_value(<<"edge">>, Thing), Stats1, Context),
                    case proplists:get_value(<<"file">>, Thing) of
                        undefined ->
                            Stats2;
                        {struct, File} -> 
                            Filename = proplists:get_value(<<"original_file">>, File),
                            {struct, Fileblob} = proplists:get_value(<<"file_blob">>, File),
                            case proplists:get_value(<<"encode">>, Fileblob) of
                                <<"base64">> ->
                                    Data = base64:decode(proplists:get_value(<<"data">>, Fileblob)),
                                    write_file(AnymetaId, RscId, Filename, Data, Stats2, Context);
                                Encoding ->
                                    Stats2#stats{error=[{AnymetaId, {unknown_file_encoding, Encoding}} | Stats2#stats.error]}
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
                case proplists:get_value(<<"uri">>, Thing) of
                    <<"javascript:any_action", _/binary>> -> true;
                    <<"../admin.php", _/binary>> -> true;
                    _ ->
                        % TODO: add a callback for skippable kinds
                        case proplists:get_value(<<"kind">>, Thing) of
                            <<"ROLE">> -> true;
                            <<"TYPE">> -> true;
                            <<"LANGUAGE">> -> true;
                            _ -> false
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
    map_text_field(<<"body">>, T) ->
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
    D1 = lists:foldl(fun({L,T}, Acc) ->
                        dict:append(L, {Iso, T}, Acc)
                     end,
                     D,
                     Texts),
    group_by_lang(Ts, D1).



map_fields(Thing) ->
    map_fields(Thing, []).

map_fields([], Acc) ->
    Acc;
map_fields([{<<"pubstate">>, <<"1">>}|T], Acc) ->
    map_fields(T, [{is_published, true}|Acc]);
map_fields([{<<"pubstate">>, _}|T], Acc) ->
    map_fields(T, [{is_published, false}|Acc]);
map_fields([{<<"pub_date_start">>, Date}|T], Acc) ->
    map_fields(T, [{publication_start, convert_datetime(Date)}|Acc]);
map_fields([{<<"pub_date_end">>, Date}|T], Acc) ->
    map_fields(T, [{publication_end, convert_datetime(Date)}|Acc]);
map_fields([{<<"create_date">>, Date}|T], Acc) ->
    map_fields(T, [{created, convert_datetime(Date)}|Acc]);
map_fields([{<<"modify_date">>, Date}|T], Acc) ->
    map_fields(T, [{modified, convert_datetime(Date)}|Acc]);
map_fields([{<<"coverage">>, {struct, Cs}}|T], Acc) ->
    % TODO: map the location GPS to zotonic props
    % Time & location
    Acc1 = lists:foldl(
        fun({<<"date_start">>, D}, A) -> [ {date_start, convert_datetime(D)} | A ];
           ({<<"date_end">>, D}, A) -> [ {date_end, convert_datetime(D)} | A ];
           (_, A) -> A
        end,
        Acc,
        Cs),
    map_fields(T, Acc1);
map_fields([{<<"symbolic_name">>, <<"">>}|T], Acc) ->
    map_fields(T, [{name, undefined}|Acc]);
map_fields([{<<"symbolic_name">>, <<"NULL">>}|T], Acc) ->
    map_fields(T, [{name, undefined}|Acc]);
map_fields([{<<"symbolic_name">>, Name}|T], Acc) ->
    map_fields(T, [{name, z_string:to_lower(Name)}|Acc]);
map_fields([{<<"axo">>, Axo}|T], Acc) ->
    case z_convert:to_list(z_string:to_lower(Axo)) of
        "public" -> map_fields(T, [{visible_for, 0}|Acc]);
        "system" -> map_fields(T, [{visible_for, 0}|Acc]);
        "metadata" -> map_fields(T, [{visible_for, 0}|Acc]);
        _ -> map_fields(T, [{visible_for, 1}|Acc])
    end;
map_fields([{<<"rights">>, <<"">>}|T], Acc) ->
    map_fields(T, Acc);
map_fields([{<<"rights">>, <<"CR">>}|T], Acc) ->
    map_fields(T, Acc);
map_fields([{<<"rights">>, Rights}|T], Acc) ->
    map_fields(T, [{cc_rights, Rights}|Acc]);
map_fields([_|T], Acc) ->
    map_fields(T, Acc).


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
                _ -> {category, document}
            end
    end;
fix_media_category(Cat, _Thing) ->
    Cat.


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

map_kind_type(<<"ROLE">>, _) -> predicate;
map_kind_type(<<"TYPE">>, _) -> category;
map_kind_type(<<"ATTACHMENT">>, _) -> media;
map_kind_type(<<"ARTICLE">>, _) -> article;
map_kind_type(<<"ARTEFACT">>, _) -> artifact;
map_kind_type(<<"LISTPUBLISH">>, _) -> 'query';
map_kind_type(<<"LISTEDIT">>, _) -> 'query';
map_kind_type(<<"LANGUAGE">>, _) -> language;
map_kind_type(<<"INSTITUTION">>, _) -> organization;
map_kind_type(<<"SET">>, _) -> collection;
map_kind_type(<<"KEYWORD">>, _) -> keyword;
map_kind_type(<<"TAG">>, _) -> keyword;
map_kind_type(<<"PERSON">>, _) -> person;
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

map_language(X) -> X.
 


write_rsc(AnymetaId, Fields, Stats, Context) ->
    Uuid = proplists:get_value(anymeta_uuid, Fields),
    ensure_category(proplists:get_value(category, Fields), Context),
    case check_previous_import(Uuid, Context) of
        {ok, RscId} ->
            Fields1 = proplists:delete(name, Fields),
            progress(io_lib:format("~w: updating (zotonic id: ~w)", [AnymetaId, RscId]), Context),
            {ok, RscId} = m_rsc_update:update(RscId, Fields1, [{escape_texts, false}, is_import], Context),
            register_import_anymeta_id(AnymetaId, Uuid, Context),
            {ok, RscId, Stats};
        none -> 
            Name = proplists:get_value(name, Fields),
            case check_existing_rsc(Fields, Context) of
                {ok, RscId} ->
                    progress(io_lib:format("~w: exists, updating (name: ~p, zotonic id: ~w)", [AnymetaId, Name, RscId]), Context),
                    {ok, RscId} = m_rsc_update:update(RscId, Fields, [{escape_texts, false}, is_import], Context);
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
            register_import(RscId, AnymetaId, Uuid, Context),
            {ok, RscId, Stats}
    end.

    ensure_category(Category, Context) ->
        case m_category:name_to_id(Category, Context) of
            {ok, _Id} ->
                ok;
            {error, _} ->
                progress(io_lib:format("Insert category ~w", [category]), Context),
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
                    ], Context)
        end.


    check_previous_import(Uuid, Context) ->
        case z_db:q1("select rsc_id from import_anymeta where uuid = $1", [Uuid], Context) of
            undefined ->
                none;
            RscId -> 
                {ok, RscId}
        end.
        
    register_import(RscId, AnymetaId, Uuid, Context) ->
        z_db:q("insert into import_anymeta (uuid, rsc_id, anymeta_id, imported)
                values ($1, $2, $3, now())",
               [Uuid, RscId, AnymetaId],
               Context).
               
    % Used when a resource was previously created as an edge-object stub
    register_import_anymeta_id(AnymetaId, Uuid, Context) ->
        z_db:q("update import_anymeta set anymeta_id = $1
                where uuid = $2 and anymeta_id = 0",
               [AnymetaId, Uuid],
               Context).

    check_existing_rsc(Fields, Context) ->
        case proplists:get_value(name, Fields) of
            undefined ->
                none;
            Name ->
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
        end.

    ensure_uuid(Host, Uuid, Context) ->
        case check_previous_import(Uuid, Context) of
            {ok, RscId} ->
                RscId;
            none ->
                Ps = [
                    {category, other},
                    {is_published, false},
                    {visible_for, 1},
                    {title, iolist_to_binary(["Edge object stub: ",Uuid])},
                    {anymeta_uuid, Uuid},
                    {anymeta_host, Host}
                ],
                {ok, RscId} = m_rsc:insert(Ps, Context),
                register_import(RscId, 0, Uuid, Context),
                progress(io_lib:format("    Edge stub for ~p (zotonic id ~w)", [Uuid, RscId]), Context),
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
            ObjectUuid = proplists:get_value(<<"object_uuid">>, Edge),
            ObjectId = ensure_uuid(Host, ObjectUuid, Context),
            progress(io_lib:format("    Edge ~w -[~p]-> ~w", [SubjectId,P1,ObjectId]), Context),
            {ok, _} = m_edge:insert(SubjectId, P1, ObjectId, [no_touch], Context),
            Stats
    end.


%% @doc Show progress in the progress area of the import window.
progress(Message, Context) ->
    mod_signal:emit_script({import_anymeta_progress, []}, 
                           z_render:wire({insert_top, [{target, "progress"}, {text, [Message,"<br/>"]}]}, Context)).
