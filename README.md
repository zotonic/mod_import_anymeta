Zotonic - Import data from an anyMeta site
==========================================

[Zotonic](http://zotonic.com/) is a CMS.
It is written in Erlang.
It is extensible with modules.
It is pretty fast.

This is a module for Zotonic.
It can import data from anyMeta sites.

anyMeta is an information management system made by [Mediamatic Lab](http://www.mediamatic.nl/)

All data is fetched using the anyMeta thing API: http://example.com/thing/12345?format=json Where 12345 is the id of the to be exported page.

When importing you can supply a range of ids to be imported. If only the start of the range is filled in then the import will continue till it finds 100 consecutive not founds, at which point it will stop.

It is safe to re-run the import, as the module keeps a mapping of Anymeta UUID to Zotonic resource id.

Optionally you dan supply the sysadmin password of the anyMeta site to import non-public data.


Database
--------

The module keeps an administration of all imported pages.

This administration is in the table `import_anymeta` and contains a mapping from the anyMeta uuid and thing-id to the local resource id, including a timestamp when the import was done.


Admin templates and menu
------------------------

The import module overrules and adds some extra templates to the admin:

 * A menu item *Import from anyMeta* in the **Modules** menu
 * The basic form (title, subtitle etc.) is changed so that an extra field `chapeau` is added.
 * An extra sidebar template with information about the conversion and a link to the page on the original anyMeta site


Limitations
-----------

This module has the following limitations:

 * anyMeta `listpublish` and `listedit` are only partially converted, a manual correction will be necessary.
 * An empty placeholder resource will be created for some unconverted *things*, these placeholders can be deleted later.
 * Menu’s need to be re-done, as the menu structure of Zotonic and anyMeta is totally different.


Conversion callbacks
--------------------

The *import_anymeta* module uses three notifications to help in the conversion:

 * A *first* notification to map an anyMeta kind/type combination into a Zotonic category:
   `{import_anymeta_kind_type, AnymetaKind, AnymetaTypeList}` This expects a return of `{ok, CategoryAtom}` or `undefined`

 * A *fold* to convert an anyMeta thing to a Zotonic resource: `import_anymeta_fields` with a property list as the accumulator.
   A first default mapping of anyMeta fields to Zotonic properties has been done already, the handlers can do subsequent mapping
   or translation of values.

 * A *first* to map a predicate: `{import_anymeta_map_predicate, Predicate}` Where `Predicate` is an atom, either already mapped for 
   some standard predicates, or a lower-cased atom representation of the original predicate’s name.  A handler should either
   return `undefined` to keep the original predicate name, or an atom with the name of the predicate to be used.

Below are two example modules from live projects.


Example 1: Women on Waves
-------------------------

This is the conversion used by the Women on Waves web site:

    %% @doc Women on Waves main site module
    %% @copyright 2012 Marc Worrell

    -module(womenonwaves).

    -mod_prio(50).
    -mod_title("Women on Waves").
    -mod_description("Web site for Women on Waves.").
    -mod_schema(3).

    -export([
        observe_import_anymeta_kind_type/2,
        observe_import_anymeta_map_predicate/2,

        manage_schema/2
    ]).

    -include_lib("zotonic.hrl").

    observe_import_anymeta_kind_type({import_anymeta_kind_type, <<"ATTACHMENT">>, [<<"PRESS_COVERAGE">>,<<"CARTOON">>|_]}, _Context) ->
        {ok, cartoon};
    observe_import_anymeta_kind_type({import_anymeta_kind_type, <<"ATTACHMENT">>, [<<"CARTOON">>|_]}, _Context) ->
        {ok, cartoon};
    observe_import_anymeta_kind_type({import_anymeta_kind_type, <<"ATTACHMENT">>, [<<"PRESS_COVERAGE">>|<<"OPINION">>]}, _Context) ->
        {ok, press_image};
    observe_import_anymeta_kind_type({import_anymeta_kind_type, <<"ATTACHMENT">>, [<<"PRESS_COVERAGE">>|_]}, _Context) ->
        {ok, press_image};
    observe_import_anymeta_kind_type({import_anymeta_kind_type, <<"ATTACHMENT">>, [<<"PRESS_COVERAGE_E">>|_]}, _Context) ->
        {ok, press_image};
    observe_import_anymeta_kind_type({import_anymeta_kind_type, <<"ATTACHMENT">>, [<<"CAMPAIGNS_DIARY_POLAND">>|_]}, _Context) ->
        {ok, campaign_image};
    observe_import_anymeta_kind_type({import_anymeta_kind_type, <<"ARTICLE">>, [<<"PRESSRELEASE">>|_]}, _Context) ->
        {ok, press_release};
    observe_import_anymeta_kind_type({import_anymeta_kind_type, <<"ARTICLE">>, [<<"PRESS_RELEASE">>|_]}, _Context) ->
        {ok, press_release};
    observe_import_anymeta_kind_type({import_anymeta_kind_type, <<"ARTICLE">>, [<<"PRESS">>|_]}, _Context) ->
        {ok, press};
    observe_import_anymeta_kind_type({import_anymeta_kind_type, <<"ARTICLE">>, [<<"ABOUT_US">>|_]}, _Context) ->
        {ok, about_us};
    observe_import_anymeta_kind_type({import_anymeta_kind_type, <<"ARTICLE">>, [<<"CAMPAIGNS">>,<<"DIARY">>]}, _Context) ->
        {ok, campaign_diary};
    observe_import_anymeta_kind_type({import_anymeta_kind_type, <<"ARTICLE">>, [<<"DIARY">>]}, _Context) ->
        {ok, diary};
    observe_import_anymeta_kind_type({import_anymeta_kind_type, <<"ARTICLE">>, [<<"CAMPAIGNS">>|_]}, _Context) ->
        {ok, campaigns};
    observe_import_anymeta_kind_type({import_anymeta_kind_type, <<"ARTICLE">>, [<<"REALITY">>|_]}, _Context) ->
        {ok, reality};
    observe_import_anymeta_kind_type({import_anymeta_kind_type, <<"ARTICLE">>, [<<"WE_HELP">>|_]}, _Context) ->
        {ok, reality};
    observe_import_anymeta_kind_type({import_anymeta_kind_type, <<"ARTICLE">>, [<<"PRESS_COVERAGE">>|_]}, _Context) ->
        {ok, media_press};
    observe_import_anymeta_kind_type({import_anymeta_kind_type, _Kind, _TypesSymbolic}, _Context) ->
        ?DEBUG({_Kind, _TypesSymbolic}),
        undefined.


    observe_import_anymeta_map_predicate({import_anymeta_map_predicate, diary_portugal}, _Context) ->
        related;
    observe_import_anymeta_map_predicate({import_anymeta_map_predicate, set_campaigns}, _Context) ->
        hascampaigncollection;
    observe_import_anymeta_map_predicate({import_anymeta_map_predicate, set_reality}, _Context) ->
        hasrealitycollection;
    observe_import_anymeta_map_predicate({import_anymeta_map_predicate, P}, _Context) ->
        P.

    manage_schema(install, _Context) ->
        #datamodel{
            categories = [
                  {anymeta_type, categorization, [{title, <<"Anymeta Type/Kind">>}] },
                  {press_image, image, [{title, <<"Press Image">>}]},
                  {campaign_image, image, [{title, <<"Campaign Image">>}]},
                  {cartoon, image, [{title, <<"Cartoon">>}]}
            ],
            predicates=[
                {hasfeatured, [{title, <<"Has Featured">>}], []}
            ],
            resources=[
                    {page_home,
                     text,
                     [{title, <<"Home">>},
                      {summary, <<"Women on Waves is a Dutch non-profit organization concerned with women’s human rights. Its mission is to prevent unwanted pregnancy and unsafe abortions throughout the world.">>},
                      {page_path, <<"/">>}
                     ]
                    }
            ]};
    manage_schema({upgrade, 2}, _Context) -> #datamodel{categories=[{cartoon, image, [{title, <<"Cartoon">>}]}]};
    manage_schema({upgrade, 3}, _Context) -> #datamodel{categories=[{campaign_image, image, [{title, <<"Campaign Image">>}]}]}.






Example 1: Women on Web
-----------------------

This is the conversion used by the Women on Web web site:

    -module(womenonweb).

    -mod_prio(50).
    -mod_title("Women on Web").
    -mod_description("Web site for Women on Web.").

    -export([
        observe_import_anymeta_kind_type/2,
        observe_import_anymeta_fields/3
    ]).

    -include("zotonic.hrl").

    observe_import_anymeta_kind_type({import_anymeta_kind_type, <<"ATTACHMENT">>, [<<"PORTRAIT">>|_]}, _Context) ->
        {ok, portrait};
    observe_import_anymeta_kind_type({import_anymeta_kind_type, <<"LOCATION">>, [<<"COUNTRY">>|_]}, _Context) ->
        {ok, country};
    observe_import_anymeta_kind_type({import_anymeta_kind_type, <<"KEYWORD">>, [X|_]}, _Context) ->
        {ok, z_convert:to_atom(z_string:to_lower(X))};
    observe_import_anymeta_kind_type({import_anymeta_kind_type, <<"ARTICLE">>, Ts}, _Context) ->
        case lists:member(<<"QUESTION">>, Ts) of
            true -> {ok, consult_question};
            false ->
                case lists:member(<<"CONSULT">>, Ts) of
                    true -> {ok, qanda};
                    false -> undefined
                end
        end;
    observe_import_anymeta_kind_type({import_anymeta_kind_type, _Kind, _TypesSymbolic}, _Context) ->
        ?DEBUG({_Kind, _TypesSymbolic}),
        undefined.


    %% @doc Map some block texts in the portraits to normal properties.
    observe_import_anymeta_fields(import_anymeta_fields, Props, _Context) ->
        case proplists:get_value(category, Props) of
            portrait ->
                Bs = proplists:get_value(blocks, Props, []),
                [
                    {name_first,   take_one(qb([<<"name_first">>], Bs))},
                    {name_surname, take_one(qb([<<"name_last">>], Bs))},
                    {q_feeling,    qb([<<"wiz_keyword_q_feeling">>, <<"q_feeling">>], Bs)},
                    {q_how,        qb([<<"wiz_keyword_q_how">>, <<"q_how">>], Bs)},
                    {q_situation,  qb([<<"wiz_keyword_q_situation">>, <<"q_situation">>], Bs)},
                    {q_status,     qb([<<"wiz_keyword_q_status">>, <<"q_status">>], Bs)},
                    {q_where,      qb([<<"wiz_keyword_q_where">>, <<"q_where">>], Bs)},
                    {blocks, []}
                    | proplists:delete(blocks, Props)
                ];
            _ ->
                Props
        end.
    
    qb(_, []) ->
        undefined;
    qb(P, [B|Bs]) ->
        Name = proplists:get_value(name, B),
        case lists:member(Name, P) of
            true ->
                case proplists:get_value(body, B) of
                    undefined -> undefined;
                    null -> undefined;
                    X when is_binary(X) -> z_string:trim(z_html:strip(X));
                    L when is_list(L) -> z_string:trim(z_html:strip(iolist_to_binary(L)));
                    {trans, Ts} -> {trans, [{Iso,z_string:trim(z_html:strip(X))} || {Iso,X} <- Ts]}
                end;
            false ->
                qb(P, Bs)
        end.
    
    take_one(undefined) -> undefined;
    take_one(null) -> undefined;
    take_one(B) when is_binary(B) -> B;
    take_one(L) when is_list(L) -> L;
    take_one({trans, []}) -> undefined;
    take_one({trans, [{_,B}|_]}) -> B.




