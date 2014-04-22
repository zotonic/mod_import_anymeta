-module(import_anymeta_tests).

-export([test/1, test_full/1]).

-include_lib("../include/mod_import_anymeta.hrl").

-define(FRED, "http://common.test.mediamatic.nl/id/352").
-define(IMAGE1, "http://common.test.mediamatic.nl/id/353").
-define(IMAGE2, "http://common.test.mediamatic.nl/id/354").
-define(IMAGE3, "http://common.test.mediamatic.nl/id/355").
-define(IMAGE4, "http://common.test.mediamatic.nl/id/356").
-define(KEYWORD1, "http://common.test.mediamatic.nl/id/357").

test_full(Context0) ->
    Context = z_acl:sudo(Context0),
    mod_import_anymeta:init(Context),

    Host = "test.com",

    Files = files(),
    FilesWithThingId = lists:zip(lists:seq(1000, 1000+length(Files)-1), Files),

    lists:foldl(
      fun({ThingId, Filename}, Stats) ->
              {ok, Body} = file:read_file(Filename),
              {struct, Thing} = mochijson2:decode(z_string:sanitize_utf8(Body)),
              Stats1 = mod_import_anymeta:import_thing(Host, ThingId, Thing, true, Stats, Context),
              Stats1#stats{
                found=Stats#stats.found+1, 
                consequetive_notfound=0
               }
      end,
      #stats{},
      FilesWithThingId
     ),

    %%Stats = handle_delayed(Stats1#stats.delayed, Host, [], [], true, Stats1#stats{delayed=[]}, Context),
    test(Context).

test(Context0) ->
    Context = z_acl:sudo(Context0),
    Files = files(),
    test_import_person_fred(json_file_to_id(lists:nth(1, Files), Context),
                            json_file_to_id(lists:nth(10, Files), Context),
                            Context),

    lager:info("All tests ok.").

files() ->
    filelib:wildcard(code:lib_dir(zotonic) ++ "/priv/modules/mod_import_anymeta/testdata/*.json").

json_file_to_id(File, Context) ->
    {ok, Body} = file:read_file(File),
    {struct, Thing} = mochijson2:decode(z_string:sanitize_utf8(Body)),
    RscUri = proplists:get_value(<<"resource_uri">>, Thing),
    {ok, Id} = mod_import_anymeta:find_any_id(RscUri, Context),
    Id.




test_import_person_fred(Id, OrgId, Context) ->
    <<"FredP">> = z_trans:trans(m_rsc:p(Id, title, Context), Context),
    <<"Fred">> = m_rsc:p(Id, name_first, Context),
    <<"P">> = m_rsc:p(Id, name_surname, Context),

    [OrgId] = m_edge:objects(Id, interest, Context),
    [OrgId] = m_edge:objects(Id, works_for, Context),
    ok.
