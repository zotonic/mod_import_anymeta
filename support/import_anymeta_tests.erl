-module(import_anymeta_tests).

-export([test/1]).

-include_lib("../include/mod_import_anymeta.hrl").

test(Context0) ->
    Context = z_acl:sudo(Context0),
    mod_import_anymeta:init(Context),
    
    Host = "test.com",

    Files = filelib:wildcard(code:lib_dir(zotonic) ++ "/priv/modules/mod_import_anymeta/testdata/*.json"),
    FilesWithThingId = lists:zip(lists:seq(1000, 1000+length(Files)-1), Files),
    
    Stats1 =
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
    Stats = Stats1,
    io:format("Test stats: ~p~n", [Stats]),
    ok. 
