%% @doc Low level function to correct an issue where some external things where mapped to a single resource
%%      due to duplicate usage of names in Anymeta.

-module(import_anymeta_duplicates).

-export([
    duplicates/1,
    run/3,
    reimport/5
    ]).

-include("../include/mod_import_anymeta.hrl").

duplicates(Context) ->
    z_db:q("
        create table import_anymeta_duplicates as
        select imp.rsc_id, imp.anymeta_id, imp.host, imp.rsc_uri
        from import_anymeta imp,
             (select rsc_id, count(*) as ct 
                from import_anymeta 
                group by rsc_id 
                having count(*) > 1) dups
        where imp.rsc_id = dups.rsc_id
    ", Context).

run(Host, Secret, Context0) ->
    Context = z_acl:sudo(Context0),
    io:format("Deleting duplicated resources ... "),
    drop_duplicates(Context),
    io:format("~nReimporting from ~p ... ", [Host]),
    reimport(Host, Secret, Context).


reimport(Host, Secret, Context) ->
    AnyIds = z_db:q("select distinct anymeta_id, host 
                     from import_anymeta_duplicates
                     order by anymeta_id", Context),
    lists:foreach(fun({AnyId, HostOriginal}) ->
                    io:format("~p ", [AnyId]),
                    reimport(AnyId, Host, z_convert:to_list(HostOriginal), Secret, Context)
                  end,
                  AnyIds).

reimport(AnyId, Host, HostOriginal, Secret, Context) ->
    Opt = #opt{
        host=Host,
        host_original=HostOriginal,
        secret=Secret,
        from=AnyId,
        to=AnyId
    },
    case mod_import_anymeta:import_single(Opt, Context) of
        {ok, Thing} ->
            % Import referring edges
            reimport_referring(Opt, AnyId, proplists:get_value(<<"all_incoming">>, Thing), Context);
        {error, _} = Error ->
            % Error, skip to next
            Error
    end.

reimport_referring(_Opt, _ObjectId, [], _Context) ->
    ok;
reimport_referring(Opt, ObjectId, AnyIds, Context) ->
    lists:foreach(fun(AnyId) ->
                    mod_import_anymeta:import_referring_edge(
                                Opt, 
                                z_convert:to_integer(AnyId), ObjectId, 
                                Context)
                  end,
                  AnyIds).


drop_duplicates(Context) ->
    RscIds = z_db:q("select distinct rsc_id from import_anymeta_duplicates", Context),
    lists:foreach(
        fun({Id}) ->
            io:format("."),
            m_rsc:delete(Id, Context)
        end,
        RscIds).

