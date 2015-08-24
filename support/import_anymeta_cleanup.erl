-module(import_anymeta_cleanup).

-export([
    cleanup_tags/1,
    cleanup_kw_tag/3
    ]).

cleanup_tags(Context0) ->
    Context = z_acl:sudo(Context0),
    case m_category:name_to_id(tag, Context) of
        {ok, CatId} ->
            Ids = z_db:q("select id from rsc where category_id = $1", [CatId], Context),
            lists:foreach(fun({Id}) ->
                              case z_db:q1("select count(*) 
                                            from edge 
                                            where object_id = $1", 
                                            [Id], Context) 
                              of
                                0 ->
                                    io:format("x"),
                                    m_rsc:delete(Id, Context);
                                _ ->
                                    io:format("."),
                                    ok
                              end
                          end,
                          Ids);
        Error ->
            Error
    end.

cleanup_kw_tag(Host, Secret, Context0) ->
    Context = z_acl:sudo(Context0),
    case m_category:get_range(keyword, Context) of
        {From, To} ->
            AnyIds = z_db:q("
                        select r.id, imp.anymeta_id
                        from rsc r, import_anymeta imp
                        where r.id = imp.rsc_id
                          and r.pivot_category_nr >= $1
                          and r.pivot_category_nr <= $2",
                        [From, To],
                        Context),
            lists:foreach(fun({RscId, AnyId}) ->
                            cleanup_kw_tag_1(RscId, AnyId, Host, Secret, Context)
                          end,
                          AnyIds);
        Error ->
            Error
    end.

cleanup_kw_tag_1(RscId, AnyId, Host, Secret, Context) ->
    case m_rsc:is_a(RscId, tag, Context) of
        true ->
            io:format("."),
            ok;
        false ->
            case mod_import_anymeta:get_thing(AnyId, Host, Secret, no, Context) of
                {ok, Thing} ->
                    case proplists:get_value(<<"kind">>, Thing) of
                        <<"TAG">> ->
                            io:format("+"),
                            m_rsc_update:update(RscId, [{category,tag}], Context);
                        _ ->
                            io:format("."),
                            ok
                    end;
                {error, _} = Error ->
                    io:format("~nError on fetching id ~p: ~p~n", [AnyId, Error]),
                    Error
            end
    end.

