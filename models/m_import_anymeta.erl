-module(m_import_anymeta).

-export([
    m_find_value/3,
    m_to_list/2,
    m_value/2
    ]).

-include("zotonic.hrl").

m_find_value(host, #m{}, Context) ->
    host(Context);
m_find_value(_, #m{}, _Context) ->
    undefined.

m_to_list(#m{}, _Context) ->
    [].

m_value(#m{}, _Context) ->
    undefined.



host(Context) ->
    case z_db:q_row("
        select host, count(*)
        from import_anymeta 
        group by host 
        order by count(*) desc
        limit 1",
        Context)
    of
        {Host, _Count} ->
            Host;
        _ ->
            undefined
    end.
