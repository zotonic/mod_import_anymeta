%% Some sites like to have a html editable 'summary'.
%% We decided to call that 'introduction_html', as the 

-module(import_anymeta_intro).

-export([summary2intro/1]).


summary2intro(Context0) ->
    Context = z_acl:sudo(Context0),
    io:format("Fetching ids..."),
    Ids = z_db:q("select id from rsc order by id", Context),
    io:format("~nChecking ~p resources... ", [length(Ids)]),
    lists:map(
        fun({Id}) ->
            z_pivot_rsc:pivot_delay(Context),
            case maybe_move_summary(Id, Context) of
                true -> io:format("x");
                false -> io:format(".")
            end
        end,
        Ids),
    io:format("~nDone.~n"),
    ok.

maybe_move_summary(Id, Context) ->
    SummaryHtml = m_rsc:p_no_acl(Id, summary_html, Context),
    case is_non_p_html(SummaryHtml) of
        true ->  do_move_summary(Id, Context);
        false -> false
    end.

do_move_summary(Id, Context) ->
    Body = m_rsc:p_no_acl(Id, body, Context),
    {BlockBody, BlockOther} = case m_rsc:p_no_acl(Id, blocks, Context) of
        undefined -> {[], undefined};
        [] -> {[], undefined};
        {rsc_list, []} -> {[], undefined};
        Blocks when is_list(Blocks) ->
            lists:partition(
                fun(B) -> proplists:get_value(name, B) =:= <<"body">> end, 
                Blocks);
        FunkyBlocks ->
            io:format("~nRemoving illegal 'blocks' property in rsc ~p: ~p~n",
                      [Id, FunkyBlocks]),
            {[], []}
    end,
    NewBody = case BlockBody of
        [] -> undefined;
        [Block] -> proplists:get_value(body, Block)
    end,
    NewProps = [
        {summary, z_html:strip(Body)},
        {summary_html, undefined},
        {introduction_html, strip_class(Body)},
        {body, NewBody},
        {blocks, BlockOther}
    ],
    {ok, _} = m_rsc:update(Id, NewProps, [no_touch, {escape_texts, false}], Context),
    io:format("x"),
    true.


strip_class(undefined) ->
    undefined;
strip_class({trans, Tr}) ->
    {trans, lists:map(
        fun({Iso,Text}) ->
            {Iso, strip_class(Text)}
        end,
        Tr)};
strip_class(Text) when is_binary(Text) ->
    binary:replace(Text, <<"p class=\"summary\"">>, <<"p">>, [global]).


is_non_p_html(undefined) -> false;
is_non_p_html(<<>>) -> false;
is_non_p_html([]) -> false;
is_non_p_html({trans, Tr}) -> lists:any(fun is_non_p_html/1, Tr);
is_non_p_html({_, V}) -> is_non_p_html(V);
is_non_p_html(V) when is_list(V); is_binary(V) ->
    case re:run(V, <<"<[^/p]">>) of
        nomatch -> false;
        {match, _} -> true
    end;
is_non_p_html(_) -> false.
