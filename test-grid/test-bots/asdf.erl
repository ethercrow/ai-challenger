#!/usr/bin/env escript

-record(world, {myid, energy, field = []}).

-mode(compile).

zip4(L1, L2, L3, L4) ->
    lists:zipwith(fun({A, B}, {C, D}) -> {A, B, C, D} end, lists:zip(L1, L2), lists:zip(L3, L4)).

readInput(#world{field = F} = W) ->
    Tokens = string:tokens(io:get_line(""), " "),
    case Tokens of
        ["\n"] -> readInput(W);
        [".\n"] -> W;
        ["E", Es] -> {E, _} = string:to_integer(Es), readInput(W#world{energy = E});
        ["Y", Id] -> readInput(W#world{myid = lists:nth(1, Id)});
        [L] -> readInput(W#world{field = F ++ [lists:droplast(L)]})
    end.

distance(X1, Y1, X2, Y2) ->
    (X1-X2)*(X1-X2) + (Y1-Y2)*(Y1-Y2).

makeColumns([H | T]) ->
    B = lists:map(fun(X) -> [X] end, H),
    lists:foldl(fun(X, A)-> lists:zipwith(fun(Z, Y) -> Z ++ [Y] end, A, X) end, B, T).

mapFunc(F, Columns) ->
    lists:flatten(lists:map(fun({N, X}) -> mapFuncSub(F, N, X) end, 
        lists:zip(lists:seq(0, length(Columns)-1), Columns))).

mapFuncSub(F, N, Line) ->
    lists:map(fun({M, C, LC, NC}) -> F(N, M, C, LC, NC) end, zip4(lists:seq(0, length(Line)-1), Line,
            [$- | lists:droplast(Line)], lists:nthtail(1, Line) ++ [$-])).

takeTurn(#world{myid = I, energy = E, field = Lines}) ->
    Columns = makeColumns(Lines),
    Enemies = mapFunc(fun(N, M, C, _, _) when (C =/= I) and (C =/= $-) -> {N, M};
                  (_, _, _, _, _) -> [] end, Columns),
    Spare = lists:usort(mapFunc(fun(N, M, C, LC, NC) when (C == $-) and ((NC == I) or (LC == I)) -> {N, M};
                              (_, _, _, _, _) -> [] end, Columns) ++ 
                        mapFunc(fun(N, M, C, LC, NC) when (C == $-) and ((NC == I) or (LC == I)) -> {M, N};
                              (_, _, _, _, _) -> [] end, Lines)),
    Avail = lists:map(fun({X1, Y1}) -> {lists:min(lists:map(fun({X2, Y2}) -> distance(X1, Y1, X2, Y2) end, Enemies)), X1, Y1} end, Spare),
    Orders = lists:sublist(lists:sort(fun({D1, _, _}, {D2, _, _}) -> D1 =< D2 end, Avail), E),
    lists:map(fun({_, X, Y}) -> io:format("C ~p ~p\n",[X, Y]) end, Orders),
    io:format(".\n").

play() ->
    World = readInput(#world{}),
    takeTurn(World),
    play().

main([]) ->
    play().