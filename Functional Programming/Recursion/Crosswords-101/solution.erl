-module(solution).
-export([main/0]).

main() ->
  {G, W} = read_data(),
  Result = solve(G, W),
  write_grid(Result),
  true.


solve(G, W) ->
  {V, H} = boxes(G),
  %io:format("V=~p H=~p~n", [V, H]),

  X = crossings(V, H),
  %io:format("X=~p~n", [X]),
  
  LS = solve(W, V, H, X),
  %io:format("LS=~p~n", [LS]),
  
  % pick first solution
  [LS1|_T] = LS,
  
  N = length(LS1),
  MapLS = to_array_map(N, LS1),
  Boxes = V ++ H,
  MapBoxes = to_array_map(N, Boxes),
  lists:foldl(fun(Index, GS) ->
    S = maps:get(Index, MapLS),
    Box = maps:get(Index, MapBoxes),
    {Dir, I, J, _Len} = Box,
    case Dir of
      v ->
        update_vert(S, I, J, GS);
      h ->
        update_horiz(S, I, J, GS)
    end
  end, G, lists:seq(1, N)).


update_vert(S, I0, J0, G0) ->
  {_IF, GF} =
  lists:foldl(fun(C, {I, G}) ->
    GI = maps:get(I, G),
    GI2 = maps:update(J0, C, GI),
    G2 = maps:update(I, GI2, G),
    {I+1, G2}            
  end, {I0, G0}, S),
  GF.


update_horiz(S, I0, J0, G0) ->
  GI = maps:get(I0, G0),
  {_JF, GF} =
  lists:foldl(fun(C, {J, G}) ->
    G2 = maps:update(J, C, G),
    {J+1, G2}            
  end, {J0, GI}, S),
  maps:update(I0, GF, G0).


solve(W, V, H, X) ->
  %io:format("W=~p~n", [W]),
  Boxes = V ++ H,
  %io:format("Boxes=~p~n", [Boxes]),
  N = length(W),
  perms(W, Boxes, N, X).

perms([], _Boxes, _N, _X) ->
  [[]];
perms(L, Boxes, N, X) ->
  [HB|TB] = Boxes,
  [[H|T] || H <- L, T <- perms(L--[H], TB, N, X), test(H, HB), test(H, T, HB, TB, N, X)].


test(Word, Box) ->
  WordLen = length(Word),
  {_Dir, _I, _J, BoxLen} = Box,
  WordLen =:= BoxLen.


test(H, T, HB, TB, N, X) ->
  case 1+length(T) =:= N of
    false ->
      true;
    true ->
      LW = [H|T],
      LB = [HB|TB],
      MapW = to_array_map(N, LW),
      RevB = reverse_map(LB),
      lists:foldl(fun(Crossing, OK) ->
        {VBox, HBox, VP, HP} = Crossing,
        case OK andalso maps:is_key(VBox, RevB) andalso maps:is_key(HBox, RevB) of
          false ->
            OK;
          true ->
            IndexV = maps:get(VBox, RevB),
            IndexH = maps:get(HBox, RevB),
            WV = maps:get(IndexV, MapW),
            WH = maps:get(IndexH, MapW),
            CV = lists:nth(VP, WV),
            CH = lists:nth(HP, WH),
            CV =:= CH
        end
      end, true, X)
  end.


reverse_map(L) ->
  reverse_map(L, 1, maps:new()).

reverse_map([], _I, Map) ->
  Map;
reverse_map([H|T], I, Map) ->
  Map2 = maps:put(H, I, Map),
  reverse_map(T, I+1, Map2).


% list of crossings between a vertical and a horizontal box
crossings([], _H) ->
  [];
crossings(_V, []) ->
  [];
crossings(V, H) ->
  [X || VBox <- V, HBox <- H, X <- crossing(VBox, HBox), is_tuple(X)].

crossing(VBox, HBox) ->
  {v, VI, VJ, VLen} = VBox,
  {h, HI, HJ, HLen} = HBox,
  case (HI >= VI) andalso 
       (VJ >= HJ) andalso 
       (HI+1 =< VI+VLen) andalso 
       (VJ+1 =< HJ+HLen) of
    false ->
      [none];
    true ->
      VP = HI-VI+1,
      HP = VJ-HJ+1,
      [{VBox, HBox, VP, HP}]
  end.


% determine the word boxes {i, j, length}
boxes(G) ->
  V = box_filter(vboxes(G)),
  H = box_filter(hboxes(G)),
  {V, H}.


vboxes(G) ->
  lists:foldl(fun(J, LJ) ->
    {_State2, LJ2} =            
    lists:foldl(fun(I, {State, LI}) ->
      GI = maps:get(I, G),
      GIJ = maps:get(J, GI),
      step(v, I, J, GIJ, State, LI)
    end, {scan, LJ}, lists:seq(1, 10)),
    LJ2
  end, [], lists:seq(1, 10)).


hboxes(G) ->
  lists:foldl(fun(I, LI) ->
    GI = maps:get(I, G),
    {_State2, LI2} =            
    lists:foldl(fun(J, {State, LJ}) ->
      GIJ = maps:get(J, GI),
      step(h, I, J, GIJ, State, LJ)
    end, {scan, LI}, lists:seq(1, 10)),
    LI2
  end, [], lists:seq(1, 10)).


step(Dir, I, J, GIJ, State, L) ->
  case {State, GIJ} of
    {scan, $+} ->
      {scan, L};
    {scan, $-} ->
      Box = {Dir, I, J, 1},
      L2 = [Box|L],
      {add, L2};
    {add, $+} ->
      {scan, L};
    {add, $-} ->
      [Box|T] = L,
      {Dir, StartI, StartJ, Len} = Box,
      Box2 = {Dir, StartI, StartJ, Len+1},
      L2 = [Box2|T],
      {add, L2}
  end.


box_filter(L) ->
  lists:filter(fun(Box) ->
    {_Dir, _StartI, _StartJ, Len} = Box,
    Len > 2
  end, L).


write_grid(G) ->
  lists:foreach(fun(I) ->
    GI = maps:get(I, G),
    lists:foreach(fun(J) ->
      GIJ = maps:get(J, GI),
      io:format("~c", [GIJ])
    end, lists:seq(1, 10)),
    io:format("~n")
  end, lists:seq(1, 10)).


read_data() ->
  G = read_grid(),
  W = read_words(),
  {G, W}.


% map i -> map j -> g_ij
read_grid() ->
  Fmt = unicode:characters_to_list(lists:duplicate(10, "~s")),
  {ok, L} = io:fread("", Fmt),
  L2 = lists:foldl(fun(S, LS) ->
    AS = to_array_map(length(S), S),                     
    [AS|LS]
  end, [], L),
  to_array_map(10, lists:reverse(L2)).


% map i -> w_i
read_words() ->
  {ok, [S]} = io:fread("", "~s"),
  string:lexemes(S, [$;]).


% return map of list items
to_array_map(N, L) ->
  Index = lists:seq(1, N),
  maps:from_list(lists:zip(Index, L)).
