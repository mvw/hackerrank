-module(solution).
-export([main/0]).

main() ->
  {ok, [N]} = io:fread("", "~d"),
  L = read_list(N),
  Map = map(L),
  Result = search(N, Map),
  io:format("~p~n", [Result]),
  true.


search(N, Map) ->
  A = 1,
  B = N,
  search(A, B, Map).


search(A, B, Map) ->
  MinIndex = min_index(A, B, Map),
  case MinIndex > A of
    true ->
      MaxLeft = search(A, MinIndex-1, Map);
    false ->
      MaxLeft = 0
  end,
  case MinIndex < B of
    true ->
      MaxRight = search(MinIndex+1, B, Map);
    false ->
      MaxRight = 0
  end,
  Mid = (B-A+1) * maps:get(MinIndex, Map),
  max(max(MaxLeft, MaxRight), Mid).


min_index(A, B, Map) ->
  MinIndex = A,
  Min = maps:get(A, Map),
  min_index(A+1, B, MinIndex, Min, Map).


min_index(A, B, MinIndex, _Min, _Map) when A > B ->
  MinIndex;
min_index(A, B, MinIndex, Min, Map) ->
  Value = maps:get(A, Map),
  case Value < Min of
    true ->
      MinIndex2 = A,
      Min2 = Value;
    false ->
      MinIndex2 = MinIndex,
      Min2 = Min
  end,
  min_index(A+1, B, MinIndex2, Min2, Map).


map(L) ->
  map(L, 1, maps:new()).

map([], _Index, Map) ->
  Map;
map([H|L], Index, Map) ->
  Map2 = maps:put(Index, H, Map),
  map(L, Index+1, Map2).


read_list(N) ->
  Fmt = unicode:characters_to_list(lists:duplicate(N, "~d")),
  {ok, L} = io:fread("", Fmt),
  L.
