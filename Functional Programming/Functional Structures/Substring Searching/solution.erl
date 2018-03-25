-module(solution).
-export([main/0]).

main() ->
  {ok, [T]} = io:fread("", "~d"),
  L = read_data(T),
  find(L),
  true.


find([]) ->
  true;
find([Text, Pat|T]) ->
  case kmp(Text, Pat) of
    nomatch ->
      Answer = "NO";
    _Else ->
      Answer = "YES"
  end,
  io:format("~s~n", [Answer]),
  find(T).


% Knuth-Morris-Pratt algorithm according to
% https://de.wikipedia.org/wiki/Knuth-Morris-Pratt-Algorithmus
kmp(Text, Pat) ->
  W = array_map(Pat),
  Table = kmp_prefix_table(W),
  T = array_map(Text),
  kmp_search(T, W, Table).


% prefix analyis phase
kmp_prefix_table(W) ->
  I = 0,
  N = maps:size(W),
  J = -1,
  Table = maps:put(I, J, maps:new()),
  kmp_prefix_table(W, I, N, J, Table).

kmp_prefix_table(_W, I, N, _J, Table) when I >= N ->
  Table;
kmp_prefix_table(W, I, N, J, Table) ->
  WI = maps:get(I, W),
  J1 = kmp_prefix_table2(W, WI, J, Table),
  I2 = I+1,
  J2 = J1+1,
  Table2 = maps:put(I2, J2, Table),
  kmp_prefix_table(W, I2, N, J2, Table2).

kmp_prefix_table2(_W, _WI, J, _Table) when J < 0 ->
  J;
kmp_prefix_table2(W, WI, J, Table) ->
  WJ = maps:get(J, W),
  case WJ =/= WI of
    true ->
      J2 = maps:get(J, Table),
      kmp_prefix_table2(W, WI, J2, Table);
    false ->
      J
  end.


% search phase
kmp_search(T, W, Table) ->
  M = maps:size(T),
  N = maps:size(W),
  I = 0,
  J = 0,
  kmp_search(M, N, I, J, T, W, Table).

kmp_search(M, _N, I, _J, _T, _W, _Table) when I >= M ->
  nomatch;
kmp_search(M, N, I, J, T, W, Table) ->
  TI = maps:get(I, T),
  J1 = kmp_search2(TI, W, J, Table),
  I2 = I+1,
  J2 = J1+1,
  case J2 =:= N of
    true ->
      % first match at I-N
      match;
    false ->
      kmp_search(M, N, I2, J2, T, W, Table)
  end.

kmp_search2(_TI, _W, J, _Table) when J < 0 ->
  J;
kmp_search2(TI, W, J, Table) ->
  WJ = maps:get(J, W),
  case WJ =/= TI of
    false ->
      J;
    true ->
      J2 = maps:get(J, Table),
      kmp_search2(TI, W, J2, Table)
  end.


% list to array map with first index 0
array_map(L) ->
  I = 0,
  Map = maps:new(),
  array_map(L, I, Map).

array_map([], _I, Map) ->
  Map;
array_map([H|T], I, Map) ->
  I2 = I+1,
  Map2 = maps:put(I, H, Map),
  array_map(T, I2, Map2).


read_data(N) ->
  Fmt = unicode:characters_to_list(lists:duplicate(N, "~s~s")),
  {ok, L} = io:fread("", Fmt),
  L.
