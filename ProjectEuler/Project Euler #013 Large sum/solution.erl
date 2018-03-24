-module(solution).
-export([main/0]).

main() ->
  {ok, [N]} = io:fread("", "~d"),
  L = read_data(N),
  Sum = lists:sum(L),
  S = integer_to_list(Sum),
  Result = lists:sublist(S, 10),
  io:format("~s~n", [Result]),
  true.


read_data(N) ->
  Fmt = unicode:characters_to_list(lists:duplicate(N, "~d")),
  {ok, L} = io:fread("", Fmt),
  L.
