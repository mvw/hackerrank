-module(solution).
-export([main/0]).

main() ->
  {ok, [T]} = io:fread("", "~d"),
  L = read_list(T),
  lists:foreach(fun(N) ->
    S1 = (N*(N+1)) div 2,
    S2 = (N*(N+1)*(2*N+1)) div 6,
    Diff = abs(S2 - S1*S1),
    io:format("~p~n", [Diff])
  end, L),
  true.

read_list(N) ->
  Fmt = unicode:characters_to_list(lists:duplicate(N, "~d")),
  {ok, L} = io:fread("", Fmt),
  L.
