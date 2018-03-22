-module(solution_slow).
-export([main/0]).

% Erlang can handle looooong numbers

main() ->
  {ok, [N]} = io:fread("", "~d"),
  L = read_list(N),
  S = lists:sort(L),
  lists:foreach(fun(I) ->
    io:format("~p~n", [I])
  end, S),
  true.

read_list(N) ->
  Fmt = unicode:characters_to_list(lists:duplicate(N, "~d")),
  {ok, L} = io:fread("", Fmt),
  L.
