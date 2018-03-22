-module(solution_slow_tc).
-export([main/0]).

% Erlang can handle looooong numbers

main() ->
  T0 = erlang:timestamp(),
  {ok, [N]} = io:fread("", "~d"),
  L = read_list(N),
  T1 = erlang:timestamp(),
  S = lists:sort(L),
  T2 = erlang:timestamp(),
  lists:foreach(fun(I) ->
    io:format("~p~n", [I])
  end, S),
  T3 = erlang:timestamp(),
  M = 1000*1000,
  io:format("read : ~p~n", [timer:now_diff(T1, T0)/M]),
  io:format("sort : ~p~n", [timer:now_diff(T2, T1)/M]),
  io:format("write: ~p~n", [timer:now_diff(T3, T2)/M]),
  true.

read_list(N) ->
  Fmt = unicode:characters_to_list(lists:duplicate(N, "~d")),
  {ok, L} = io:fread("", Fmt),
  L.
