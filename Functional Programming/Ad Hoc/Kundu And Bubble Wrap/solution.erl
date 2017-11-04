-module(solution).
-export([main/0]).

% See https://en.wikipedia.org/wiki/Coupon_collector%27s_problem

main() ->
  {ok, [N, M]} = io:fread("", "~d~d"),
  T = expected_time(N, M),
  io:format("~p~n", [T]),
  true.

expected_time(N, M) ->
  K = N*M,
  K * h(K).

h(1) ->
  1.0;
h(N) ->
  1.0/N + h(N-1).
