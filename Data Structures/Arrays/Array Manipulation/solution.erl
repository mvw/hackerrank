-module(solution).
-export([main/0]).

% This approach from the discussion
% quasi works on the derivative 
% and then integrates the solution

main() ->
  {ok, [_N, M]} = io:fread("", "~d~d"),
  L = read_ops(M),
  D = setup(L),
  Maximum = get_max(D),
  io:format("~p~n", [Maximum]),
  true.


setup(L) ->
  D = maps:new(),
  setup(L, D).

setup([], D) ->
  D;
setup([A, B, K|T], D) ->
  D2 = modify(A, K, D),
  D3 = modify(B+1, -K, D2),
  setup(T, D3).


modify(I, K, D) ->
  case maps:is_key(I, D) of
    false ->
      maps:put(I, K, D);
    true ->
      Old = maps:get(I, D),
      maps:update(I, Old+K, D)
  end.


% Optimization: skip known gaps: get map keys, sort keys, sum over those
get_max(D) ->
  K = maps:keys(D),
  L = lists:sort(K),
  {_S, Max} = 
  lists:foldl(fun(I, {SI, MaxI}) ->
    SI2 = SI + maps:get(I, D),
    MaxI2 = max(MaxI, SI2),
    {SI2, MaxI2}
  end, {0, 0}, L),
  Max.


read_ops(N) ->
  Fmt = string:copies("~d~d~d", N),
  {ok, L} = io:fread("", Fmt),
  L.
