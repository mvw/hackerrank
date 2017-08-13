-module(solution).
-export([main/0]).

main() ->
  M = 100000007,
  Mem0 = array:from_list([0, 1]),
  {ok, [T]} = io:fread("", "~d"),
  lists:foldl(fun(_I, Mem) ->
    {ok, [N]} = io:fread("", "~d"),
    {Fib, Mem2} = fib_mod(N, M, Mem),
    io:format("~p~n", [Fib]),
    Mem2
  end, Mem0, lists:seq(1, T)),
  true.

% the recursion tree is too large to visit for larger N
% we retrieve from memory if we can, otherwise we 
% extend the memory step by step as needed
fib_mod(N, M, Mem) ->
  S = array:size(Mem),
  case N < S of
    true ->
      {array:get(N, Mem), Mem};
    false ->
      Fib = (array:get(S-1, Mem) + array:get(S-2, Mem)) rem M,
      Mem2 = array:set(S, Fib, Mem),
      fib_mod(N, M, Mem2)
  end.
  
