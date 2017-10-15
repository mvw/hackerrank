-module(solution).
-export([main/0]).

main() ->
  {ok, [T]} = io:fread("", "~d"),
  lists:foldl(fun(_I, Mem) ->
    {ok, [N]} = io:fread("", "~d"),
    {Number, Mem2} = b(N, Mem),
    Result = Number rem p(),
    io:format("~p~n", [Result]),
    Mem2
  end, maps:new(), lists:seq(1, T)),
  true.


% For a set 1..N we can pick the top node, 
% e.g. set 1..5 and top node 2.
% This splits the set into 1 < 2 < 3 4 5
% so we have the b(1) left subtrees times b(3) right subtrees
% for this case.
% Here are for 1..5 and 1..4 the splits into left and right subtree numbers
%   L|R  L|R
%   0|4  0|3
%   1|3  1|2
%   2|2  2|1
%   3|1  3|0
%   4|0
b(0, Mem) ->
  % one choice:the empty tree
  {1, Mem};
b(1, Mem) ->
  {1, Mem};
b(N, Mem) ->
  case maps:is_key(N, Mem) of
    true ->
      {maps:get(N, Mem), Mem};
    false ->
      {B, Mem2} = b2(N, Mem),
      {B, maps:put(N, B, Mem2)}
  end.

b2(N, Mem) ->
  Last = N-1,
  lists:foldl(fun(I, {SumI, MemI}) ->
    {B1, Mem2} = b(I, MemI),
    {B2, Mem3} = b(Last-I, Mem2),
    {SumI+B1*B2, Mem3}
  end, {0, Mem}, lists:seq(0, Last)).


p() ->
  100000007.
