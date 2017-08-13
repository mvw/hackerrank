-module(solution).
-export([main/0]).

main() ->
  {ok, [T]} = io:fread("", "~d"),
  Fmt = string:copies(" ~d", T),
  {ok, L} = io:fread("", Fmt),
  lists:foreach(fun(N) ->
    P = p(N),
    io:format("~p~n", [P])                
  end, L).

% P(N) = 1 + 4 + 7 + 10 + 13 + ..
%      = sum 3 * I - 2
%      = 3 (sum i) - 2 (sum 1)
%      = 3 N(N+1)/2 - 2N
%      = 3 (N^2 + N)/2 - 2N
%      = N(3 N - 1)/2
p(N) -> 
  (N*(3*N-1)) div 2.
