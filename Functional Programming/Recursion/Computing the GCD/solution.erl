-module(solution).
-export([main/0]).

main() ->
  {ok, [A, B]} = io:fread("", "~d ~d"),
  Gcd = gcd(A, B),
  io:format("~p~n", [Gcd]).

gcd(X, Y) when X =:= Y ->
  X;
gcd(X, Y) when X > Y ->
  gcd(X - Y, Y);
gcd(X, Y) ->
  gcd(Y, X).
