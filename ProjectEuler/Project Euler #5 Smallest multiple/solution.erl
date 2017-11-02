-module(solution).
-export([main/0]).

main() ->
  {ok, [T]} = io:fread("", "~d"),
  L = read_list(T),
  lists:foreach(fun(N) ->
    X = lcm(N),
    io:format("~p~n", [X])
  end, L),
  true.


lcm(N) ->
  lists:foldl(fun(K, LCM) ->
    lcm(LCM, K)
  end, 1, lists:seq(1, N)).


% lcm(a, b) = |a b| / gcd(a,b) = (|a| / gcd(a,b)) |b|
lcm(X, Y) when Y > X ->
  lcm(Y, X);
lcm(X, Y) ->
  ((X div gcd(X, Y)) * Y).


gcd(A, B) when B =:= 0 ->
  A;
gcd(A, B) ->
  gcd(B, A rem B).


read_list(N) ->
  Fmt = unicode:characters_to_list(lists:duplicate(N, "~d")),
  {ok, L} = io:fread("", Fmt),
  L.
