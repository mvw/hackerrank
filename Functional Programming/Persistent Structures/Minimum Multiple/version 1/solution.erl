-module(solution).
-export([main/0]).

main() ->
  {ok, [N]} = io:fread("", "~d"),
  %io:format("N=~p~n", [N]),
  L = read_line(N),
  A = list_to_map(L),
  %io:format("A=~w~n", [A]),
  {ok, [K]} = io:fread("", "~d"),
  %io:format("K=~p~n", [K]),
  Ops = read_ops(K),
  %io:format("Ops=~p~n", [Ops]),
  eval(Ops, A).


% eval(Ops, A):
% work through the list of operations, acting on the map with the array A
% e.g Ops=["Q",0,4,"U",1,2,"Q",0,2,"Q",3,4,"Q",2,4,"U",3,8,"Q",2,3]
eval([], _A) ->
  true;
eval(["Q", Arg1, Arg2|T], A) ->
  Val = q(A, Arg1, Arg2),
  io:format("~p~n", [Val]),
  eval(T, A);
eval(["U", Arg1, Arg2|T], A) ->
  A2 = u(A, Arg1, Arg2),
  eval(T, A2).
                              

% big prime (for a finite field from 0 to P-1)
p() ->
  1000000007.


% query operation
% find the least common multiple of the numbers in A[L, R] (mod P)
q(A, L, R) ->
  lcm(A, L, R) rem p().

lcm(A, I, I) ->
  maps:get(I, A);
lcm(A, L, R) ->
  A1 = maps:get(L, A),
  A2 = maps:get(L+1, A),
  Lcm = lcm(A1, A2),
  lcm(A, L+2, R, Lcm).

lcm(_, L, R, Lcm) when L > R ->
  Lcm;
lcm(A, L, R, Lcm) ->
  AL = maps:get(L, A),
  Lcm2 = lcm(Lcm, AL),
  lcm(A, L+1, R, Lcm2).


% lcm(a, b) = |a b| / gcd(a,b) = (|a| / gcd(a,b)) |b|
lcm(X, Y) when Y > X ->
  lcm(Y, X);
lcm(X, Y) ->
  ((X div gcd(X, Y)) * Y).


gcd(A, B) when B =:= 0 ->
  A;
gcd(A, B) ->
  gcd(B, A rem B).


% update operation
u(A, Idx, Val) ->
  AI = maps:get(Idx, A),
  maps:update(Idx, AI * Val, A).


list_to_map(L) ->
  list_to_map(L, 0, maps:new()).

list_to_map([], _Index, Map) ->
  Map;
list_to_map([H|T], Index, Map) ->
  list_to_map(T, Index+1, maps:put(Index, H, Map)).


read_ops(N) ->
  Fmt = string:copies("~s~d~d ", N),
  {ok, L} = io:fread("", Fmt),
  L.

read_line(N) ->
  Fmt = string:copies("~d", N),
  {ok, L} = io:fread("", Fmt),
  L.
