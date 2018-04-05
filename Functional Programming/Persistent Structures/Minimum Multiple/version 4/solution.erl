-module(solution).
-export([main/0]).

% The a_i and val are from S={1,..,100}
% S contains 25 prime numbers
% We use the fundamental theorem of arithmetic,
%   x = prod_1^25 P_i^e_i,
% representing each number x as vector of exponents (e_i) in N^25
% Then:
%   mult(x, y) -> (e_i + f_i)
%   lcm(x, y) -> (max(e_i, f_i))
%   lcm(a[L..R]) -> Range Maximum Query problem

main() ->
  {ok, [N]} = io:fread("", "~d"),
  L = read_list(N),
  {ok, [K]} = io:fread("", "~d"),
  Ops = read_ops(K),
  A = array(L),
  Primes = primes(100),
  Vectors = vectors(Primes),
  RMQ = rmq(A, Vectors),
  %io:format("RMQ=~p~n", [RMQ]),
  eval(Ops, RMQ, Primes, Vectors).


% evaluate queries
eval([], _RMQ, _Primes, _Vectors) ->
  true;
eval([[$Q], L, R|T], RMQ, Primes, Vectors) ->
  V = q(RMQ, L, R),
  M = vector_to_int(V, Primes),
  M2 = M rem p(),
  io:format("~p~n", [M2]),
  eval(T, RMQ, Primes, Vectors);
eval([[$U], Idx, Val|T], RMQ, Primes, Vectors) ->
  RMQ2 = u(RMQ, Idx, Val, Vectors),
  %io:format("RMQ=~p~n", [RMQ2]),
  eval(T, RMQ2, Primes, Vectors).


% big prime (for a finite field from 0 to P-1)
p() ->
  1000000007.


% query operation
% find the least common multiple of the numbers in A[L, R] (mod P)
q(RMQ, L, R) ->
  rmq_lcm(L, R, RMQ).


% update operation
u(RMQ, _Idx, 1, _Vectors) ->
  RMQ;
u(RMQ, Idx, Val, Vectors) ->
  VF = maps:get(Val, Vectors),
  {Parent, none, none, VI} = maps:get(Idx, RMQ),
  VI2 = mult(VF, VI),
  RMQ2 = maps:update(Idx, {Parent, none, none, VI2}, RMQ),
  update(Parent, RMQ2).


update(none, RMQ) ->
  RMQ;
update(Node, RMQ) ->
  {Parent, ChildLeft, ChildRight, V} = maps:get(Node, RMQ),
  {_, _, _, VLeft} = maps:get(ChildLeft, RMQ),
  {_, _, _, VRight} = maps:get(ChildRight, RMQ),
  V2 = lcm(VLeft, VRight),
  case V2 =:= V of
    true ->
      RMQ;
    false ->
      RMQ2 = maps:update(Node, {Parent, ChildLeft, ChildRight, V2}, RMQ),
      update(Parent, RMQ2)
  end.


% determine lcm for range query
rmq_lcm(L, R, RMQ) ->
  Root = maps:get(root, RMQ),
  rmq_lcm(Root, L, R, RMQ).

rmq_lcm(Node, L, R, RMQ) ->
  case is_inside(Node, L, R) of
    true ->
      {_, _, _, V} = maps:get(Node, RMQ),
      V;
    false ->
      case is_outside(Node, L, R) of
        true ->
          % neutral element regarding LCM is 1
          [];
        false ->
          {ChildLeft, ChildRight} = split(Node),
          VLeft = rmq_lcm(ChildLeft, L, R, RMQ),
          VRight = rmq_lcm(ChildRight, L, R, RMQ),
          lcm(VLeft, VRight)
      end
  end.


is_inside(Element, L, R) when is_integer(Element) ->
  (Element >= L) andalso (Element =< R);
is_inside(Interval, L, R) ->
  {Left, Right} = Interval,
  (Left >= L) andalso (Right =< R).


is_outside(Element, L, R) when is_integer(Element) ->
  (Element < L) orelse (Element > R);
is_outside(Interval, L, R) ->
  {Left, Right} = Interval,
  (Right < L) orelse (Left > R).


% calculate Range Maximum Query data structure
% this solution builds a segment tree according to
% https://www.geeksforgeeks.org/segment-tree-set-1-range-minimum-query/
rmq(A, Vectors) ->
  N = maps:size(A),
  Left = 0,
  Right = N-1,
  Node = key(Left, Right),
  Parent = none,
  RMQ0 = maps:new(),
  RMQ = maps:put(root, Node, RMQ0),
  rmq(Node, Parent, A, Vectors, RMQ).

rmq(Element, Parent, A, Vectors, RMQ) when is_integer(Element) ->
  % solve
  Value = maps:get(Element, A),
  V = maps:get(Value, Vectors),
  maps:put(Element, {Parent, none, none, V}, RMQ);
rmq(Interval, Parent, A, Vectors, RMQ) ->
  % divide
  {ChildLeft, ChildRight} = split(Interval),
  RMQ1 = rmq(ChildLeft, Interval, A, Vectors, RMQ),
  RMQ2 = rmq(ChildRight, Interval, A, Vectors, RMQ1),
  % conquer
  {Interval, _, _, VLeft} = maps:get(ChildLeft, RMQ2),
  {Interval, _, _, VRight} = maps:get(ChildRight, RMQ2),
  V = lcm(VLeft, VRight),
  maps:put(Interval, {Parent, ChildLeft, ChildRight, V}, RMQ2).


split(Interval) ->
  {Left, Right} = Interval,
  Mid = Left + (Right-Left) div 2,
  ChildLeft = key(Left, Mid),
  ChildRight = key(Mid+1, Right),
  {ChildLeft, ChildRight}.


key(Left, Right) when Left =:= Right ->
  Left;
key(Left, Right) ->
  {Left, Right}.


% lcm: max per component
lcm(V1, V2) ->
  V = [],
  lcm(V1, V2, V).

lcm([], [], V) ->
  lists:reverse(V);
lcm([], [H|T], V) ->
  lcm([], T, [H|V]);
lcm([H|T], [], V) ->
  lcm(T, [], [H|V]);
lcm([H1|T1], [H2|T2], V) ->
  M = max(H1, H2),
  V2 = [M|V],
  lcm(T1, T2, V2).


% mult: add per component
mult(V1, V2) ->
  V = [],
  mult(V1, V2, V).

mult([], [], V) ->
  lists:reverse(V);
mult([], [H|T], V) ->
  mult([], T, [H|V]);
mult([H|T], [], V) ->
  mult(T, [], [H|V]);
mult([H1|T1], [H2|T2], V) ->
  M = H1+H2,
  V2 = [M|V],
  mult(T1, T2, V2).


% map of 25-vectors for 1..100
vectors(Primes) ->
  X = 1,
  Map = maps:new(),
  vectors(X, Primes, Map).

vectors(X, _Primes, Map) when X > 100 ->
  Map;
vectors(X, Primes, Map) ->
  V = vector(X, Primes),
  Map2 = maps:put(X, V, Map),
  vectors(X+1, Primes, Map2).


% calculate 25-vector for X
vector(X, Primes) ->
  I = 25,
  L = [],
  vector(I, X, Primes, L).

vector(I, _X, _Primes, L) when I =:= 0 ->
  L;
vector(I, X, Primes, L) ->
  P = maps:get(I, Primes),
  {X2, E} = shave(X, P),
  case (L =:= []) andalso (E =:= 0) of
    true ->
      % we drop leading zero digits, so [] represents 1
      L2 = L;
    false ->
      L2 = [E|L]
  end,
  vector(I-1, X2, Primes, L2).


% try to shave of the factor P from x
shave(X, P) ->
  E = 0,
  shave(E, X, P).

shave(E, X, P) when (X rem P) =/= 0 ->
  {X, E};
shave(E, X, P) ->
  shave(E+1, X div P, P).


% convert 25-vector to int
vector_to_int(V, Primes) ->
  I = 1,
  X = 1,
  vector_to_int(V, I, X, Primes).


vector_to_int([], _I, X, _Primes) ->
  X;
vector_to_int([H|T], I, X, Primes) ->
  P = maps:get(I, Primes),
  F = pow(P, H),
  X2 = F*X,
  vector_to_int(T, I+1, X2, Primes).


% simple exponentiation
pow(_X, 0) ->
  1;
pow(X, E) ->
  X * pow(X, E-1).


% code from Project Euler #10 Summation of primes:

% map of prime numbers <= n
primes(1) ->
  maps:new();
primes(2) ->
  maps:put(1, 2, primes(1));
primes(N) when N < 5 ->
  maps:put(2, 3, primes(2));
primes(N) ->
  primes(5, N, primes(3)).

primes(X, N, Primes) when X > N ->
  Primes;
primes(X, N, Primes) ->
  case is_prime(X, Primes) of
    true ->
      I_max = maps:size(Primes)+1,
      Primes2 = maps:put(I_max, X, Primes);
    false ->
      Primes2 = Primes
  end,
  primes(X+2, N, Primes2).


is_prime(X, Primes) ->
  I = 2,
  is_prime(I, X, Primes).

is_prime(I, X, Primes) ->
  P = maps:get(I, Primes),
  case P*P > X of
    true ->
      true;
    false ->
      case X rem P =:= 0 of
        true ->
          false;
        false ->
          is_prime(I+1, X, Primes)
      end
  end.


% array map starting at index 0
array(L) ->
  I=0,
  Map = maps:new(),
  array(L, I, Map).

array([], _I, Map) ->
  Map;
array([H|T], I, Map) ->
  Map2 = maps:put(I, H, Map),
  array(T, I+1, Map2).


read_ops(N) ->
  Fmt = unicode:characters_to_list(lists:duplicate(N, "~s~d~d")),
  {ok, L} = io:fread("", Fmt),
  L.


read_list(N) ->
  Fmt = unicode:characters_to_list(lists:duplicate(N, "~d")),
  {ok, L} = io:fread("", Fmt),
  L.
