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
  eval(Ops, RMQ, N, Primes, Vectors).


% evaluate queries
eval([], _RMQ, _N, _Primes, _Vectors) ->
  true;
eval([[$Q], L, R|T], RMQ, N, Primes, Vectors) ->
  V = q(RMQ, L, R),
  M = vector_to_int(V, Primes),
  M2 = M rem p(),
  io:format("~p~n", [M2]),
  eval(T, RMQ, N, Primes, Vectors);
eval([[$U], Idx, Val|T], RMQ, N, Primes, Vectors) ->
  RMQ2 = u(RMQ, Idx, Val, N, Vectors),
  %io:format("RMQ=~p~n", [RMQ2]),
  eval(T, RMQ2, N, Primes, Vectors).


% big prime (for a finite field from 0 to P-1)
p() ->
  1000000007.


% query operation
% find the least common multiple of the numbers in A[L, R] (mod P)
q(RMQ, L, R) ->
  rmq_lcm(L, R, RMQ).


% update operation
u(RMQ, Idx, Val, N, Vectors) ->
  V = maps:get(Val, Vectors),
  VI = maps:get({Idx, 0}, RMQ),
  VI2 = mult(V, VI),
  RMQ2 = maps:update({Idx, 0}, VI2, RMQ),
  rmq_j(N, RMQ2).


% based on my solution to Range Minimum Query:

% determine lcm for range query
rmq_lcm(L, R, RMQ) ->
  {K, TwoK} = k(L, R),
  V1 = maps:get({L, K}, RMQ),
  V2 = maps:get({R-TwoK+1, K}, RMQ),
  lcm(V1, V2).


% k = floor(log_2(R-L+1))
k(L, R) ->
  K = -1,
  TwoK = 1,
  W = R-L+1,
  k(K, TwoK, W).

k(K, TwoK, W) when TwoK > W ->
  {K, TwoK div 2};
k(K, TwoK, W) ->
  k(K+1, 2*TwoK, W).


% calculate Range Maximum Query data structure M_ij
rmq(A, Vectors) ->
  N = maps:size(A),
  M0 = rmq_0(N, A, Vectors),
  rmq_j(N, M0).

% initialize for intervals of length 2^0
rmq_0(N, A, Vectors) ->
  I = 0,
  M = maps:new(),                 
  rmq_0(I, N, A, Vectors, M).

rmq_0(I, N, _A, _Vectors, M) when I =:= N ->
  M;
rmq_0(I, N, A, Vectors, M) ->
  AI = maps:get(I, A),
  VI = maps:get(AI, Vectors),
  M2 = maps:put({I, 0}, VI, M),
  rmq_0(I+1, N, A, Vectors, M2).

% calculate bigger intervals
rmq_j(N, M) ->
  J = 1,
  TwoJ = 2,
  rmq_j(J, TwoJ, N, M).

rmq_j(_J, TwoJ, N, M) when TwoJ > N ->
  M;
rmq_j(J, TwoJ, N, M) ->
  I = 0,
  IMax = I+TwoJ-1,
  M2 = rmq(I, IMax, J, TwoJ, N, M),
  rmq_j(J+1, 2*TwoJ, N, M2).

rmq(_I, IMax, _J, _TwoJ, N, M) when IMax =:= N ->
  M;
rmq(I, IMax, J, TwoJ, N, M) ->
  JM = J-1,
  I2 = I+(1 bsl JM),
  V1 = maps:get({I, JM}, M),
  V2 = maps:get({I2, JM}, M),
  MIJ = lcm(V1, V2),
  M2 = maps:put({I, J}, MIJ, M),
  rmq(I+1, IMax+1, J, TwoJ, N, M2).


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


% from my solution to Project Euler #10 Summation of primes:

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
