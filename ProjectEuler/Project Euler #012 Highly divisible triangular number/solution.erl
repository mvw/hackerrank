-module(solution).
-export([main/0]).

main() ->
  {ok, [T]} = io:fread("", "~d"),
  L = read_data(T),
  S = lists:sort(L),
  Tri = triangle(S),
  lists:foreach(fun(N) ->
    X = maps:get(N, Tri),
    io:format("~p~n", [X])
  end, L),
  true.


% find the wanted triangle numbers accoording
% to the sorted list S
triangle(S) ->
  I = 1,
  X = 1,
  Primes = primes(2),
  Map = maps:new(),
  triangle(S, I, X, Primes, Map).

triangle([], _I, _X, _Primes, Map) ->
  Map;
triangle(S, I, X, Primes, Map) ->
  [N|T] = S,
  {ND, Primes2} = divisors(X, Primes),
  case ND > N of
    true ->
      % found triangle number X
      Map2 = maps:put(N, X, Map),
      triangle(T, I, X, Primes2, Map2);
    false ->
      % try next 
      I2 = I+1,
      X2 = X+I2,
      triangle(S, I2, X2, Primes2, Map)
  end.


% count the divisors of X
divisors(X, Primes) ->
  I = 1,
  P = 2,
  LP = [],
  divisors(X, I, P, Primes, LP).

divisors(X, _I, P, Primes, LP) when P > X ->
  % no more divisors, finish
  ND = nd(LP),
  {ND, Primes};
divisors(X, I, P, Primes, LP) when X rem P =:= 0->
  % found new divisor P
  divisors(X div P, I, P, Primes, [P|LP]);
divisors(X, I, P, Primes, LP) ->
  % P is not working, try next prime
  {I2, P2, Primes2} = next(I, P, Primes),
  divisors(X, I2, P2, Primes2, LP).


% turn list of prime divisors into number of divisors
nd(LP) ->
  P = none,
  E = 0,
  ND = 1,
  nd(LP, P, E, ND).

nd([], _P, E, ND) ->
  (E+1)*ND;
nd([H|T], P, E, ND) when H =/= P ->
  nd(T, H, 1, (E+1)*ND);
nd([P|T], P, E, ND) ->
  nd(T, P, E+1, ND).


% initial primes map
primes(0) ->
  maps:new();
primes(1) ->
  maps:put(1, 2, primes(0));
primes(2) ->
  maps:put(2, 3, primes(1)).


% at the I-th prime P, get the next prime
next(I, P, Primes) ->
  I2 = I+1,
  case I2 =< maps:size(Primes) of
    true ->
      P2 = maps:get(I2, Primes),
      {I2, P2, Primes};
    false ->
      X = P+2,
      primes(I2, X, Primes)
  end.


% discover the I-th prime, start checking at X
primes(I, X, Primes) ->
  case is_prime(X, Primes) of
    true ->
      Primes2 = maps:put(I, X, Primes),
      {I, X, Primes2};
    false ->
      primes(I, X+2, Primes)
  end.


% is X a prime number?
is_prime(X, Primes) ->
  I = 1,
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


read_data(N) ->
  Fmt = unicode:characters_to_list(lists:duplicate(N, "~d")),
  {ok, L} = io:fread("", Fmt),
  L.
