-module(solution).
-export([main/0]).

main() ->
  Primes = get_primes(),
  %io:format("Primes=~w~n", [Primes]),
	{ok, [N]} = io:fread("", "~d"),
  %io:format("N=~p~n", [N]),
  L = read_line(N),
  %io:format("L=~w~n", [L]),
  A = list_to_map(Primes, L),
  %io:format("A=~w~n", [A]),
	{ok, [K]} = io:fread("", "~d"),
  %io:format("K=~p~n", [K]),
  Ops = read_ops(K),
  %io:format("Ops=~p~n", [Ops]),
  eval(Primes, Ops, A).


% eval(Ops, A):
% work through the list of operations, acting on the map with the array A
% e.g Ops=["Q",0,4,"U",1,2,"Q",0,2,"Q",3,4,"Q",2,4,"U",3,8,"Q",2,3]
eval(_Primes, [], _A) ->
  true;
eval(Primes, ["Q", Arg1, Arg2|T], A) ->
  Val = q(Primes, A, Arg1, Arg2),
  io:format("~p~n", [Val]),
  eval(Primes, T, A);
eval(Primes, ["U", Arg1, Arg2|T], A) ->
  A2 = u(Primes, A, Arg1, Arg2),
  eval(Primes, T, A2).
                              

% big prime (for a finite field from 0 to P-1)
p() ->
  1000000007.


% query operation
% find the least common multiple of the numbers in A[L, R] (mod P)
q(Primes, A, L, R) ->
  Lcm = lcm(A, L, R),
  to_integer(Primes, Lcm).


% lcm of the array elements from index L to R
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


% lcm calculation via the lcm(a,b) = |a b| / gcd(a,b) 
% is too slow for test case 11 and above :-(

% Now trying prime factorization
% a=prod p^(a_i), b=prod p^(b_i) => lcm(a,b) = prod p^max(a_i, b_i)

lcm(LA, LB) ->
  lcm2(LA, LB, []).

lcm2([], [], Result) ->
  lists:reverse(Result);
lcm2([HA|TA], [HB|TB], Result) ->
  lcm2(TA, TB, [max(HA, HB)|Result]).


% update operation
u(Primes, A, Idx, Val) ->
  LA = maps:get(Idx, A),
  LB = to_prime_list(Primes, Val),
  LC = mult(LA, LB),
  maps:update(Idx, LC, A).


% multiply two prime lists
mult(LA, LB) ->
  mult2(LA, LB, []).

mult2([], [], Result) ->
  lists:reverse(Result);
mult2([HA|TA], [HB|TB], Result) ->
  mult2(TA, TB, [HA+HB|Result]).


% extend list if needed to 25 components
extend(L) ->
  N = length(L),
  extend2(N, L).

extend2(25, L) ->
  L;
extend2(N, L) ->
  extend2(N+1, L ++ [0]).


% convert prime list to integer (mod P)
to_integer(Primes, L) ->
  to_integer(Primes, L, 1).

to_integer(_Primes, [], Result) ->
  Result;
to_integer([HP|TP], [H|T], Result) ->
  D = pow(HP, H),
  Result2 = (Result * D) rem p(),
  to_integer(TP, T, Result2).


% modular exponentiation
pow(_I, 0) ->
  1;
pow(I, E) ->
  (I * pow(I, E-1)) rem p().


% convert a positive integer to a list of prime exponents
to_prime_list(_Primes, 1) ->
  extend([0]);
to_prime_list(Primes, I) ->
  to_prime_list(Primes, I, []).

to_prime_list(_Primes, 1, Result) ->
  L = lists:reverse(Result),
  extend(L);
to_prime_list([], _I, _Result) ->
  out_of_primes;
to_prime_list([P|TPrimes], I, Result) ->
  {E, I2} = prime_exponent(I, P),
  to_prime_list(TPrimes, I2, [E|Result]).


% determine exponent of prime P in prime factorization of I
prime_exponent(I, P) ->
  prime_exponent(I, P, 0).

prime_exponent(1, _P, E) ->
  {E, 1};
prime_exponent(I, P, E) ->
  case I rem P =:= 0 of
    false ->
      {E, I};
    true ->
      prime_exponent(I div P, P, E+1)
  end.


% get the primes within {2, .., Max} as ascending list
get_primes() ->
  Max = 97,
  lists:foldl(fun(D, Primes) ->
                case is_prime(D) of
                  false ->
                    Primes;
                  true ->
                    [D|Primes]
                end
              end, [], lists:seq(Max, 2, -1)).

% test if I is prime
is_prime(1) ->
  false;
is_prime(I) when I =:= 2; I =:= 3; I =:= 5; I =:= 7 -> 
  true;
is_prime(I) when I rem 2 =:= 0; I rem 3 =:= 0; I rem 5 =:= 0; I rem 7 =:= 0 ->
  false;
is_prime(I) -> is_prime(I, 11).

% test if I is prime by trying all divisors D .. SQRT(D)
% assumes I >= 2 and I is odd
is_prime(I, D) when D * D > I -> true;
is_prime(I, D) when I rem D =:= 0 -> false;
is_prime(I, D) ->
  is_prime(I, D + 2).


% convert the list of array entries into a map
% store the integers as primes list
list_to_map(Primes, L) ->
  list_to_map(Primes, L, 0, maps:new()).

list_to_map(_Primes, [], _Index, Map) ->
  Map;
list_to_map(Primes, [H|T], Index, Map) ->
  Value = to_prime_list(Primes, H),
  list_to_map(Primes, T, Index+1, maps:put(Index, Value, Map)).


read_ops(N) ->
  Fmt = string:copies("~s~d~d ", N),
  {ok, L} = io:fread("", Fmt),
  L.

read_line(N) ->
  Fmt = string:copies("~d", N),
  {ok, L} = io:fread("", Fmt),
  L.
