-module(solution).
-export([main/0]).

% 12s CPU
% 1024M RAM
% 50kB Code

% 1 <= N <= M <= 10^9
% M - N <= 10^6 (max number of gap positions to test)

% Pi(10^4.5) = Pi(31623) = 3401 (number of primes), from WolframAlpha query

% Every gap, except 4:(3,5) and 6:(5,7)is of the form 30*n, 30*n+12 or 30*n+18
% source: https://de.wikipedia.org/wiki/Primzahlzwilling#Eigenschaften

main() ->
  N = read_int(),
  M = read_int(),
  Pairs = pairs(N, M),
  write_int(Pairs).

% number of pairs in {N,..,M}
pairs(N, _M) when N < 1 ->
  n_too_small;
pairs(N, M) when N > M ->
  n_larger_m;
pairs(_N, M) when M > 1000000000 ->
  m_too_large;
pairs(N, M) when M - N < 2 -> 
  0;
pairs(N, M) ->
  Primes = get_primes(),
  %io:format("Primes=~p~n", [Primes]), 
  pairs(N+1, M-1, 0, Primes).

% I: check pair (I-1,I+1)
% I_max: maximal feasible I value
% Found: pairs found so far
pairs(I, I_max, Found, _Primes) when I > I_max ->
  Found;
pairs(I, I_max, Found, Primes) when I =:= 4; I =:= 6 ->
  % pairs at 4:(3,5) and 6:(5, 7) 
  pairs(I + 2, I_max, Found + 1, Primes);
pairs(I, I_max, Found, Primes) when I < 6 ->
  % 2:(1,3) 3:(2,4), 5:(4,6) - nothing here
  pairs(I+1, I_max, Found, Primes);
pairs(I, I_max, Found, Primes) ->
  % for I >= 6 we can use the gap property, so we
  % need to check only 3 out of 30 positions (10^5 checks)
  pairs(I, I_max, I rem 30, Found, Primes).

% test the positions with remainder from {0, 29}
pairs(I, I_max, _R, Found, _Primes) when I > I_max ->
  % search finished
  Found;
pairs(I, I_max, 0, Found, Primes) ->
  % need to test candidate
  Found2 = test(I, Found, Primes),
  pairs(I+12, I_max, 12, Found2, Primes);
pairs(I, I_max, 12, Found, Primes) ->
  Found2 = test(I, Found, Primes),
  pairs(I+6, I_max, 18, Found2, Primes);
pairs(I, I_max, 18, Found, Primes) ->
  Found2 = test(I, Found, Primes),
  pairs(I+12, I_max, 0, Found2, Primes);
pairs(I, I_max, R, Found, Primes) when R < 12 ->
  % skip to next candidate
  pairs(I+(12-R), I_max, 12, Found, Primes);
pairs(I, I_max, R, Found, Primes) when R < 18 ->
  pairs(I+(18-R), I_max, 18, Found, Primes);
pairs(I, I_max, R, Found, Primes) ->
  pairs(I+(30-R), I_max, 0, Found, Primes).


% test the pair around gap position I, updating Found, using Primes
test(I, Found, Primes) ->
  Test = test_prime(I+1, Primes) andalso test_prime(I-1, Primes),
  case Test of
    false ->
      Found;
    true ->
      Found+1
  end.

% test if I is prime, using a list of about 3401 prime divisors
% this assumes the largest prime divisor is within the list
test_prime(_I, []) -> true;
test_prime(1, _Primes) -> false;
test_prime(I, [P|_T]) when P*P > I -> true;
test_prime(I, [P|_T]) when I rem P =:= 0 -> false;
test_prime(I, [_P|T]) ->
  test_prime(I, T).

% get the primes within {2, .., Max} as ascending list
get_primes() ->
  Max = 31623,
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
% This is too slow for all test cases, using up to 31623 divisors, 
% so we only use it to build the list of prime divisors in get_primes()
is_prime(I, D) when D * D > I -> true;
is_prime(I, D) when I rem D =:= 0 -> false;
is_prime(I, D) ->
  is_prime(I, D + 2).


read_int() -> 
  {ok, [I]} = io:fread("", "~d"),
  I.

write_int(I) ->
  io:format("~p~n", [I]).
