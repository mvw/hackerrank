-module(solution).
-export([main/0]).

% Pi(10^(6/2) = 168 (according to WA)

main() ->
  {ok, [T]} = io:fread("", "~d"),
  %io:format("T=~p~n", [T]),
  IdList = read_int(T),
  %io:format("IDs=~p~n", [IdList]),
  Primes = get_primes(),
  %io:format("Primes=~p~n", [Primes]),
  fates(IdList, Primes),
	ok.

fates([], _Primes) ->
  ok;
fates([Id|T], Primes) ->
  fate(Id, Primes),
  fates(T, Primes).

fate(Id, Primes) ->
  Is_prime = test_prime(Id, Primes),
  case Is_prime of
    false ->
      io:format("DEAD~n");
    true ->
      S = integer_to_list(Id),
      No_zero_digit = not has_zero_digit(S),
      case No_zero_digit of
        false ->
          io:format("DEAD~n");
        true ->
          [_H|T] = S,
          Is_left_prime = is_left_prime(T, Primes),
          Is_right_prime = is_right_prime(lists:droplast(S), Primes),
          case {Is_left_prime, Is_right_prime} of
            {true, true} ->
              io:format("CENTRAL~n");
            {true, false} ->
              io:format("LEFT~n");
            {false, true} ->
              io:format("RIGHT~n");
            {false, false} ->
              io:format("DEAD~n")
          end
      end
  end.

% check if the base 10 representation has a zero digit  
has_zero_digit(S) ->
  lists:member($0, S).

is_left_prime([], _Primes) ->
  true;
is_left_prime(S, Primes) ->
  I = list_to_integer(S),
  Is_prime = test_prime(I, Primes),
  case Is_prime of
    false ->
      false;
    true ->
      [_H|T] = S,
      is_left_prime(T, Primes)
  end.

is_right_prime([], _Primes) ->
  true;
is_right_prime(S, Primes) ->
  I = list_to_integer(S),
  Is_prime = test_prime(I, Primes),
  case Is_prime of
    false ->
      false;
    true ->
      T = lists:droplast(S),
      is_right_prime(T, Primes)
  end.


% from my submission to https://www.hackerrank.com/challenges/twins

% test if I is prime, using a list of prime divisors
% this assumes the largest prime divisor is within the list
test_prime(_I, []) -> true;
test_prime(1, _Primes) -> false;
test_prime(I, [P|_T]) when P*P > I -> true;
test_prime(I, [P|_T]) when I rem P =:= 0 -> false;
test_prime(I, [_P|T]) ->
  test_prime(I, T).

% get the primes within {2, .., Max} as ascending list
get_primes() ->
  Max = 1000,
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
% This is slow so we only use it to build the list of prime divisors in get_primes()
is_prime(I, D) when D * D > I -> true;
is_prime(I, D) when I rem D =:= 0 -> false;
is_prime(I, D) ->
  is_prime(I, D + 2).


read_int(0) ->
  [];
read_int(I) ->
  {ok,[X]} = io:fread("", "~d"),
  [X|read_int(I-1)].
