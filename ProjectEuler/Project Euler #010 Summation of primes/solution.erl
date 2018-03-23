-module(solution).
-export([main/0]).

main() ->
  {ok, [T]} = io:fread("", "~d"),
  L = read_data(T),
  S = lists:sort(L),
  N_max = lists:last(S),
  Primes = primes(N_max),
  Sums = sums(S, Primes),
  lists:foreach(fun(N) ->
    Sum = maps:get(N, Sums),
    io:format("~p~n", [Sum])
  end, L),
  true.


sums(S, Primes) ->
  I = 1,
  I_max = maps:size(Primes),
  Sum = 0,
  Sums = maps:new(),
  sums(S, I, I_max, Sum, Primes, Sums).

sums([], _I, _I_max, _Sum, _Primes, Sums) ->
  Sums;
sums([N|T], I, I_max, Sum, Primes, Sums) when I > I_max ->
  Sums2 = maps:put(N, Sum, Sums),
  sums(T, I, I_max, Sum, Primes, Sums2);
sums(S, I, I_max, Sum, Primes, Sums) ->
  [N|T] = S,
  P = maps:get(I, Primes),
  case P =< N of
    true ->
      sums(S, I+1, I_max, Sum+P, Primes, Sums);
    false ->
      Sums2 = maps:put(N, Sum, Sums),
      sums(T, I, I_max, Sum, Primes, Sums2)
  end.


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


read_data(N) ->
  Fmt = unicode:characters_to_list(lists:duplicate(N, "~d")),
  {ok, L} = io:fread("", Fmt),
  L.
