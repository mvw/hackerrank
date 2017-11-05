-module(solution).
-export([main/0]).

main() ->
  {ok, [T]} = io:fread("", "~d"),
  L = read_list(T),
  % fill Primes cache with first two primes
  Primes0 = maps:put(1, 2, maps:new()),
  Primes1 = maps:put(2, 3, Primes0),
  lists:foldl(fun(N, Primes) ->
    {P, Primes2} = prime(N, Primes), 
    io:format("~p~n", [P]),
    Primes2     
  end, Primes1, L),
  true.


prime(N, Primes) ->
  case maps:is_key(N, Primes) of
    true ->
      {maps:get(N, Primes), Primes};
    false ->
      NMax = maps:size(Primes),
      PMax = maps:get(NMax, Primes),
      lists:foldl(fun(NI, {PI, PrimesI}) ->
        P = next_prime(PI+2, PrimesI),
        {P, maps:put(NI, P, PrimesI)}
      end, {PMax, Primes}, lists:seq(NMax+1, N))
  end.


% find next prime, using I as first candidate
next_prime(I, Primes) ->
  J = 1, 
  next_prime(I, J, Primes).


next_prime(I, J, Primes) ->
  P = maps:get(J, Primes),
  case P*P > I of
    true ->
      % P is too large for a two factor product, so I must be single factor
      I;
    false ->
      case I rem P =:= 0 of
        true ->
          % P divides I, try I+2 as next candidate, start again
          next_prime(I+2, Primes);
        false ->
          % try next prime divisor on candidate I
          next_prime(I, J+1, Primes)
      end
  end.


read_list(N) ->
  Fmt = unicode:characters_to_list(lists:duplicate(N, "~d")),
  {ok, L} = io:fread("", Fmt),
  L.
