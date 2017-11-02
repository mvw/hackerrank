-module(solution).
-export([main/0]).

main() ->
  {ok, [T]} = io:fread("", "~d"),
  L = read_list(T),
  lists:foreach(fun(N) ->
    Palin = find_palin(N),
    io:format("~p~n", [Palin])
  end, L),
  true.


find_palin(N) ->
  find_palin(N-1, N).

find_palin(X, N) ->
  case palin(X) andalso prod(X) of
    false ->
      find_palin(X-1, N);
    true ->
      X
  end.


% check if X is a palindrome
palin(X) ->
  L = integer_to_list(X),
  palin2(L).

palin2([A, B, C, C, B, A]) ->
  true;
palin2(_L) ->
  false.


prod(X) ->
  prod(X, 100).

prod(X, P) when P*P > X ->
  false;
prod(X, P) when X rem P =:= 0 andalso X div P < 1000 ->
  true;
prod(X, P) ->
  prod(X, P+1).


read_list(N) ->
  Fmt = unicode:characters_to_list(lists:duplicate(N, "~d")),
  {ok, L} = io:fread("", Fmt),
  L.
