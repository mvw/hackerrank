-module(solution).
-export([main/0]).

main() ->
  {ok, [N, K]} = io:fread("", "~d ~d"),
  SD = super_digit(N, K),
  io:format("~p~n", [SD]),
  ok.

super_digit(N, K) ->
  L = integer_to_list(N),
  Sum = digit_sum(L),
  super_digit(K * Sum).

super_digit(X) ->
  L = integer_to_list(X),
  case length(L) of
    1 ->
      X;
    _Else ->
      Sum = digit_sum(L),
      super_digit(Sum)
  end.

digit_sum(L) ->
  digit_sum(L, 0).

digit_sum([], Sum) ->
  Sum;
digit_sum([H|T], Sum) ->
  digit_sum(T, Sum + H - $0).
