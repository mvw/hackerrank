-module(solution).
-export([main/0]).

main() ->
  {ok, [N]} = io:fread("", "~d"),
  L = read_list(N),
  Result = rand_force(N, L),
  io:format("~s~n", [Result]),
  ok.


% for small N we might get away with this random trial and error
rand_force(N, L) ->
  R = rand_list(N-1),
  case eval(L, R) =:= 0 of
    true ->
      combine(L, R);
    false ->
      rand_force(N, L)
  end.


rand_list(0) ->
  [];
rand_list(N) ->
  Op = rand:uniform(3),
  [Op|rand_list(N-1)].


eval([HL|TL], R) ->
  eval(TL, R, HL).

eval([], [], Result) ->
  Result;
eval([HL|TL], [HR|TR], Result) ->
  case HR of
    1 ->
      Result2 = (Result+HL) rem 101;
    2 ->
      Result2 = (Result-HL) rem 101;
    3 ->
      Result2 = (Result*HL) rem 101
  end,
  eval(TL, TR, Result2).


combine(L, R) ->
  combine(L, R, []).

combine([HL], [], Result) ->
  lists:reverse([integer_to_list(HL)|Result]);
combine([HL|TL], [HR|TR], Result) ->
  combine(TL, TR, [int_to_op(HR), integer_to_list(HL)|Result]).


int_to_op(I) ->
  case I of
    1 ->
      "+";
    2 ->
      "-";
    3 ->
      "*"
  end.


read_list(N) ->
  Fmt = string:copies("~d", N),
  {ok, L} = io:fread("", Fmt),
  L.
