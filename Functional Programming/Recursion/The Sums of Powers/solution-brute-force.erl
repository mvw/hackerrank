% this is too slow for one of the test cases
% but the use of binary words for subsets is simple

-module(solution).
-export([main/0]).

main() ->
  {ok, [X]} = io:fread("", "~d"),
  {ok, [N]} = io:fread("", "~d"),
  Basis = get_basis(X, N),
  D = array:size(Basis),
  Sets = pow(2, D),
  %io:format("Basis=~p~n", [array:to_list(Basis)]),
  %io:format("D=~p~n", [D]),
  %io:format("Sets=~p~n", [Sets]),
  Count = brute_force(X, Basis, D, Sets),
  io:format("~p~n", [Count]),
  ok.

brute_force(X, Basis, D, Sets) ->
  brute_force(X, Basis, D, Sets, 1, 0).

brute_force(_X, _Basis, _D, Sets, W, Count) when W =:= Sets ->
  Count;
brute_force(X, Basis, D, Sets, W, Count) ->
  T = test(X, Basis, D, W),
  Count2 =
  case T of
    true ->
      Count + 1;
    false ->
      Count
  end,
  brute_force(X, Basis, D, Sets, W+1, Count2).

test(X, Basis, D, W) ->
  sane(D, W) andalso test(X, Basis, W, D, 1 bsl (D-1), 0).

% if W has less 1-bits than the spoiler summands it is sane
sane(D, W) ->
  Spoiler_Summands = 13,
  Bits = bits(D, W),
  Bits < Spoiler_Summands.

bits(D, W) ->
  bits(D, W, 1, 1, 0).

bits(D, _W, I, _Mask, Sum) when I > D ->
  Sum;
bits(D, W, I, Mask, Sum) ->
  case W band Mask =/= 0 of
    true ->
      Sum2 = Sum + 1;
    false ->
      Sum2 = Sum
  end,
  bits(D, W, I+1, Mask bsl 1, Sum2).
  
test(X, _Basis, _W, _I, _Mask, Sum) when Sum > X ->
  %io:format("Sum=~p~n", [Sum]),
  false;
test(X, _Basis, _W, 0, _Mask, Sum) ->
  %io:format("I=~p Sum=~p~n", [0, Sum]),
  Sum =:= X;
test(X, Basis, W, I, Mask, Sum) ->
  %io:format("W=~p I=~p Mask=~p Sum=~p~n", [W, I, Mask, Sum]),
  I2 = I-1,
  Mask2 = Mask bsr 1,
  case (W band Mask =/= 0) of
    true ->
      Sum2 = Sum + array:get(I2, Basis);
    false ->
      Sum2 = Sum
  end,
  test(X, Basis, W, I2, Mask2, Sum2).
  

get_basis(X, N) ->
  get_basis(X, N, 1, array:new()).

get_basis(X, N, I, Basis) ->
  C = pow(I, N),
  case C > X of
    true ->
      Basis;
    false ->
      Basis2 = array:set(I-1, C, Basis),
      get_basis(X, N, I+1, Basis2)
  end.

pow(I, 1) ->
  I;
pow(I, N) ->
  I * pow(I, N-1).
