-module(solution).
-export([main/0]).

main() ->
  {ok, [T]} = io:fread("", "~d"),
  L = read_data(T),
  prod(L),
  true.


prod([]) ->
  true;
prod([N, K, S|T]) ->
  Max = max_prod(N, K, S),
  io:format("~p~n", [Max]),
  prod(T).


% The basic idea is to have a moving window of size K
% and not to perform K-1 multiplications at each step
% by dividing the old product with the leaving digit D1
% and multiplicating it with the entering digit D2.

% Alas occuring zero digits spoil that plan somewhat.

% So we do not integrate zero digits into the moving
% window's product, and keep track of how many are
% in the range of the window.

max_prod(N, K, S) ->
  {L1, L2, Prod, Zero} = prod1(S, K),
  case Zero > 0 of
    true ->
      Max = 0;
    false ->
      Max = Prod
  end,
  max_prod(L1, L2, N-K, Prod, Zero, Max).


max_prod(_L1, _L2, I, _Prod, _Zero, Max) when I =:= 0 ->
  Max;
max_prod([H1|T1], [H2|T2], I, Prod, Zero, Max) ->
  D1 = H1-$0,
  D2 = H2-$0,
  case D1 =:= 0 of
    true ->
      Prod2 = Prod,
      Zero2 = Zero-1;
    false ->
      Prod2 = Prod div D1,
      Zero2 = Zero
  end,
  case D2 =:= 0 of
    true ->
      Prod3 = Prod2,
      Zero3 = Zero2+1;
    false ->
      Prod3 = Prod2*D2,
      Zero3 = Zero2
  end,
  case Zero3 > 0 of
    true ->
      Max2 = Max;
    false ->
      Max2 = max(Max, Prod3)
  end,
  max_prod(T1++[H2], T2, I-1, Prod3, Zero3, Max2).


% the initial product of the first K digits
prod1(S, K) ->
  Prod = 1,
  Zero = 0,
  prod1([], S, K, Prod, Zero).

prod1(L1, L2, K, Prod, Zero) when K =:= 0 ->
  {lists:reverse(L1), L2, Prod, Zero};
prod1(L1, [H|T], K, Prod, Zero) ->
  D = H-$0,
  case D =:= 0 of
    true ->
      Prod2 = Prod,
      Zero2 = Zero+1;
    false ->
      Prod2 = D*Prod,
      Zero2 = Zero
  end,
  prod1([H|L1], T, K-1, Prod2, Zero2).


read_data(N) ->
  Fmt = unicode:characters_to_list(lists:duplicate(N, "~d~d~s")),
  {ok, L} = io:fread("", Fmt),
  L.
