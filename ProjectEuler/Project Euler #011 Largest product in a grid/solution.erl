-module(solution).
-export([main/0]).

main() ->
  N = 20,
  L = read_data(N*N),
  A = array(L),
  M1 = max_h(A),
  M2 = max_v(A),
  M3 = max_d1(A),
  M4 = max_d2(A),
  M = lists:max([M1, M2, M3, M4]),
  io:format("~p~n", [M]),
  true.


max_h(A) ->
  lists:foldl(fun(I, Max) ->
    max(Max, max_h(I, A))
  end, 0, lists:seq(1, 20)).

max_h(I, A) ->
  E1 = maps:get({I, 1}, A),
  E2 = maps:get({I, 2}, A),
  E3 = maps:get({I, 3}, A),
  E4 = maps:get({I, 4}, A),
  L = [E1, E2, E3, E4],
  P = prod(L),
  max_h(I, 4, L, A, P).

max_h(_I, J, _L, _A, Max) when J =:= 20 ->
  Max;
max_h(I, J, L, A, Max) ->
  J2 = J+1,
  [_E1, E2, E3, E4] = L,
  E5 = maps:get({I, J2}, A),
  L2 = [E2, E3, E4, E5],
  P = prod(L2),
  Max2 = max(Max, P),
  max_h(I, J2, L2, A, Max2).


max_v(A) ->
  lists:foldl(fun(J, Max) ->
    max(Max, max_v(J, A))
  end, 0, lists:seq(1, 20)).

max_v(J, A) ->
  E1 = maps:get({1, J}, A),
  E2 = maps:get({2, J}, A),
  E3 = maps:get({3, J}, A),
  E4 = maps:get({4, J}, A),
  L = [E1, E2, E3, E4],
  P = prod(L),
  max_v(4, J, L, A, P).

max_v(I, _J, _L, _A, Max) when I =:= 20 ->
  Max;
max_v(I, J, L, A, Max) ->
  I2 = I+1,
  [_E1, E2, E3, E4] = L,
  E5 = maps:get({I2, J}, A),
  L2 = [E2, E3, E4, E5],
  P = prod(L2),
  Max2 = max(Max, P),
  max_v(I2, J, L2, A, Max2).


max_d1(A) ->
  lists:foldl(fun(I, Max) ->
    max(Max, max_d1(I, A))
  end, 0, lists:seq(1, 17)).

max_d1(I, A) ->
  E1 = maps:get({I, 1}, A),
  E2 = maps:get({I+1, 2}, A),
  E3 = maps:get({I+2, 3}, A),
  E4 = maps:get({I+3, 4}, A),
  P = E1*E2*E3*E4,
  max_d1(I, 4, A, P).

max_d1(_I, J, _A, Max) when J > 20 ->
  Max;
max_d1(I, J, A, Max) ->
  J2 = J+1,
  E1 = maps:get({I, J-3}, A),
  E2 = maps:get({I+1, J-2}, A),
  E3 = maps:get({I+2, J-1}, A),
  E4 = maps:get({I+3, J}, A),
  P = E1*E2*E3*E4,
  Max2 = max(Max, P),
  max_d1(I, J2, A, Max2).


max_d2(A) ->
  lists:foldl(fun(I, Max) ->
    max(Max, max_d2(I, A))
  end, 0, lists:seq(1, 17)).

max_d2(I, A) ->
  E1 = maps:get({I, 4}, A),
  E2 = maps:get({I+1, 3}, A),
  E3 = maps:get({I+2, 2}, A),
  E4 = maps:get({I+3, 1}, A),
  P = E1*E2*E3*E4,
  max_d2(I, 4, A, P).

max_d2(_I, J, _A, Max) when J > 20 ->
  Max;
max_d2(I, J, A, Max) ->
  J2 = J+1,
  E1 = maps:get({I, J}, A),
  E2 = maps:get({I+1, J-1}, A),
  E3 = maps:get({I+2, J-2}, A),
  E4 = maps:get({I+3, J-3}, A),
  P = E1*E2*E3*E4,
  Max2 = max(Max, P),
  max_d2(I, J2, A, Max2).


prod([A, B, C, D]) ->
  A*B*C*D.


array(L) ->
  I = 1, 
  J = 1,
  A = maps:new(),
  array(L, I, J, A).

array([], _I, _J, A) ->
  A;
array(L, I, J, A) when J > 20 ->
  array(L, I+1, 1, A);
array([H|T], I, J, A) ->
  A2 = maps:put({I, J}, H, A),
  array(T, I, J+1, A2).


read_data(N) ->
  Fmt = unicode:characters_to_list(lists:duplicate(N, "~d")),
  {ok, L} = io:fread("", Fmt),
  L.
