-module(solution).
-export([main/0]).

main() ->
  {ok, [T]} = io:fread("", "~d"),
  L = read_list(T),
  lists:foreach(fun(N) ->
    P = p_max(N),
    io:format("~p~n", [P])
  end, L),
  true.


p_max(N) ->
  p_max(N, 2, 0).


p_max(1, 2, Max) ->
  Max;
p_max(N, 2, Max) when N rem 2 =:= 0 ->
  p_max(N div 2, 2, max(Max, 2));
p_max(N, 2, Max) ->
  p(N, 3, Max).


p(N, P, Max) when P > N ->
  Max;
p(N, P, Max) when P*P > N ->
  max(Max, N);
p(N, P, Max) when N rem P =:= 0 ->
  Q = N div P,
  p(Q, P, max(Max, P));
p(N, P, Max) ->
  p(N, P+2, Max).


read_list(N) ->
  Fmt = unicode:characters_to_list(lists:duplicate(N, "~d")),
  {ok, L} = io:fread("", Fmt),
  L.
