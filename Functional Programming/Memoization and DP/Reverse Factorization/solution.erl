-module(solution).
-export([main/0]).

main() ->
  {ok, [N, K]} = io:fread("", "~d~d"),
  case N =:= 1 of
    true ->
      % the path with zero hops
      io:format("1~n"),
      true;
    false ->
      L = read_list(K),
      F = filter_divisors(L, N),
      case F =:= [] of
        true ->
          % N not 0 and no divisors
          io:format("-1~n"),
          false;
        false ->
          S = lists:sort(F),
          {Results, _Map} = search(N, S),
          Min = opt(Results),
          write(Min),
          true
      end
  end.


filter_divisors(L, N) ->
  lists:filter(fun(X) ->
    (X =/= 1) andalso (N rem X =:= 0)
  end, L).


search(N, L) ->
  Map = maps:new(),
  search(N, L, Map).


search(N, _L, Map) when N =< 1 ->
  {[[]], Map};
search(N, L, Map) ->
  case maps:is_key(N, Map) of
    true ->
      Results = maps:get(N, Map),
      {Results, Map};
    false ->
      {Results2, Map2} =
      lists:foldl(fun(X, {ResultsX, MapX}) ->
        case N rem X =:= 0 of
          false ->
            {ResultsX, MapX};
          true ->
            {Results, MapX2} = search(N div X, L, MapX),
            Results2 = lists:map(fun(T) ->
              [X|T]
            end, Results),
            Results3 = lists:filter(fun(LR) ->
              ascending(LR)
            end, Results2),
            ResultsX2 = ResultsX ++ Results3,
            {ResultsX2, MapX2}
        end
      end, {[], Map}, L),
      Map3 = maps:put(N, Results2, Map2),
      {Results2, Map3}
  end.


ascending([_X]) ->
  true;
ascending([X, Y|T]) ->
  case X =< Y of
    false ->
      false;
    true ->
      ascending([Y|T])
  end.


% pick the first entry with minimal length
opt([]) ->
  [];
opt(L) ->
  [Min|_T] = L,
  MinLen = length(Min),
  opt(L, MinLen, Min).

opt([], _MinLen, Min) ->
  Min;
opt([H|T], MinLen, Min) ->
  Len = length(H),
  case Len < MinLen of
    true ->
      opt(T, Len, H);
    false ->
      opt(T, MinLen, Min)
  end.


% e.g. [2, 2, 3] -> output "1 2 4 12"
write([]) ->
  io:format("-1~n");
write(L) ->
  write(L, 1).

write([], X) ->
  io:format("~p~n", [X]);
write([H|T], X) ->
  io:format("~p ", [X]),
  write(T, H*X).


read_list(N) ->
  Fmt = unicode:characters_to_list(lists:duplicate(N, "~d")),
  {ok, L} = io:fread("", Fmt),
  L.
