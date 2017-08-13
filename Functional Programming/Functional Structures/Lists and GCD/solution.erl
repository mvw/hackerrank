-module(solution).
-export([main/0]).

main() ->
  {ok, [Q]} = io:fread("", "~d"),
  L = lists:foldl(fun(_I, LI) ->
    S = read_list(),
    [S | LI]
  end, [], lists:seq(1, Q)),
  %io:format("L=~p~n", [L]),
  Map = gcd(L),
  write(Map),
  ok.

gcd([Map1|[Map2|L]]) ->
  lists:foldl(fun(Map, Gcd) ->
    gcd(Map, Gcd)
  end, gcd(Map1, Map2), L).

gcd(Map1, Map2) ->
  %io:format("gcd: Map1=~p Map2=~p~n", [Map1, Map2]),
  maps:fold(fun(P, N1, Map) ->
    case maps:is_key(P, Map2) of
      true ->
        N2 = maps:get(P, Map2),
        N = min(N1, N2),
        maps:put(P, N, Map);
      false ->
        Map
    end
  end, maps:new(), Map1).

write(Map) ->
  LP = lists:sort(maps:keys(Map)),
  L = lists:foldl(fun(PI, LI) ->
    NI = maps:get(PI, Map),
    LI ++ [PI, NI]
  end, [], LP),
  Fmt = string:copies("~p ", length(L) - 1) ++ "~p~n",
  io:format(Fmt, L).

% we read the input line into a map of P->N
read_list() ->
  S = io:get_line(""),
  L = re:split(S, "[ \n]"),
  read_list(L, maps:new()).

read_list([], Map) ->
  Map;
read_list([_BP], Map) ->
  Map;
read_list([BP|TP], Map) ->
  [BN|TN] = TP,
  P = binary_to_integer(BP),
  N = binary_to_integer(BN),
  Map2 = maps:put(P, N, Map),
  read_list(TN, Map2).
