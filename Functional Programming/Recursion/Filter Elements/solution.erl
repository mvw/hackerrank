-module(solution).
-export([main/0]).

main() ->
  {ok, [T]} = io:fread("", "~d"),
  lists:foreach(fun(_I) ->
    {ok, [N, K]} = io:fread("", "~d ~d"),
    A = read_list(N),                  
    Map = count(A),
    Map2 = remap(K, Map),
    L = to_list(N, Map2),
    write_list(L)
  end, lists:seq(1, T)),
  ok.

% pull the rabitts out of the hat (in reverse order,
% because we assemble the list in reverse order)
to_list(N, Map) ->
  case maps:size(Map) =:= 0 of
    true ->
      [-1];
    false ->
      lists:foldl(fun(I, L) ->
        case maps:is_key(I, Map) of
        true ->
          AI = maps:get(I, Map),
          [AI|L];
        false ->
          L
      end
    end, [], lists:seq(N, 1, -1))
  end.

% convert map AI -> {Count, Index} 
% to map of Index -> AI (if Count >= K)
remap(K, Map) ->
  maps:fold(fun(AI, {Count, Index}, NewMap) ->
    case Count >= K of
      true ->
        maps:put(Index, AI, NewMap);
      false ->
        NewMap
    end
  end, maps:new(), Map).

% A map of AI->{Count, Index}
count(A) ->
  {_Index, Map} =
  lists:foldl(fun(AI, {Index, Map}) ->
    case maps:is_key(AI, Map) of
      false ->
        Map2 = maps:put(AI, {1, Index}, Map);
      true ->
        Map2 = maps:update_with(AI, fun({Count, I}) -> 
          {Count+1, I}
        end, Map)
    end,
    {Index+1, Map2}
  end, {1, maps:new()}, A),
  Map.

read_list(N) ->
    Fmt = string:copies(" ~d", N),
    {ok, L} = io:fread("", Fmt),
    L.

write_list(L) ->
  N = length(L), 
  Fmt = string:copies("~B ", N-1) ++ "~B~n",
  io:format(Fmt, L).
