-module(solution).
-export([main/0]).

% prisoners are vertices, handcuffs are edges of a graph
% determine the connected components of that graph

main() ->
  {ok, [N, M]} = io:fread("", "~d~d"),
  LE = read_pairs(M),
  G = graph(LE),
  %io:format("G=~p~n", [G]),
  {Components, Sizes} = components(N, G),
  %io:format("Components=~p~n", [Components]),
  %io:format("Sizes=~p~n", [Sizes]),
  Result = 
  lists:foldl(fun(Component, Sum) ->
    Size = maps:get(Component, Sizes),            
    Cost = cost(Size),
    Sum + Cost
  end, N-maps:size(Components), maps:keys(Sizes)),            
  io:format("~p~n", [Result]),
  true.


cost(Size) ->
  cost(Size, 1).

cost(Size, Cost) when Size =< Cost*Cost ->
  Cost;
cost(Size, Cost) ->
  cost(Size, Cost+1).


% Components: Node -> Component
% Sizes: Component -> size of Component

components(N, G) ->
  lists:foldl(fun(From, {Components, Sizes}) ->
    case maps:is_key(From, G) andalso not maps:is_key(From, Components) of
      true ->
        Component = maps:size(Sizes) + 1,
        Components2 = maps:put(From, Component, Components),
        Sizes2 = maps:put(Component, 1, Sizes),
        dfs(From, G, Component, Components2, Sizes2);
      false ->
        {Components, Sizes}
    end
  end, {maps:new(), maps:new()}, lists:seq(1, N)).


dfs(From, G, Component, Components, Sizes) ->
  AdjL = maps:get(From, G),
  lists:foldl(fun(To, {ComponentsTo, SizesTo}) ->
    case maps:is_key(To, ComponentsTo) of
      true ->
        {ComponentsTo, SizesTo};
      false ->
        ComponentsTo2 = maps:put(To, Component, ComponentsTo),
        Size = maps:get(Component, SizesTo),
        SizesTo2 = maps:update(Component, Size+1, Sizes),
        dfs(To, G, Component, ComponentsTo2, SizesTo2)
      end
  end, {Components, Sizes}, AdjL).


% build map Graph: From -> [To_1,.., To_k]
graph(L) ->
  Map = maps:new(),
  graph(L, Map).

graph([], Map) ->
  Map;
graph([A, B|T], Map) ->
  Map2 = graph_insert(A, B, Map),
  Map3 = graph_insert(B, A, Map2),
  graph(T, Map3).


graph_insert(From, To, Map) ->
  case maps:is_key(From, Map) of
    true ->
      L = maps:get(From, Map),
      L2 = [To|L],
      maps:update(From, L2, Map);
    false ->
      L = [To],
      maps:put(From, L, Map)
  end.


read_pairs(N) ->
  Fmt = unicode:characters_to_list(lists:duplicate(N, "~d~d")),
  {ok, L} = io:fread("", Fmt),
  L.
