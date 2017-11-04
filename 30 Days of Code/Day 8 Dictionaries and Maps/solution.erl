-module(solution).
-export([main/0]).

main() ->
  {ok, [N]} = io:fread("", "~d"),
  Pairs = read_pairs(N),
  Map = dict(Pairs), 
  Queries = read_list(),
  lists:foreach(fun(Name) ->
    case maps:is_key(Name, Map) of
      true ->
        Number = maps:get(Name, Map),
        io:format("~s=~p~n", [Name, Number]);
      false ->
        io:format("Not found~n")
    end
  end, Queries),
  true.


dict(L) ->
  dict(L, maps:new()).

dict([], Map) ->
  Map;
dict([Name, Number|T], Map) ->
  Map2 = maps:put(Name, Number, Map),
  dict(T, Map2).


read_list() -> 
  InputSize = 10000000, % found by trial and error
  Data = io:get_chars("", InputSize),
  string:lexemes(Data, [$\r, $\n]).

read_pairs(N) ->
  Fmt = unicode:characters_to_list(lists:duplicate(N, "~s ~d")),
  {ok, L} = io:fread("", Fmt),
  L.
