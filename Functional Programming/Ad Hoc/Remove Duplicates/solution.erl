-module(solution).
-export([main/0]).

main() ->
  {ok, [S]} = io:fread("", "~s"),
  S2 = remove_duplicates(S),
  io:format("~s~n", [S2]),
	ok.

remove_duplicates(S) ->
  remove_duplicates(S, [], maps:new()).

remove_duplicates([], Result, _Memory) ->
  lists:reverse(Result);
remove_duplicates([H|T], Result, Memory) ->
  case maps:is_key(H, Memory) of
    true ->
      remove_duplicates(T, Result, Memory);
    false ->
      Memory2 = maps:put(H, seen, Memory),
      remove_duplicates(T, [H|Result], Memory2)
  end.
