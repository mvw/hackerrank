-module(solution1).
-export([main/0]).

main() ->
  {ok, [T]} = io:fread("", "~d"),
  L = read_data(T),
  find(L),
  true.


% Let us find out if the Erlang implementation is good enough for this task

find([]) ->
  true;
find([Text, Pat|T]) ->
  case re:run(Text, Pat) of
    nomatch ->
      Answer = "NO";
    _Else ->
      Answer = "YES"
  end,
  io:format("~s~n", [Answer]),
  find(T).


read_data(N) ->
  Fmt = unicode:characters_to_list(lists:duplicate(N, "~s~s")),
  {ok, L} = io:fread("", Fmt),
  L.
