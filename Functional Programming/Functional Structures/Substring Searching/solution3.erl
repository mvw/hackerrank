-module(solution3).
-export([main/0]).

main() ->
  {ok, [T]} = io:fread("", "~d"),
  L = read_data(T),
  find(L),
  true.


% Let us find out if the Erlang implementation is good enough for this task
% This approach stolen from cchalasani

find([]) ->
  true;
find([Text, Pat|T]) ->
  BinText = list_to_binary(Text),
  BinPat = list_to_binary(Pat),
  case binary:match(BinText, BinPat) of
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
