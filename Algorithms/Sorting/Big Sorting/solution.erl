-module(solution).
-export([main/0]).

% While Erlang can handle long integer numbers,
% it seems the comparison is too slow for this task,
% or it might be the output.
% So we read the numbers as strings and implement 
% our own comparison.


main() ->
  {ok, [N]} = io:fread("", "~d"),
  L = read_list(N),
  S = lists:sort(fun(A, B) ->
    compare(A, B)
  end, L),
  lists:foreach(fun(I) ->
    io:format("~s~n", [I])
  end, S),
  true.


% first we go for the length of the strings
compare(A, B) ->
    LA = length(A),
    LB = length(B),
    case LA < LB of
      true ->
        true;
      false ->
        case LA > LB of
          true ->
            false;
          false ->
            compare2(A, B)
        end
    end.


% the strings are of equal length, so let us compare the digits
compare2([], []) ->
  true;
compare2([H|TA], [H|TB]) ->
  compare2(TA, TB);
compare2([HA|_TA], [HB|_TB]) ->
  HA < HB.


% read this as list of strings
read_list(N) ->
  Fmt = unicode:characters_to_list(lists:duplicate(N, "~s")),
  {ok, L} = io:fread("", Fmt),
  L.

