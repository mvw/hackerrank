-module(solution).
-export([main/0]).

main() ->
  {ok, [T]} = io:fread("", "~d"),
  lists:foreach(fun(_I) ->
    {ok, [L]} = io:fread("", "~s"),
    R = to_string(check(L)),
    io:format("~s~n", [R])
  end, lists:seq(1, T)),
  ok.

check(L) ->
  check(L, 0, 0, 0, 0).

check([], Red, Green, Yellow, Blue) ->
  check_equal(Red, Green, Yellow, Blue);
check([$R|T], Red, Green, Yellow, Blue) ->
  check_diff(T, Red+1, Green, Yellow, Blue);
check([$G|T], Red, Green, Yellow, Blue) ->
  check_diff(T, Red, Green+1, Yellow, Blue);
check([$Y|T], Red, Green, Yellow, Blue) ->
  check_diff(T, Red, Green, Yellow+1, Blue);
check([$B|T], Red, Green, Yellow, Blue) ->
  check_diff(T, Red, Green, Yellow, Blue+1).

check_equal(Red, Green, Yellow, Blue) ->
  (Red =:= Green) andalso (Yellow =:= Blue).

check_diff(L, Red, Green, Yellow, Blue) ->
  B = (abs(Red-Green) =< 1) andalso (abs(Yellow-Blue) =< 1),
  case B of
    false ->
      false;
    true ->
      check(L, Red, Green, Yellow, Blue)
  end.

to_string(B) ->
  case B of
    true ->
      "True";
    false ->
      "False"
  end.
