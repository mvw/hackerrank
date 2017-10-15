-module(solution).
-export([main/0]).

main() ->
  {ok, [T]} = io:fread("", "~d"),
  lists:foreach(fun(_I) ->
    {ok, [N]} = io:fread("", "~d"),
    L = read_list(N),
    write_answer(check(L))
  end, lists:seq(1, T)),
  true.


check([]) ->
  % empty list is fine
  true;
check([H|T]) ->
  % start filling the left sublist
  Left = [],
  check_left(T, H, Left).


check_left([], _Node, Left) ->
  % a left side only tree is fine, check it
  check(lists:reverse(Left));
check_left([H|T], Node, Left) when H < Node ->
  % continue left fill
  check_left(T, Node, [H|Left]);
check_left(L, Node, Left) ->
  % case H =:= Node is not possible by problem statement ("unique numbers")
  % H > Node -> start filling the right sublist
  Right = [],
  check_right(L, Node, Left, Right).


check_right([], _Node, Left, Right) ->
  % the split into left and right sublist succeeded, now check them
  check(lists:reverse(Left)) andalso check(lists:reverse(Right));
check_right([H|T], Node, Left, Right) when H > Node ->  
  % continue right fill
  check_right(T, Node, Left, [H|Right]);
check_right(_L, _Node, _Left, _Right) ->
  % H < Node -> order violation, no BST pre-order run
  false.
 

write_answer(Result) ->
  case Result of
    true ->
      Answer = "YES";
    false ->
      Answer = "NO"
  end,                   
  io:format("~s~n", [Answer]).


read_list(N) ->
  Fmt = string:copies("~d", N),
  {ok, L} = io:fread("", Fmt),
  L.
