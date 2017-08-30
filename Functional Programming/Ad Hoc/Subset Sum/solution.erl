-module(solution).
-export([main/0]).

main() ->
  {ok, [N]} = io:fread("", "~d"),
  L = read_list(N),
  {ok, [T]} = io:fread("", "~d"),
  LT = read_list(T),
  A = sort(L),
  Tree = tree(A),
  lists:foreach(fun(S) ->
    Result = check(Tree, S),
    io:format("~p~n", [Result])
  end, LT),
  true.


check(Tree, S) ->
  % returns Key >= S (if present)
  Iter = gb_trees:iterator_from(S, Tree),
  Next = gb_trees:next(Iter),
  case Next of
    none ->
      -1;
    {_Key, Value, _Iter2} ->
      Value
  end.


% integrate A and store results into general balanced tree
tree(A) ->
  tree(A, 1, 0, gb_trees:empty()).

tree([], _Index, _Sum, Tree) ->
  Tree;
tree([H|T], Index, Sum, Tree) ->
  Sum2 = Sum + H,
  Tree2 = gb_trees:insert(Sum2, Index, Tree),
  tree(T, Index+1, Sum2, Tree2).


% sort descending
sort(L) ->
  lists:sort(fun(X, Y) ->
    Y =< X
  end, L).


read_list(N) ->
  Fmt = string:copies("~d", N),
  {ok, L} = io:fread("", Fmt),
  L.
