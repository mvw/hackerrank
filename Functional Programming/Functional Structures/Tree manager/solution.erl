-module(solution).
-export([main/0]).

main() ->
  {ok, [_Q]} = io:fread("", "~d"),
  Queries = read_list(),
  eval(Queries),
  true.


eval(Queries) ->
  Node = 0,
  NextNode = 1,
  Tree = maps:put(Node, {0, [], nil}, maps:new()),
  Words1 = maps:put("change", change, maps:new()),
  Words2 = maps:put("print", print, Words1),
  Words3 = maps:put("visit", visit, Words2),
  Words4 = maps:put("left", left, Words3),
  Words5 = maps:put("right", right, Words4),
  Words6 = maps:put("parent", parent, Words5),
  Words7 = maps:put("child", child, Words6),
  Words8 = maps:put("insert", insert, Words7),
  Words = maps:put("delete", delete, Words8),
  eval(Queries, Node, NextNode, Tree, Words).


eval([], _Node, _NextNode, _Tree, _Words) ->
  true;
eval([OpString|T], Node, NextNode, Tree, Words) ->
  Op = maps:get(OpString, Words),
  case Op of
    change ->
      change(T, Node, NextNode, Tree, Words);
    print ->
      print(T, Node, NextNode, Tree, Words);
    visit ->
      visit(T, Node, NextNode, Tree, Words);
    insert ->
      insert(T, Node, NextNode, Tree, Words);
    delete ->
      delete(T, Node, NextNode, Tree, Words)
  end.


change(T, Node, NextNode, Tree, Words) ->
  [ArgStr|T2] = T,
  X = list_to_integer(ArgStr),
  {_ValueOld, Children, Parent} = maps:get(Node, Tree),
  Tree2 = maps:put(Node, {X, Children, Parent}, Tree),
  eval(T2, Node, NextNode, Tree2, Words).


print(T, Node, NextNode, Tree, Words) ->
  {Value, _Children, _Parent} = maps:get(Node, Tree),
  io:format("~p~n", [Value]),
  eval(T, Node, NextNode, Tree, Words).


visit(T, Node, NextNode, Tree, Words) ->
  [Arg1Str|T2] = T,
  Arg1 = maps:get(Arg1Str, Words),
  case Arg1 of
    left ->
      visit_left(T2, Node, NextNode, Tree, Words);
    right ->
      visit_right(T2, Node, NextNode, Tree, Words);
    parent ->
      visit_parent(T2, Node, NextNode, Tree, Words);
    child ->
      visit_child(T2, Node, NextNode, Tree, Words)
  end.


visit_left(T, Node, NextNode, Tree, Words) ->
  {_Value, _Children, Parent} = maps:get(Node, Tree),
  {_ValueP, ChildrenP, _ParentP} = maps:get(Parent, Tree),
  Index = find(ChildrenP, Node),
  Node2 = lists:nth(Index-1, ChildrenP),
  eval(T, Node2, NextNode, Tree, Words).


visit_right(T, Node, NextNode, Tree, Words) ->
  {_Value, _Children, Parent} = maps:get(Node, Tree),
  {_ValueP, ChildrenP, _ParentP} = maps:get(Parent, Tree),
  Index = find(ChildrenP, Node),
  Node2 = lists:nth(Index+1, ChildrenP),
  eval(T, Node2, NextNode, Tree, Words).


visit_parent(T, Node, NextNode, Tree, Words) ->
  {_Value, _Children, Parent} = maps:get(Node, Tree),
  Node2 = Parent,
  eval(T, Node2, NextNode, Tree, Words).


visit_child(T, Node, NextNode, Tree, Words) ->
  [Arg2Str|T2] = T,
  N = list_to_integer(Arg2Str),
  {_Value, Children, _Parent} = maps:get(Node, Tree),
  Node2 = lists:nth(N, Children),
  eval(T2, Node2, NextNode, Tree, Words).


insert(T, Node, NextNode, Tree, Words) ->
  [Arg1Str, Arg2Str|T2] = T,
  Arg1 = maps:get(Arg1Str, Words),
  X = list_to_integer(Arg2Str),
  case Arg1 of
    left ->
      insert_left(X, T2, Node, NextNode, Tree, Words);
    right ->
      insert_right(X, T2, Node, NextNode, Tree, Words);
    child ->
      insert_child(X, T2, Node, NextNode, Tree, Words)
  end.


insert_left(X, T, Node, NextNode, Tree, Words) ->
  {_Value, _Children, Parent} = maps:get(Node, Tree),
  {ValueP, ChildrenP, ParentP} = maps:get(Parent, Tree),
  ChildrenP2 = insert_left(ChildrenP, Node, NextNode),
  Tree2 = maps:update(Parent, {ValueP, ChildrenP2, ParentP}, Tree),
  Tree3 = maps:put(NextNode, {X, [], Parent}, Tree2),
  eval(T, Node, NextNode+1, Tree3, Words).


insert_right(X, T, Node, NextNode, Tree, Words) ->
  {_Value, _Children, Parent} = maps:get(Node, Tree),
  {ValueP, ChildrenP, ParentP} = maps:get(Parent, Tree),
  ChildrenP2 = insert_right(ChildrenP, Node, NextNode),
  Tree2 = maps:update(Parent, {ValueP, ChildrenP2, ParentP}, Tree),
  Tree3 = maps:put(NextNode, {X, [], Parent}, Tree2),
  eval(T, Node, NextNode+1, Tree3, Words).


insert_left(L, Node, NextNode) ->
  Index = find(L, Node),
  Len = length(L),
  lists:sublist(L, 1, Index-1) ++ [NextNode] ++ lists:sublist(L, Index, Len-Index+1).


insert_right(L, Node, NextNode) ->
  Index = find(L, Node),
  Len = length(L),
  lists:sublist(L, 1, Index) ++ [NextNode] ++ lists:sublist(L, Index+1, Len-Index).


find(L, Node) ->
  find(L, Node, 1).

find([H|_T], Node, Index) when H =:= Node ->
  Index;
find([_H|T], Node, Index) ->
  find(T, Node, Index+1).


insert_child(X, T, Node, NextNode, Tree, Words) ->
  {Value, Children, Parent} = maps:get(Node, Tree),
  Tree2 = maps:update(Node, {Value, [NextNode|Children], Parent}, Tree),
  Tree3 = maps:put(NextNode, {X, [], Node}, Tree2),
  eval(T, Node, NextNode+1, Tree3, Words).


delete(T, Node, NextNode, Tree, Words) ->
  {_Value, _Children, Parent} = maps:get(Node, Tree),
  {ValueP, ChildrenP, ParentP} = maps:get(Parent, Tree),
  ChildrenP2 = delete(ChildrenP, Node),
  Tree2 = maps:update(Parent, {ValueP, ChildrenP2, ParentP}, Tree),
  Node2 = Parent,
  eval(T, Node2, NextNode, Tree2, Words).


delete(L, Node) ->
  Index = find(L, Node),
  Len = length(L),
  lists:sublist(L, 1, Index-1) ++ lists:sublist(L, Index+1, Len-Index).


read_list() -> 
  InputSize = 100000000,
  Data = io:get_chars("", InputSize),
  string:lexemes(Data, [$ , $\r, $\n]).
