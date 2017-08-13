-module(solution).
-export([main/0]).

main() ->
	{ok, [N]} = io:fread("", "~d"),
  LN = read_list(2*N),
  Tree = tree(N, LN),
  %io:format("~w~n", [Tree]),
	{ok, [T]} = io:fread("", "~d"),
  LK = read_list(T),
  operate(N, Tree, LK),
  true.

% work through the list of K_1 to K_T
operate(_N, _Tree, []) ->
  true;
operate(N, Tree, [K|TK]) ->
  Tree2 = swap_op(Tree, K),
  write(N, Tree2),
  operate(N, Tree2, TK).


% swap operation
swap_op(Tree, K) ->
  swap_op(Tree, K, 1, 1).

swap_op(Tree, _K, nil, _Height) ->
  Tree;
swap_op(Tree, K, Node, Height) ->
  case Height rem K =:= 0 of
    true ->
      Tree2 = swap(Tree, Node);
    false ->
      Tree2 = Tree
  end,
  {Left, Right} = maps:get(Node, Tree2),
  Height2 = Height+1,
  Tree3 = swap_op(Tree2, K, Left, Height2),
  swap_op(Tree3, K, Right, Height2).


% swap children of Node
swap(Tree, Node) ->
  {Left, Right} = maps:get(Node, Tree),
  maps:update(Node, {Right, Left}, Tree).


% build tree from pair list
tree(N, L) ->
  tree(N, L, 1, maps:new()).

tree(N, _, I, Tree) when I > N ->
  Tree;
tree(N, [Left, Right|T], I, Tree) ->
  tree(N, T, I+1, insert_node(Tree, I, Left, Right)).


% insert a node and its child references into the tree map
insert_node(Tree, Node, -1, -1) ->
  insert_node(Tree, Node, nil, nil);  % mapping to nil as special value
insert_node(Tree, Node, -1, Right) ->
  insert_node(Tree, Node, nil, Right);
insert_node(Tree, Node, Left, -1) ->
  insert_node(Tree, Node, Left, nil);
insert_node(Tree, Node, Left, Right) ->
  maps:put(Node, {Left, Right}, Tree).


% write tree using inorder traversal
write(N, Tree) ->
  L = inorder(Tree, 1),
  Fmt = string:copies("~w ", N-1) ++ "~w~n", 
  io:format(Fmt, L).

inorder(_Tree, nil) ->
  [];
inorder(Tree, Node) ->
  {Left, Right} = maps:get(Node, Tree),
  inorder(Tree, Left) ++ [Node] ++ inorder(Tree, Right).


read_list(N) ->
  Fmt = string:copies("~d", N),
  {ok, L} = io:fread("", Fmt),
  L.
