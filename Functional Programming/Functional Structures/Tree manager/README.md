# Tree manager

## Problem
http://www.hackerrank.com/challenges/tree-manager/problem

An interpreter for a language of some tree operations

## Solution

We act on a list of strings, where we recognize operations
and arguments and then perform it on a data structure
for the tree.

This is a map ``Tree : Node -> {Value, Children, Parent}``
where ``Children`` is a list of the node numbers of the 
child nodes.

The simple list turned out to be fast enough for the
test cases. 

We keep a counter NextNode which gets increased once we
created a new node.

For deletion we update the children list, but we
do not free the data for the subtree.

There is a slight optimization using the ``Words``
hashmap for recognition of the keywords, however
without it, the time limits would be passed as well.

The input is brute force for a large limit,
not nice, but it is fast enough.
