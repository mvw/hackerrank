# Number of Binary Search Tree

## Problem
http://www.hackerrank.com/challenges/number-of-binary-search-tree

The basic task here is to calculate the number of binary search trees (BST) for a set of nodes 1..N

## Experience

I solved this after the topic related Valid BST problem (see below).

My first try was without memoization. This version was able to calculate all but the last test case of
the first sample solution. 

After this I wired in a map to hold the non-trivial calculations.
This solved the two samples and all test cases after submission.

It turned out that my optimization to only calculate about half of the products was not
needed to tackle the test cases.
I added the de-optimized version, which is shorter, as ``solution2.erl``.


## Solution

The basic idea here is that all nodes in the left subtree must be smaller than the node and all 
nodes in the right subtree must be larger.

This means we do not have to consider all permutations of the N nodes. 
We can concentrate on the top node and its left and right sets, which leads to a split.

Example: Set 1..5 and top node 2.

This splits the set into 1 < 2 < 3 4 5 so we have the b(1) left subtrees times b(3) right subtrees
for this case.

See the comment in the source code for examples of what splits occur.


## Related to
* [Valid BST](https://github.com/mvw/hackerrank/tree/master/Functional%20Programming/Functional%20Structures/Valid%20BST)
