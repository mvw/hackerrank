# Valid BST

## Problem
http://www.hackerrank.com/challenges/valid-bst

The basic task here is to decide if a given list of unique numbers from 1 to N is 
the result of a pre-order traversal run through a binary search tree (BST).

Pre-order traversal means first visit node, then visit left subtree then right subtree.

## Solution
We need to check if the order properties of the BST hold:
- an empty tree (resulting in an empty list) is valid
- the left subtree has only elements smaller than the node
- the right subtree has only elements larger than the node

And this is what my solution does.
First it checks for the empty case.

Otherwise it grabs the node and tries to split the remaining list into the 
sublists for the left and right subtree.

While the list elements are smaller than the node, we push them on the left sublist.

We can not hit a list element equal to the node, as the problem statement
says the list contains unique numbers.

If we hit a list element larger than the node, we start pushing the following 
list elements on the right sublist. Note that the recursion will continue
with a different function, which embodies the switch from "left fill" to
"right fill" mode.

If we hit a smaller list element, then this list was not the result of
some pre-order traversal. We can return with a false value.

If everything went well, we were able to split the list into node and
left and right sublists. 

Finally we continue recursively with the checks of the left
and (if this succeeds) the right sublist.

## Related to
* [Number of Binary Search Tree](http://github.com/mvw/hackerrank/tree/master/Functional%20Programming/Memoization%20and%20DP/Number%20of%20Binary%20Search%20Tree)
