# Prison Transport

## Problem
http://www.hackerrank.com/challenges/prison-transport/problem

Find the total bus transportation costs for a group of prisoners.

## Solution

This can be solved using graph theory.

The prisoners (those which are bound to other prisoners) are modeled by vertices, 
the handcuffs form the edges of a graph.

We determine the connected components, which are the prisoner groups
that need to be transported by a bus ride. 

Here we discover the components by depth-first search.

Key data structures are: 
  - map ``G``: node to adjacency list of the node
  - map ``Components``: node to component
  - map ``Sizes``: component to number of nodes of the component
