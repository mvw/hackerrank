# Range Minimum Query

## Problem
https://www.hackerrank.com/challenges/range-minimum-query

This is about efficient queries for a range [L..R]
of indices of an array A[0]..A[N-1].

In this case determing the minimum over the subarray.

## Solution

I started reading these articles:
* [Wikipedia: Range minimum query](https://en.wikipedia.org/wiki/Range_minimum_query) - gives an overview and has good references
* [Stanford: CS166](web.stanford.edu/class/archive/cs/cs166/cs166.1146/lectures/00/Small00.pdf) - this explains the ideas behind several RMQ algorithms
* [Journal of Algorithms: Lowest common ancestors in trees and directed acyclic graphs](http://www.cs.sunysb.edu/~bender/pub/JALG05-daglca.pdf) - provides the Sparse Table algorithm for RMQ, the one I settled for
* [Topcoder: Range Minimum Query and Lowest Common Ancestor](https://www.topcoder.com/community/data-science/data-science-tutorials/range-minimum-query-and-lowest-common-ancestor/#Sparse_Table_(ST)_algorithm) - spells out in detail how to calculate the sparse table

## Experience
Warm-up.
