# Minimum Multiple

## Problem
https://www.hackerrank.com/challenges/minimum-multiple/problem

This is about 
- efficient lcm queries for a continuous range [L..R] of indices of an array A[0]..A[N-1]
- efficient updates of the array, an array element has to be multiplied by a factor

## Solution
This task buggered me for quite some months. See below how I finally solved it.

## Experience

### Version 1
A naive implementation, calculating the lcm by the formula
```erlang
lcm(a, b) = |a b| / gcd(a,b) = (|a| / gcd(a,b)) |b|
```
and the gcd by Euclid's algorithm.

It manages to pass test cases #00 to #10, still ten more to go.

### Version 2
Another way to compute the lcm is to use the prime factorization of a whole number.

Initial array element values and factors are limited to numbers from the set 1..100,
which we can represent by the exponents of the 25 prime numbers from that interval.

Then 
- lcm calculation boils down to picking the max of exponents and 
- multiplication of two numbers to addition of the exponents.

So no new prime factors are introduced by these two operations.

Alas this solution also passes only test cases #00 to #10

### Version 3
Revisiting unsolved problems, I came along the "Range Minimum Query" problem.

It dawned to me that "Minimum Multiple" can be formulated as Range Maximum Query problem.

To get more familiar with RMQ data structures I solved "Range Minimum Query" first.
Of the various data structures I chose the "Sparse Table" which was good enough to tackle the problem.

So this version uses a "Sparse Table". It needs O(n log n) space and allows for O(1) lookup and is not
too complicated. 

The trick is storing 2^k long subranges (of O(n log n) many) and reconstructing 
every subrange (of O(n^2) many) by using two of the precalculated subranges only.

The flip side is that the "Minimum Multiple" problem involves changes to the matrix.

Recalculating the whole sparse table every time turned out to be too slow. 
The test cases seemed to feature many updates, perhaps half of the ops. 
So getting efficient here as well is important.

This solution passes only test cases #00 to #06.

### Version 4
I found no treatise on the sparse table which dealt with efficient updates. 
So I tried to come up with my own scheme. This improved my understanding of the algorithm.

Alas my analysis seemed to indicate that updating only those sparse table elements which need an update after
modification of an array element (first column of the sparse table) will involve still too many elements.
Worst case would be modifying a middle array element and the update would spread considerably, as this element is
part of a lot of interval ranges. 
This might still be O(n log n) per update, for O(n) updates. 

While it tempted me to finish that road, I finally broke up with the sparse table.

It seemed to me that the segment tree might be good enough:
Query is only O(log n) instead of O(1), but update is O(log n) as well and far simpler than what a minimal
update of the sparse table would be (if I am not wrong).

The trick here is how to split the full interval into interval halves and how to combine these intervals into
any possible subrange.

This solution finally passed all test case, thus #00 to #20.

### Further improvements ###
We might drop the lcm/factor implementations using the prime exponent vectors and return to the simpler gcd
calculation from version 1. But I was too lazy to try this out.

## Keywords
array, data structure, Euclid's algorithm, lcm
