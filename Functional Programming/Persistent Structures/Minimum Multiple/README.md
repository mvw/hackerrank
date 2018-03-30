# Minimum Multiple

## Problem
https://www.hackerrank.com/challenges/minimum-multiple/problem

This is about 
- efficient lcm queries for a continuous range [L..R] of indices of an array A[0]..A[N-1]
- efficient updates of the array, an array element has to be multiplied by a factor

## Solution
I am still banging my head against this problem.

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

Then lcm calculation boils down to picking the max of exponents and multiplication
of two numbers to addition of the exponents.

Alas this solution also passes only test cases #00 to #10

## Keywords
array, data structure, Euclid's algorithm, lcm, RQM
