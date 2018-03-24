# Project Euler #12 Highly divisible triangular number

## Problem

http://www.hackerrank.com/contests/projecteuler/challenges/euler012

For 1 <= T <= 10 we get a list N_1, .., N_T with 1 <= N_i <= 10^3.

We have to output a list tri_1, .., tri_T, where tri_i is the
smallest triangular number with more than N_i divisors.

## Solution

The problem somewhat resembles
[Project Euler #10](https://github.com/mvw/hackerrank/tree/master/ProjectEuler/Project%20Euler%20%23010%20Summation%20of%20primes)

From the unique prime factorization of an integer

  x = p_1^{e_1} p_2^{e_2} p_3^{e_3} ..
  
we see that all divisors can be created by varying the exponents from 0 to e_i for each prime factor p_i.

This gives prod_i (1+e_i) as number of divisors of x.

We sort the N_i in ascending order, so we can grow the triangular numbers while 
growing the map of discovered prime numbers.


## Experience

Nothing special.
