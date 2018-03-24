# Project Euler #10 Summation of primes

## Problem

http://www.hackerrank.com/contests/projecteuler/challenges/euler010

For 1 <= T <= 10^4 we get a list N_1, .., N_T with 1 <= N_i <= 10^6.

We have to output a list S_1, .., S_T, where S_i contains the
sum of all prime numbers less equal than N_i.

## Solution

### We try to avoid repetition

If we know the largest N_i, we can calculate the set of prime numbers not greater 
than N_i once and use them for all N_i.

If we sort the N_i in ascending order, we can calculate S_{i+1} starting
from S_i, just adding the prime numbers between N_i and N_{i+1}.

### Finding prime numbers

`primes(N)` builds a map k -> P_k for all prime numbers not greater than N:

It handles the first few cases until we need only to check odd numbers.

`primes(X, N, Primes)` extends the prime number map Primes with all primes
not greater than N with first candidate X, progressing in steps of two.

We check X if it is a prime by using all known primes as divisors, stopping
if P*P > X.

### Summation

We have two cases where we save the summation depending if we have run 
out of primes or not.


## Experience

Nothing special.
