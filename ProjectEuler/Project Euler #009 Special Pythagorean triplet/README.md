# Project Euler #9 Special Pythagorean triplet

## Problem
http://www.hackerrank.com/contests/projecteuler/challenges/euler009

## Solution
We have two equations

a^2 + b^2 = c^2

a + b + c = N

and three variables, plus some additional constraints (natural numbers,
a < b < c) so we can dare to search one free variable (I took c) from
1 to N, see if we hit a feasible solution and keep track of the maximum
of the resulting abc value. 

## Experience
Influenced by the later harder problems, I overthought this one first,
going into the direction of systematically generating Pythagorean triplets.
E.g. see https://en.wikipedia.org/wiki/Formulas_for_generating_Pythagorean_triples.

Alas returning at a later point, it turns out that no cleverness was needed,
e.g. like a number theoretic test for recognizing a square number (we just compare 
the floating point number result against the truncated number) or
adjusting the bounds of the search space (we just added some, possibly avoidable,
safeguard tests). 
