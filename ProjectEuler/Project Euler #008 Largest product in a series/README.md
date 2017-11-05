# Project Euler #8 Largest product in a series

## Problem
http://www.hackerrank.com/contests/projecteuler/challenges/euler008

## Solution
The basic idea is to have a moving window of size ``K``
and not to perform K-1 multiplications at each step
by dividing the old product with the leaving digit ``D1``
and multiplicating it with the entering digit ``D2``.

Alas occuring zero digits spoil that plan somewhat.

So we do not integrate zero digits into the moving
window's product, and keep track of how many are
in the range of the window.

## Experience
My first version read in the number as an integer,
and then turned it into a string via ``integer_to_list/1``.

For whatever reason this led to runtime errors 
with the test cases #2 to #9.

When I read in the number as string all testcases
were passed.

Erlang is able to handle 1000 digit integers and
memory consumption should be not an issue
(3x1000x8 bytes for the number I would assume from the docs).
Strange.
