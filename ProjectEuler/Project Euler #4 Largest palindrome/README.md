# Project Euler #4 Largest palindrome

## Problem
http://www.hackerrank.com/contests/projecteuler/challenges/euler004

## Solution

I had no better idea than to walk down the candidate list
from N-1 and check if we hit a palindrome which can be 
factored into two three-digit factors.

So I used Erlang pattern matching to test if we have
a palindrome. Fun. 

And I used trial division with divisors

  100, 101, 102, ..
  
to split off a factor P and see if the remaining Q is less than 1000.

Again I stopped trials if

  P*P > X

because then it is clear that we can not split X into two factors.

It might be necessary to check for Q >= 100 as well, not sure if 
this is not needed or if the test cases are not good enough to
expose this hole.

To my surprise this approach was fast enough to tackle all test
cases.
