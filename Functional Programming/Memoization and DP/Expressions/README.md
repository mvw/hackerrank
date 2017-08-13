# Expressions

## Problem
http://www.hackerrank.com/challenges/expressions

We have a sequence of N integers x_i, N from {2,..,10^4}, x_i from {1,..,100}
where we need to fill the N-1 gaps with binary operators op_i from {+,-,*}
to get the total value 

x_1 op_1 x_2 op_2 .. op_{N-1} x_N = 0 (mod 101)

where the usual operator precedence is not applied, we just evaluate from left to right.

## Experience
Warming up I coded a random trial and error. I was embarrassed that this
already tackled all test cases. Oh boy! :blush:

My expectation was, this challenge is categorized as hard, that some clever balancing 
scheme was needed, as brute-forcing up to 3^999 configurations seemed not possible.
(The subdomain might be a hint as well :grin:)

## Notes
- The modulus m=101 is a prime so Z/mZ is a finite field.
- With only 100 values to choose up to 10^4 items from, repetition must
  occur.

## Erlang Insights
- How does one get a random number in Erlang? It turns out two Erlang modules are available
  (using Erlang/OTP 20 [erts-9.0]):
  - http://erlang.org/doc/man/rand.html
  - http://erlang.org/doc/man/random.html

  [This article](https://hashrocket.com/blog/posts/the-adventures-of-generating-random-numbers-in-erlang-and-elixir)
  pointed to rand as the proper choice. 
  So I used rand:uniform(3) to generate random numbers from {1,2,3}.
- combine/3 returns a list of strings, knowing that io:format can deal with deep lists
