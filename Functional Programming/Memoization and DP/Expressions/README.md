https://www.hackerrank.com/challenges/expressions

We have a sequence of N integers x_i, N from {2,..,10^4}, x_i from {1,..,100}
where we need to fill the N-1 gaps with operators op_i from {+,-,*}
to get the total value x_1 op_1 x_2 op_2 .. = 0 mod 101, where the usual
operator precedence is not applied, we can evaluate from left to right.

Warming up I coded a random trial and error. I was embarrassed that this
already tackled all test cases. Oh boy. :-)

My expectation was that some clever balancing scheme was needed, 
as brute forcing up to 3^999 configurations seemed not possible.

Notable:
- The modulus m=101 is a prime so Z/mZ is a finite field.
- With only 100 values to choose up to 10^4 items from, repetition must
  occur.
