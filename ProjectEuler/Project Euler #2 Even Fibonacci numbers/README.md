My first attempt was solving the linear recurrence relation
which leads to 

    F_n = k_1 r_1^n + k_2 r_2^n

and can be summed to a solution of the form

    S = k_1 r_1^2 G(K, r_1^3) + k_2 r_2^2 G(K, r_2^3)

where G(K, q) is the well known closed form of the 
geometric series

    G(K, q) = sum_(k=0)^K q^k = (q^(K+1)-1)/(q-1)
  
It reached 75/100, failing test case #3 with a "Wrong Answer".

The reason is probably the limited precision of Erlang's float
data type. The F_n solution uses the irrational numbers

    r1 = (1+sqrt(5))/2 =  1.618..
    r2 = (1-sqrt(5))/2 = -0.618..
    [k1; k2] = 1/sqrt(5) [r1; -r2]

to generate the integer valued sequence F_n.

I pondered about transforming the above explicit solution such
that only integer calculations were involved, but then had
a look at the discussion thread 

https://www.hackerrank.com/contests/projecteuler/challenges/euler002/forum/comments/78668

where dusty_mehul focused on a transformed Fibonacci reccurence relation E_n
which directly yields the even elements and claimed it would simplify the solution. 

So I determined E_n and directly evaluated it, which is possible in a few lines
of code. Thanks to Erlang's integer implementation it was precise enough to
tackle all test cases and even about 2s faster than the floating point arithmetic based
approach.
