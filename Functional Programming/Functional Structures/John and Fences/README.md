# John and Fences

## Problem
http://www.hackerrank.com//challenges/john-and-fences/problem

Find the biggest rectangle under the height bars.

## Solution

This looked tough. 

My first idea was to fill up the upper triangular matrix A(I,J), which holds 
the area values spanning from the height bars from index I to J, from the 
main diagonal A[I,I] upwards.

This would use the recursion

  A(I, I) = h(I)

  A(I, J+1) = (J-I+1+1)/(J-I+1) min( A(I,J), (J-I+1) h(J+1) )
  
which allows to calculate the values, just keeping two diagonals in memory
in each step.

Alas it would need S(N) = N(N+1)/2 steps, with N <= 10^5, so roughly 5*10^9

The next idea was to rotate the situation by 90 degreees to the right
and see what can be done here.

I ended up with a decomposition into rectangles

1x [1,6]

2x [1,4] [6,6]

3x [2,4] [6,6]

4x [2,4] [6,6]

5x [2,3] [6,6]

6x [3,3] [6,6]

7x [3,3] [6,6]

8x [6,6]

9x -

for the sample. This should need H*(N/2) steps at most, with H up to 10^4, 
thus 5*10^8 steps at most. Not much better.

The final solution was built on the idea that a minimal bar allows to split
the set of bars into three parts. Left, middle and right:

The middle minimal bar acts as blocker and prevents higher rectangles to further extend from the 
left side to the right side and vice versa. 

So we could divide the problem into smaller instances, over the three parts, 
find the maximum for each part and use these to find the maximum over all bars (conquer step).

Of course we do this recursively. This results in O(N log N) many steps, around 10^6, which was
fast enough.
