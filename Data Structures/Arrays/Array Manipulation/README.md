# Array Manipulation

## Problem
http://www.hackerrank.com/challenges/crush/problem

Apply m modifications to an array, each a value change over a subrange of the array.

## Experience

I first ran against the wall, trying a direct implementation which is only O(m n), 
which is around 10^12 operations for the large m and n instances. 

The discussion featured a smart solution with O(max(m, n)) which was straight forward to 
implement in Erlang.

As we know the non-zero array entries, they are the keys in our map, we can use 

sizeof(keys) <= 2m = 4*10^5 < n=10^7 

to further optimize by just integrating over those keys, which required a prior sort of the keys
which is O(m log m).


## Solution

The key idea is that it is more efficient to work on the derivative, needing only O(m) operations
and then integrate the derivative into the modified function, keeping track of the maximum.

See [my posting](https://www.hackerrank.com/challenges/crush/forum/comments/395339) for a detailed explanation.


## Related to
* [Weekly Challenges - Week 4 > Array Manipulation](http://www.hackerrank.com/contests/w4/challenges/crush)




