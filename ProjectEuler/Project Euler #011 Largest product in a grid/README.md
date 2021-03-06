# Project Euler #11 Largest product in a grid

## Problem

http://www.hackerrank.com/contests/projecteuler/challenges/euler011

For a 20x20 grid of numbers between 0 and 100, we need to find the
largest product of 4 consecutive numbers, which are all on a horizontal, 
vertical or diagonal line.


## Solution

We read the data into a map A : {i, j} -> number, where we use the
indices like for a regular matrix, having {1,1} in the upper left corner.

Then we go through all four types of lines:

1. `h` -> horizontal
2. `v` -> vertical
3. `d1` -> digonal from upper left to lower right
4. `d2` -> diagonal from upper right to lower left

and go for the maximum product each.

Finally we output the biggest of the four numbers.

E.g. for `h` we look at each horizontal line from the top line
to the bottom line.

For each line we start with the first four elements and then move the window 
of width four one to the right. We can do this until the fourth window
element has moved beyond J=20.

For `v` we can do it in a similar way.

For `d1` and `d2` we might have traversed along each possible diagonal line.

Instead I was lazy and e.g. for `d1` I just moved each "\\" arrangement from left to right.
And then do it one line lower. So the long diagonals are broken into segments of four
elements. 

For `d2` I do the same with a "/" arrangement.

For both line types we do not moving window, so we end up with more multiplications 
than following the diagonals would have needed.

## Experience

I was too lazy to reduce the lines of code.
