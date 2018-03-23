# Big Sorting

## Problem
https://www.hackerrank.com/challenges/big-sorting/problem

Sort a list of long numbers, all in all up to a million digits might be used.

## Experience

Erlang can deal with long numbers. 
See [solution_slow.erl](https://github.com/mvw/hackerrank/blob/master/Algorithms/Sorting/Big%20Sorting/solution_slow.erl).

But it turns out this approach is too slow to tackle all test cases.

So I tried to use strings instead of numbers, which needs its
own comparison function to replace the standard lexicographic order.

This was fast enough. 
See [solution.erl](https://github.com/mvw/hackerrank/blob/master/Algorithms/Sorting/Big%20Sorting/solution.erl).

I did not know what made the first solution slow. I suspected the sorting.
So I wanted to profile the code.

I downloaded a testcase from hackerrank and found out from the Erlang FAQ
how to execute the programs using standard input and output in the shell.

The easiest way to profile in this setup seemed using 
[erlang:timestamp/0](http://erlang.org/doc/man/erlang.html#type-timestamp)
and
[timer:now_diff/2](http://erlang.org/doc/man/timer.html#now_diff-2).

See 
[solution_slow_tc.erl](https://github.com/mvw/hackerrank/blob/master/Algorithms/Sorting/Big%20Sorting/solution_slow_tc.erl) 
and 
[solution_tc.erl](https://github.com/mvw/hackerrank/blob/master/Algorithms/Sorting/Big%20Sorting/solution_tc.erl) 
and 
[run-slow-tc.sh](https://github.com/mvw/hackerrank/blob/master/Algorithms/Sorting/Big%20Sorting/run-slow-tc.sh) 
and 
[run-tc.sh](https://github.com/mvw/hackerrank/blob/master/Algorithms/Sorting/Big%20Sorting/run-tc.sh).

## Results of the profiling

### solution_slow_tc.erl:

op     | time [s]
------ | -----
read : | 5.039
sort : | 0.0
write: | 31.06

### solution_tc.erl

op     | time [s]
------ | -----
read : | 0.249
sort : | 0.125
write: | 1.279

### Conclusion

So my guess was wrong, it is not sorting which is slower for integers than strings,
but the I/O. Quite a difference.
