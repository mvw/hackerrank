# Subset Sum

## Problem
http://www.hackerrank.com/challenges/subset-sum/problem

We have a list of N positive integers a_i, where N from {1,..,10^5} and a_i from {1,..,10^9}.

Against this we need to perform T queries S_j, T from {1,..,10^5} and S_j from {1,..,10^15}, 
where we want to know what the size of the smallest subset of {a_1,..,a_N} is, such that
the sum of its elements is equal or larger than S_j. Or -1 if there is no such subset.

## Experience
It makes sense to pack a subset to match the query sum S starting with the largest elements.
So sorting the list of a_i in descending order is needed.

As this problem was rated "easy", I expected this to be all, but I was wrong: With no additional
thought, this will tackle only 6 of the 17 test cases.

My next try was to re-run the code reduced such that only I/O was performed (and wrong answers were
given), to see if I/O was within the given 12s time frame. It was.

My next attempts assumed that maybe the large numbers might lead to even larger sums and thus to too
slow computations, so I did not sum up towards S, but reduced the goal S by each visited element.
No luck. 

Then I tried to early exit the linear search by looking if 

  Sum+(N-Index)*H < S
  
which is an optimization, but was not worth the effort.

Finally my brain kicked in and I took this baby more seriously.

As the number of queries T can be quite large, it makes sense to
once invest into well-prepared data, and then run the T queries against this.

- we only need to integrate/sum up our sorted list once and not T times.
- and we can beat linear search through the list of sums by some bisection
  method. At this point I remembered that I once used general balanced trees
  for a similar task and that is what I applied here too.

## Notes
- That other task was Project Euler #183

## Erlang Insights
- Another use of ``gb_trees`` and the ``gb_trees:iterator_from/2`` function 
