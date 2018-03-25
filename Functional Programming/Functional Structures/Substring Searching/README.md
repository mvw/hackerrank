# Substring Searching

## Problem
https://www.hackerrank.com/challenges/kmp-fp/problem

## Solution
For T with 1 <= T <= 10 we get a list of T pairs text_i and pat_i.

We have to determine in each case if the pattern is a substring of 
the text or not.

For the string lengths we have
1 <= n=|pat| <= m=|text| <= 10^5, only lower case letters will show up.

The problem refers to the Knuth-Morris-Pratt algorithm.
It has a complexity O(m+n).

## Experience
Somewhat lazy to start implementing the KMP algorithm I first tried to make use of
two Erlang library functions:
- `string:find(Text, Pat)` (score 27.48)
- `re:match(Text, Pat)` (score 18.47)

I was a bit surprised, that both did not get the full score of 50 points,
failing on the last test cases, which are usually larger problem instances.

Then I implemented KMP from pseudo code found in the German language
Wikipedia [article](https://de.wikipedia.org/wiki/Knuth-Morris-Pratt-Algorithmus#Implementierung)

There might be more elegant versions, this is just a quick port of the 
imperative pseudo code.

This solution passed all tests. 

Afterwards I took a look at other Erlang entries.

One was amusing, because it had to resort to some pre calculated
result.
And one managed to come with a fast enough Erlang library call
- `binary:match(BinText, BinPat)`

where the strings were converted by `list_to_binary/1`.

