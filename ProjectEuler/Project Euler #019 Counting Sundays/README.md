# Project Euler #19 Counting Sundays

## Problem
http://www.hackerrank.com/contests/projecteuler/challenges/euler019/problem

We have T queries, where 1 <= T <= 100. 

For each query get two dates Date1 = (Y1, M1, D1) and Date2 = (Y2, M2, D2) and 
we have to count the first of month dates within [Date1, Date2] which are Sundays.

We further have the constraints

1900 <= y1 <= 10^16
y1 <= y2 <= (y1+1000)  

which means the intervals are only up to 1000 years wide, but they might 
lie at large numbers.

At last we get the information that (1900, 1, 1) was a Monday and
the rules for leap years:

*A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.*

## Solution
I could vaguely remember that Carl Friedrich Gauß came up with some date formulas, which featured modular arithmetics.
E.g. see [Gaußsche Wochentagsformel](https://de.wikipedia.org/wiki/Gau%C3%9Fsche_Wochentagsformel).

So I tried to roll my own.

Another inspiration was the [Unix time](https://en.wikipedia.org/wiki/Unix_time) representation, 
which counts seconds since (1970, 1, 1).

As we only care about first of month dates, we can inspect the interval just varying year and month and not day.

This would lead to 100 queries * 1000 years * 12 months = O(10^6) months to check, which should be possible
within the Erlang time limit.

To get rid of the day level we start with
```erlang
% number of first of month sundays in [date1, date2]
fom_sundays(Y1, M1, D1, Y2, M2, D2) when D1 > 1 ->
  % shift first date to next first of month
  fom_sundays(Y1, M1+1, 1, Y2, M2, D2);
fom_sundays(Y1, M1, 1, Y2, M2, D2) when M1 > 12 ->
  fom_sundays(Y1+1, 1, 1, Y2, M2, D2);
fom_sundays(Y1, M1, 1, Y2, M2, D2) when D2 > 1 ->
  % shift second date to previous first of month
  fom_sundays(Y1, M1, 1, Y2, M2, 1);
fom_sundays(Y1, M1, 1, Y2, M2, 1) ->
fom_sundays(Y1, M1, Y2, M2).
```

## Experience
Now the easy Euler problems get harder.

## Keywords
calendar, date, modular arithmetics
