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

The main clause of the counting is:
```erlang
fom_sundays(Y1, M1, Y2, M2, N) ->
  Day = day(Y1, M1, 1),
  case (Day rem 7) =:= 6 of
    true ->
      N2 = N+1;
    false ->
      N2 = N
  end,
fom_sundays(Y1, M1+1, Y2, M2, N2).
```

Where day/3 returns the number of days since (1900, 1, 1). 

The leap year rules have been encoded into
```erlang
% leap years from 1900 until Y (if all years are full)
leap_years(Y) ->
  L1 = (Y div 4)-474,
  L2 = (Y div 100)-18,
  L3 = (Y div 400)-4,
  L1-L2+L3.
```
L1 gives the number of years divisible by 4 from 1900 to Y.

L2 gives the number of years divisible by 100 from 1900 to Y.

L3 gives the number of years divisible by 400 from 1900 to Y.

This suffices to calculate the number of days on the years level.

As it is O(1), we stay within the O(10^6) estimation.

However we also consider partial years via the month part of the given dates.
So we have to take care of the different month lengths and if
we have a leap day or not.

I decided to code the different months as Erlang clauses with a little recursion
to spare me summing up the different length of months:
```erlang
day_m(1, _LeapYear) -> 0;
day_m(2, LeapYear) -> day_m(1, LeapYear)+31;
day_m(3, LeapYear) -> day_m(2, LeapYear)+28+LeapYear;
day_m(4, LeapYear) -> day_m(3, LeapYear)+31;
day_m(5, LeapYear) -> day_m(4, LeapYear)+30;
day_m(6, LeapYear) -> day_m(5, LeapYear)+31;
day_m(7, LeapYear) -> day_m(6, LeapYear)+30;
day_m(8, LeapYear) -> day_m(7, LeapYear)+31;
day_m(9, LeapYear) -> day_m(8, LeapYear)+31;
day_m(10, LeapYear) -> day_m(9, LeapYear)+30;
day_m(11, LeapYear) -> day_m(10, LeapYear)+31;
day_m(12, LeapYear) -> day_m(11, LeapYear)+30.
```

Both parts are combined by this code:
```erlang
% number of days since (Y, 1, 1)
day(Y, M, D) ->
  Years = Y-1900,
  Days = D-1,
  D1 = Years*365,
  LeapYears = leap_years(Y-1),
  DY = D1 + LeapYears,
  LeapYear = leap_years(Y)-LeapYears,
  DY + day_m(M, LeapYear) + Days.
```

Comparing the above with Gauss formula for (Y, 1, 1)

w = (1 + 5 (A-1) mod 4 + 4 ( (A-1) mod 100) + 6 ( (A-1) mod 400)) mod 7

we see a little resemblence.


## Experience
Now the easy Euler problems get harder.

## Keywords
calendar, date, modular arithmetics
