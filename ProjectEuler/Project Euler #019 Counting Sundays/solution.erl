-module(solution).
-export([main/0]).

main() ->
  {ok, [T]} = io:fread("", "~d"),
  L = read_list(6*T),
  lists:foldl(fun(_I, LI) ->
    [Y1, M1, D1, Y2, M2, D2|TI] = LI,
    Result = fom_sundays(Y1, M1, D1, Y2, M2, D2),
    io:format("~p~n", [Result]),
    TI
  end, L, lists:seq(1, T)),
  true.


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


% start counting, checking up to 1000 years
fom_sundays(Y1, M1, Y2, M2) ->
  N = 0,
  fom_sundays(Y1, M1, Y2, M2, N).

fom_sundays(Y1, M1, Y2, M2, N) when M1 > 12 ->
  fom_sundays(Y1+1, 1, Y2, M2, N);
fom_sundays(Y1, _M1, Y2, _M2, N) when Y1 > Y2 ->
  N;
fom_sundays(Y1, M1, Y2, M2, N) when Y1 =:= Y2, M1 > M2 ->
  N;
fom_sundays(Y1, M1, Y2, M2, N) ->
  Day = day(Y1, M1, 1),
  case (Day rem 7) =:= 6 of
    true ->
      N2 = N+1;
    false ->
      N2 = N
  end,
  fom_sundays(Y1, M1+1, Y2, M2, N2).


% number of days since (Y, 1, 1)
day(Y, M, D) ->
  Years = Y-1900,
  Days = D-1,
  D1 = Years*365,
  LeapYears = leap_years(Y-1),
  DY = D1 + LeapYears,
  LeapYear = leap_years(Y)-LeapYears,
  DY + day_m(M, LeapYear) + Days.


% leap years from 1900 until Y (if all years are full)
leap_years(Y) ->
  L1 = (Y div 4)-474,
  L2 = (Y div 100)-18,
  L3 = (Y div 400)-4,
  L1-L2+L3.


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


read_list(N) ->
  Fmt = unicode:characters_to_list(lists:duplicate(N, "~d")),
  {ok, L} = io:fread("", Fmt),
  L.
