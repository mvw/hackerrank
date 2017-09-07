-module(solution).
-export([main/0]).

main() ->
  {ok, [T]} = io:fread("", "~d"),
  L = read_list(T),
  lists:foreach(fun(N) ->
    Sum = sum(N),
    io:format("~p~n", [Sum])
  end, L),
  true.


% sum all 0 < k < N, where 3|k or 5|k

% The summation pattern repeats every 15 summands:
%   00 01 02 03 04 05 06 07 08 09 10 11 12 13 14
%   1  0  0  1  0  1  1  0  0  1  1  0  1  0  0
sum(N) ->
  Q = N div 15,
  R = N-15*Q,
  % over the q-1 blocks:
  % 0+15+30+45+..+(q-1)*15=15(0+1+2+3+..(q-1)) = 15 S(q-1)
  % 3+18+33+48+..+(q-1)*15+3 = 3 q + 15 S(q-1)
  % 5+20+35+50+..+(q-1)*15+5 = 5 q + 15 S(q-1)
  % ..
  % 12 q + 15 S(q-1)
  % = 7*15 S(q-1) + q(0+3+5+6+9+10+12) = 7*15 S(q-1) + 45q
  Block = 7*15*s(Q-1)+45*Q,
  % rest:
  Rest = number(R)*15*Q + sigma(R),
  Block + Rest.

% number of involved positions < R
number(0) -> 0;
number(1) -> 1;
number(2) -> 1;
number(3) -> 1;
number(4) -> 2;
number(5) -> 2;
number(6) -> 3;
number(7) -> 4;
number(8) -> 4;
number(9) -> 4;
number(10) -> 5;
number(11) -> 6;
number(12) -> 6;
number(13) -> 7;
number(14) -> 7.


% sum of involved positions < R
sigma(0) -> 0;
sigma(1) -> 0;
sigma(2) -> 0;
sigma(3) -> 0;
sigma(4) -> 3;
sigma(5) -> 3;
sigma(6) -> 8;
sigma(7) -> 14;
sigma(8) -> 14;
sigma(9) -> 14;
sigma(10) -> 23;
sigma(11) -> 33;
sigma(12) -> 33;
sigma(13) -> 45;
sigma(14) -> 45.


% sum k from 1 to N
s(N) ->
  (N*(N+1)) div 2.


read_list(N) ->
  Fmt = string:copies("~d", N),
  {ok, L} = io:fread("", Fmt),
  L.
