-module(solution).
-export([main/0]).

main() ->
  {ok, [T]} = io:fread("", "~d"),
  L = read_list(T),
  lists:foreach(fun(N) ->
    S = s(N),
    io:format("~p~n", [S])
  end, L),
  true.

% This is the usual Fibonacci sequence, shifted by one
%   F_1 = 1
%   F_2 = 2
%   F_{n+2} = F_{n+1} + F_n
% F is even for 3|(n+1)
%
% The sequence of even numbers is
% E_1 = F_2 = 2
% E_2 = F_5 = 8
% E_3 = F_8 = 34
% E_4 = F11 = 144
%
% Assuming E_{n+2} = c1 E_{n+1} + c2 E_(n)
% we get c1 = 4 and c2 = 1 or
% E_{n+2} = 4 E_{n+1} + E_n

s(N) when N < 2 ->
  0;
s(N) when N < 8 ->
  2;
s(N) ->
  s(N, 2, 0, 0).

s(N, F1, _F2, Sum) when F1 > N ->
  Sum;
s(N, F1, F2, Sum) ->
  F = 4*F1+F2,
  s(N, F, F1, Sum+F1).


read_list(N) ->
  Fmt = string:copies("~d", N),
  {ok, L} = io:fread("", Fmt),
  L.
