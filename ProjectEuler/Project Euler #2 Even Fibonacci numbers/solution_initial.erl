-module(solution_initial).
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
%   F_{n+2} = F_n + F_{n+1}
% F is even for 3|(n+1)
% The recurrence is solved by
%   F_n = k_1 r_1^n + k_2 r_2^n
% with
%   r1 = (1+sqrt(5))/2 =  1.618..
%   r2 = (1-sqrt(5))/2 = -0.618..
%   r1-2=
%   A k = b <=> k = A^-1 b  
%   A = [r1 r2; r1^2 r2^2]  b = [1; 2]
%   A^-1 = 1/det(A) [r2^2 -r2; -r1^2 r1]
%   det(A) = sqrt(5)
%   [k1; k2] = 1/sqrt(5) [r2^2-2r2; -r1^2+2r1]
%            = 1/sqrt(5) [r1; -r2]
% So
%   F_(3K-1) = (R1^3K - R2^3K)/det(A)
%            = R1^3K/det(A) + delta 
% with |delta| < 1.
%   
%   S(K) = sum_{k=1}^K F_{3k-1}
%        = sum_{k=1}^K k_1 r_1^(3k-1) + k_2 r_2^(3k-1)
% for k=1 to F_(3K-1) =< N 
% so trunc(r1^3K/det(A)) <= N and
%    K=trunc(log_r1^3(N*det(A)+1/2))-1, log_b(x) = log(x)/log(b)
% Geometric series:
%   G(K, q) = sum_(k=0)^K q^k = (q^(K+1)-1)/(q-1)
%   S = k_1 r_1^2 G(K, r_1^3) + k_2 r_2^2 G(K, r_2^3)

s(N) ->
  Sqrt5 = math:sqrt(5),
  R1 = (1+Sqrt5)/2.0,
  R2 = (1-Sqrt5)/2.0,
  Det = Sqrt5,
  %K1 = R1/Det,
  %K2 = -R2/Det,
  R1S = R1*R1,
  R1C = R1S*R1,
  R2S = R2*R2,
  R2C = R2S*R2,
  K = trunc(ln(R1C, N*Sqrt5+0.5))-1,
  S = (R1C*g(K, R1C) - R2C*g(K, R2C)) / Det,
  round(S).


ln(B, X) ->
  math:log(X) / math:log(B).


g(K, Q) ->
  (pow(Q, K+1) - 1) / (Q - 1).


pow(_B, 0) ->
  1;
pow(B, E) ->
  B * pow(B, E-1).


read_list(N) ->
  Fmt = string:copies("~d", N),
  {ok, L} = io:fread("", Fmt),
  L.
