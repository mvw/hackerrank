-module(solution).
-export([main/0]).

main() ->
	{ok, [M, N, R]} = io:fread("", "~d ~d ~d"),
  %io:format("M=~p N=~p R=~p~n", [M, N, R]),
  A = read_array(M, N),
  A2 = rotate(A, M, N, R),
  write_array(A2),
  ok.

rotate(A, M, N, R) ->
  K_max = (min(M, N) div 2) - 1,
  lists:foldl(fun(K, AK) ->
    rotate_ring(M, N, R, K, AK)
  end, A, lists:seq(0, K_max)).

rotate_ring(M, N, R, K, A) ->
  L1 = get_ring(M, N, K, A),
  L2 = rotate(R, L1),
  set_ring(M, N, K, A, L2).

rotate(R, A) ->
  N = array:size(A),
  lists:foldl(fun(I, AI) ->
    Ele = array:get((I+R) rem N, A),             
    array:set(I, Ele, AI)
  end, array:new(N), lists:seq(0, N-1)).

dimensions(M, N, I) ->
  W = N - 2*I,
  H = M - 2*I,
  J1 = I, 
  J2 = J1+W-1,
  I1 = I+1,
  I2 = I1+H-3,
  %io:format("W=~p H=~p J1=~p J2=~p I1=~p I2=~p~n", [W, H, J1, J2, I1, I2]),
  {W, H, J1, J2, I1, I2}.

get_ring(M, N, K, A) ->
  %io:format("get_ring(M=~p, N=~p, K=~p, A)~n", [M, N, K]),
  %write_array(A),
  {W, H, J1, J2, I1, I2} = dimensions(M, N, K),
  L1 = get_ring_1(W, I1-1, J1, J2, A),
  %write_array_row("L1=", L1),
  L2 = get_ring_2(H-2, J2, I1, I2, A),
  %write_array_row("L2=", L2),
  L3 = get_ring_3(W, I2+1, J1, J2, A),
  %write_array_row("L3=", L3),
  L4 = get_ring_4(H-2, J1, I1, I2, A),
  %write_array_row("L4=", L4),
  join(L1, L2, L3, L4).

get_ring_1(S, _I, _J1, _J2, _A) when S < 1 ->
  array:new(0);
get_ring_1(S, I, J1, J2, A) ->
  Row = array:get(I, A),
  lists:foldl(fun(J, AJ) ->
    Ele = array:get(J, Row),
    array:set(J-J1, Ele, AJ)
  end, array:new(S), lists:seq(J1, J2)).

get_ring_3(S, _I, _J1, _J2, _A) when S < 1 ->
  array:new();
get_ring_3(S, I, J1, J2, A) ->
  Row = array:get(I, A),
  lists:foldl(fun(J, AJ) ->
    Ele = array:get(J, Row),
    array:set(J2-J, Ele, AJ)
  end, array:new(S), lists:seq(J2, J1, -1)).

get_ring_2(S, _J, _I1, _I2, _A) when S < 1 ->
  array:new(0);
get_ring_2(S, J, I1, I2, A) ->
  lists:foldl(fun(I, AI) ->
    Row = array:get(I, A),
    Ele = array:get(J, Row),
    array:set(I-I1, Ele, AI)
  end, array:new(S), lists:seq(I1, I2)).

get_ring_4(S, _J, _I1, _I2, _A) when S < 1 ->
  array:new(0);                                       
get_ring_4(S, J, I1, I2, A) ->
  lists:foldl(fun(I, AI) ->
    Row = array:get(I, A),
    Ele = array:get(J, Row),
    array:set(I2-I, Ele, AI)
  end, array:new(S), lists:seq(I2, I1, -1)).

set_ring(M, N, K, A, L) ->
  %io:format("set_ring(M=~p, N=~p, K=~p, A, L)~n", [M, N, K]),
  %write_array(A),
  %write_array_row("L=", L),
  {W, H, J1, J2, I1, I2} = dimensions(M, N, K),
  {L1, L2, L3, L4} = split(W, H-2, L),
  %write_array_row("L1=", L1),
  %write_array_row("L2=", L2),
  %write_array_row("L3=", L3),
  %write_array_row("L4=", L4),
  A1 = set_ring_1(W, I1-1, J1, J2, A, L1),
  A2 = set_ring_2(H-2, J2, I1, I2, A1, L2),
  A3 = set_ring_3(W, I2+1, J1, J2, A2, L3),
  set_ring_4(H-2, J1, I1, I2, A3, L4).

set_ring_1(S, _I, _J1, _J2, A, _L) when S < 1 ->
  A;
set_ring_1(_S, I, J1, J2, A, L) ->
  Row = array:get(I, A),
  Row2 = lists:foldl(fun(J, AJ) ->
    Ele = array:get(J-J1, L),
    array:set(J, Ele, AJ)
  end, Row, lists:seq(J1, J2)),
  array:set(I, Row2, A).

set_ring_3(S, _I, _J1, _J2, A, _L) when S < 1 ->
  A;
set_ring_3(_S, I, J1, J2, A, L) ->
  Row = array:get(I, A),
  Row2 = lists:foldl(fun(J, AJ) ->
    Ele = array:get(J2-J, L),
    array:set(J, Ele, AJ)
  end, Row, lists:seq(J2, J1, -1)),
  array:set(I, Row2, A).

set_ring_2(S, _J, _I1, _I2, A, _L) when S < 1 ->
  A;
set_ring_2(_S, J, I1, I2, A, L) ->
  lists:foldl(fun(I, AI) ->
    Ele = array:get(I-I1, L),
    Row = array:get(I, AI),
    Row2 = array:set(J, Ele, Row),
    array:set(I, Row2, AI)
  end, A, lists:seq(I1, I2)).

set_ring_4(S, _J, _I1, _I2, A, _L) when S < 1 ->
  A;
set_ring_4(_S, J, I1, I2, A, L) ->
  lists:foldl(fun(I, AI) ->
    Ele = array:get(I2-I, L),
    Row = array:get(I, AI),
    Row2 = array:set(J, Ele, Row),
    array:set(I, Row2, AI)
  end, A, lists:seq(I2, I1, -1)).

join(A1, A2, A3, A4) ->
  L1 = array:to_list(A1),
  L2 = array:to_list(A2),
  L3 = array:to_list(A3),
  L4 = array:to_list(A4),
  L = L1 ++ L2 ++ L3 ++ L4,
  array:from_list(L).

split(N1, N2, A) ->
  I1 = N1,
  I2 = I1 + N2,
  I3 = I2 + N1,
  I4 = I3 + N2,
  A1 = lists:foldl(fun(I, AI) ->
    Ele = array:get(I, A),
    array:set(I, Ele, AI)                 
  end, array:new(N1), lists:seq(0, I1-1)),
  A2 = lists:foldl(fun(I, AI) ->
    Ele = array:get(I, A),
    array:set(I-I1, Ele, AI)                 
  end, array:new(N2), lists:seq(I1, I2-1)),
  A3 = lists:foldl(fun(I, AI) ->
    Ele = array:get(I, A),
    array:set(I-I2, Ele, AI)                 
  end, array:new(N1), lists:seq(I2, I3-1)),
  A4 = lists:foldl(fun(I, AI) ->
    Ele = array:get(I, A),
    array:set(I-I3, Ele, AI)                 
  end, array:new(N2), lists:seq(I3, I4-1)),
  {A1, A2, A3, A4}.

% A: array of rows, row: array of elements
  
read_array(M, N) ->
  A = array:new(M),
  read_array(M, N, 1, A).

read_array(M, _N, I, A) when I > M ->
  A;
read_array(M, N, I, A) ->
  Fmt = "~d" ++ string:copies(" ~d", N-1),
  {ok, L} = io:fread("", Fmt),
  Row = array:from_list(L),
  A2 = array:set(I-1, Row, A),
  read_array(M, N, I+1, A2).


write_array(A) ->
  M = array:size(A),
  lists:foreach(fun(I) ->
    Row = array:get(I, A),
    write_array_row(Row)
  end, lists:seq(0, M-1)).

write_array_row(Row) ->
  N = array:size(Row),
  lists:foreach(fun(J) ->
    Ele = array:get(J, Row),
    io:format("~p ", [Ele])
  end, lists:seq(0, N-1)),
  io:format("~n").

write_array_row(S, Row) ->
  io:format("~s", [S]),
  write_array_row(Row).
