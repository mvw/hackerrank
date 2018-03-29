-module(solution).
-export([main/0]).

% this solution uses the Sparse Table algorithm from
% http://www.cs.sunysb.edu/~bender/pub/JALG05-daglca.pdf

main() ->
  {ok, [N, M]} = io:fread("", "~d~d"),
  LA = read_data(N),
  LR = read_data(2*M),
  A = array(LA),
  RMQ = rmq(A),
  query(LR, RMQ),
  true.


% perform queries
query([], _RMQ) ->
  true;
query([L,R|T], RMQ) ->
  Min = rmq_min(L, R, RMQ),
  io:format("~p~n", [Min]),
  query(T, RMQ).


% determine minimum for range query
rmq_min(L, R, RMQ) ->
  {K, TwoK} = k(L, R),
  A1 = maps:get({L, K}, RMQ),
  A2 = maps:get({R-TwoK+1, K}, RMQ),
  min(A1, A2).


% k = floor(log_2(R-L+1))
k(L, R) ->
  K = -1,
  TwoK = 1,
  W = R-L+1,
  k(K, TwoK, W).

k(K, TwoK, W) when TwoK > W ->
  {K, TwoK div 2};
k(K, TwoK, W) ->
  k(K+1, 2*TwoK, W).

 
% return M_ij table
rmq(A) ->
  N = maps:size(A),
  M0 = rmq0(N, A),
  rmq(N, M0).

% initialize for intervals of length 2^0
rmq0(N, A) ->
  I = 0,
  M = maps:new(),                 
  rmq_0(I, N, A, M).

rmq_0(I, N, _A, M) when I =:= N ->
  M;
rmq_0(I, N, A, M) ->
  AI = maps:get(I, A),
  M2 = maps:put({I, 0}, AI, M),
  rmq_0(I+1, N, A, M2).

% calculate bigger intervals
rmq(N, M) ->
  J = 1,
  TwoJ = 2,
  rmq(J, TwoJ, N, M).

rmq(_J, TwoJ, N, M) when TwoJ > N ->
  M;
rmq(J, TwoJ, N, M) ->
  I = 0,
  IMax = I+TwoJ-1,
  M2 = rmq(I, IMax, J, TwoJ, N, M),
  rmq(J+1, 2*TwoJ, N, M2).

rmq(_I, IMax, _J, _TwoJ, N, M) when IMax =:= N ->
  M;
rmq(I, IMax, J, TwoJ, N, M) ->
  JM = J-1,
  I2 = I+(1 bsl JM),
  A1 = maps:get({I, JM}, M),
  A2 = maps:get({I2, JM}, M),
  MIJ = min(A1, A2),
  M2 = maps:put({I, J}, MIJ, M),
  rmq(I+1, IMax+1, J, TwoJ, N, M2).


% array map starting at index 0
array(L) ->
  I=0,
  Map = maps:new(),
  array(L, I, Map).

array([], _I, Map) ->
  Map;
array([H|T], I, Map) ->
  Map2 = maps:put(I, H, Map),
  array(T, I+1, Map2).


read_data(N) ->
  Fmt = unicode:characters_to_list(lists:duplicate(N, "~d")),
  {ok, L} = io:fread("", Fmt),
  L.
