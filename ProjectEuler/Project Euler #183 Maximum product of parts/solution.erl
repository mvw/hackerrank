-module(solution).
-export([main/0]).

main() ->
  {ok, [Q]} = io:fread("", "~d"),
  L = read_list(Q),
  work(L),
  ok.


% q/r is terminating if q/r = q'/10^k
% So there must be a solution q' for the equation
%   q 10^k = q' r
%   q 2^k 5^k = q' r
% looking at the prime factors this means we need the prime factors of r,
% which are different from 2 and 5 to show up in q and
% having exponents as least as large as those in r.

% Q1=N^K R1=K^K so we compare
% p_1^{K+e_1} p_2^{K+e_2} .. 2^T 5^T = Q' p_1^{K+f_1} p_2^{K+f_2} ..
% we can choose T as large as we want, so 2 and 5 play no role in r,
% because we can match them by large enough T
% we remove the 2 and 5 divisors from r, getting r'
% we can ignore the K exponentiation on both sides, they just lead
% to an additive constant K in each exponent on each side
% finally we just test if r' divides q
terminating(Q, R) ->
  R2 = remove(R, 5),
  R3 = remove(R2, 2),
  Q rem R3 =:= 0.


remove(R, P) ->
  case R rem P =:= 0 of
    false ->
      R;
    true ->
      remove(R div P, P)
 end.


% work through list of queries
% keep a map of D values and a gb_tree of S values
work(L) ->
  work(L, maps:new(), gb_trees:empty()).

work([], _DMap, _STree) ->
  %io:format("DMap: ~p~n", [_DMap]),
  %io:format("STree: ~p~n", [_STree]),
  ok;
work([SmallN|T], DMap, STree) ->
  {Sum, DMap2, STree2} = query(SmallN, DMap, STree),
  io:format("~p~n", [Sum]),
  work(T, DMap2, STree2). 


query(SmallN, DMap, STree) ->
  % returns Key >= SmallN (if present)
  Iter = gb_trees:iterator_from(SmallN, STree),
  Next = gb_trees:next(Iter),
  case Next of
    none ->
      case gb_trees:is_empty(STree) of
        false ->
          {N1, Sum1} = gb_trees:largest(STree);
        true ->
          N1 = 4,
          Sum1 = 0
      end,
      {Sum, DMap2} = query_up(N1, Sum1, SmallN, DMap),
      STree2 = gb_trees:insert(SmallN, Sum, STree);
    {N2, Sum2, _Iter2} ->
      % TODO we might try lower values as well
      case N2 =:= SmallN of
        true ->
          Sum = Sum2,
          DMap2 = DMap,
          STree2 = STree;
        false ->
          {Sum, DMap2} = query_down(N2, Sum2, SmallN, DMap),
          STree2 = gb_trees:insert(SmallN, Sum, STree)
      end
  end,
  {Sum, DMap2, STree2}.


query_up(N1, Sum1, SmallN, DMap) ->
  lists:foldl(fun(N, {SumN, DMapN}) ->
    {D, DMap2} = d(N, DMapN),
    SumN2 = SumN + D,
    {SumN2, DMap2}
  end, {Sum1, DMap}, lists:seq(N1+1, SmallN)).


query_down(N2, Sum2, SmallN, DMap) ->
  lists:foldl(fun(N, {SumN, DMapN}) ->
    {D, DMap2} = d(N, DMapN),
    SumN2 = SumN - D,
    {SumN2, DMap2}
  end, {Sum2, DMap}, lists:seq(N2, SmallN+1, -1)).


d(N, DMap) ->
  case maps:is_key(N, DMap) of
    false ->
      D = d2(N),
      DMap2 = maps:put(N, D, DMap);
    true ->
      D = maps:get(N, DMap),
      DMap2 = DMap
  end,
  {D, DMap2}.

  
d2(N) ->
  R = m(N),
  %io:format("Q=~p R=~p~n", [N, R]),
  case terminating(N, R) of
    false ->
      N;
    true ->
      -N
  end.


% P(K) = (N/K)^K
% ln(P(K))' = ln(N)-ln(K)-1 = 0 <=> K=exp(ln(N)-1) (usually not whole)
% we return q/r in the reduced form (N,K), not (N^K, K^K)
m(N) ->
  K = math:exp(math:log(N)-1),
  %io:format("K=~p~n", [K]),
  K1 = floor(K),
  K2 = K1+1,
  %case pow(K2, K2) < N * pow(K1, K1) of
  case K2*math:log10(K2/K1) < math:log10(N/K1) of
    true ->
      K2;
    false ->
      K1
  end.


pow(_X, 0) ->
  1;
pow(X, 1) ->
  X;
pow(X, Y) ->
  X*pow(X, Y-1).


read_list(Q) ->
  Fmt = string:copies("~d", Q),
  {ok, L} = io:fread("", Fmt),
  L.
