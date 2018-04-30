-module(solution).
-export([main/0]).

% "Simplify the Algebraic Expressions"

debug() ->
  false.


main() ->
  {ok, [T]} = io:fread("", "~d"),
  lists:foreach(fun(_I) ->
    case io:get_line("") of
      eof ->
        error;
      Prog ->
        run(Prog)
    end
  end, lists:seq(1, T)),
  true.
  

run(Prog) ->
  debug("Prog=~p~n", [Prog]),
  case tokenize(Prog) of
    error ->
      error;
    Tokens ->
      debug("Tokens=~p~n", [Tokens]),
      case parse(Tokens) of
        error ->
          io:format("error~n"),
          error;
        Tree ->
          debug("Tree=~p~n", [Tree]),
          P = eval(Tree),
          debug("P=~p~n", [P]),
          print(P)
      end
  end.


% Pass 3: execution
% return a map of the result polynomial
eval({Op, Tree1, Tree2}) ->
  P1 = eval(Tree1),
  P2 = eval(Tree2),
  case Op of 
    op_add ->
      eval_add(P1, P2);
    op_sub ->
      eval_sub(P1, P2);
    op_mult ->
      eval_mult(P1, P2);
    op_div ->
      eval_div(P1, P2);
    op_exp ->
      eval_exp(P1, P2)
  end;
eval({op_minus, Tree}) ->
  P = eval(Tree),
  eval_minus(P);
eval(P) ->
  P.


eval_add(P1, P2) ->
  lists:foldl(fun(K, Map) ->
    C1 = maps:get(K, P1),
    C2 = maps:get(K, P2),
    C = C1+C2,
    maps:put(K, C, Map)            
  end, maps:new(), lists:seq(0, 5)).


eval_sub(P1, P2) ->
  lists:foldl(fun(K, Map) ->
    C1 = maps:get(K, P1),
    C2 = maps:get(K, P2),
    C = C1-C2,
    maps:put(K, C, Map)            
  end, maps:new(), lists:seq(0, 5)).


eval_mult(P1, P2) ->
  {A0, A1, A2, A3, A4, A5} = p(P1),
  {B0, B1, B2, B3, B4, B5} = p(P2),
  %
  C0 = A0*B0,
  C1 = A0*B1+A1*B0,
  C2 = A0*B2+A1*B1+A2*B0,
  C3 = A0*B3+A1*B2+A2*B1+A3*B0,
  C4 = A0*B4+A1*B3+A2*B2+A3*B1+A4*B0,
  C5 = A0*B5+A1*B4+A2*B3+A3*B2+A4*B1+A5*B0,
  %
  p(C0, C1, C2, C3, C4, C5).


eval_div(P1, P2) ->
  C2 = maps:get(0, P2),
  lists:foldl(fun(K, Map) ->
    C1 = maps:get(K, P1),
    C = rational(C1, C2),
    maps:update(K, C, Map)
  end, P1, lists:seq(0, 5)).


eval_exp(P1, P2) ->
  Deg1 = degree(P1),
  case Deg1 of
    1 ->
      C1 = maps:get(1, P1),
      C2 = maps:get(0, P2),
      case C2 of
        0 ->
          p(C1, 0, 0, 0, 0, 0);
        1 ->
          p(0, C1, 0, 0, 0, 0);
        2 ->
          p(0, 0, C1, 0, 0, 0);
        3 ->
          p(0, 0, 0, C1, 0, 0);
        4 ->
          p(0, 0, 0, 0, C1, 0);
        5 ->
          p(0, 0, 0, 0, 0, C1)
      end;
    0 ->
      C1 = maps:get(0, P1),
      C2 = maps:get(0, P2),
      C = pow(C1, C2),
      p(C, 0, 0, 0, 0, 0)
  end.


eval_minus(P) ->
  lists:foldl(fun(K, Map) ->
    C = maps:get(K, P),
    maps:update(K, -C, Map)
  end, P, lists:seq(0, 5)).


% print the polynomial map
print(P) ->
  Deg = degree(P),
  print_deg(Deg, P),
  io:format("~n").


% highest degree monomial
print_deg(Deg, P) ->
  case maps:get(Deg, P) of
    1 ->
      ok;
    -1 ->
      io:format("-");
    C ->
      io:format("~p", [C])
  end,
  print_x(Deg),
  print(Deg-1, P).


% lesser degree monomials
print(-1, _P) ->
  ok;
print(K, P) ->
  case maps:get(K, P) of
    0 ->
      ok;
    C ->
      case C > 0 of
        true ->
          io:format(" + ");
        false ->
          io:format(" - ")
      end,
      case C of
        1 ->
          ok;
        -1 ->
          ok;
        _Else ->
          io:format("~p", [abs(C)])
      end,
      print_x(K)
  end,
  print(K-1, P).


% print x^k
print_x(0) ->
  ok;
print_x(1) ->
  io:format("x");
print_x(K) ->
  io:format("x^~p", [K]).


% degree of polynomial
degree(P) ->
  degree(5, P).

degree(0, _P) ->
  0;
degree(K, P) ->
  C = maps:get(K, P),
  case C =:= 0 of
    false ->
      K;
    true ->
      degree(K-1, P)
  end.


% coefficients to map
p(A0, A1, A2, A3, A4, A5) ->
  M0 = maps:put(0, A0, maps:new()),
  M1 = maps:put(1, A1, M0),
  M2 = maps:put(2, A2, M1), 
  M3 = maps:put(3, A3, M2),
  M4 = maps:put(4, A4, M3),
  maps:put(5, A5, M4).


% map to coefficients
p(P) ->
  A0 = maps:get(0, P),
  A1 = maps:get(1, P),
  A2 = maps:get(2, P),
  A3 = maps:get(3, P),
  A4 = maps:get(4, P),
  A5 = maps:get(5, P),
  {A0, A1, A2, A3, A4, A5}.


pow(_X, 0) ->
  1;
pow(X, Y) ->
  X*pow(X, Y-1).


rational(P, Q) when Q =:= 1 ->
  P;
rational(P, Q) when Q < 0 ->
  rational(-P, -Q);
rational(P, Q) ->
  D = gcd(P, Q),
  case D =:= 1 of
    true ->
      {P, Q};
    false ->
      P2 = P div D,
      case Q =:= D of
        true ->
          P2;
        false ->
          Q2 = Q div D,
          {P2, Q2}
      end
  end.


gcd(A, B) when B =:= 0 ->
  A;
%gcd(A, B) when A < 0 ->
%  gcd(-A, B);
gcd(A, B) ->
  gcd(B, A rem B).


% Pass 2: syntactic analysis
% return a syntax tree

% List of nodes:
%   Map : k -> a_k
%   {op_minus, Tree}
%   {op_add, TreeA1, TreeA2}
%   {op_sub, TreeA1, TreeA2}
%   {op_mult, TreeA1, TreeA2}
%   {op_div, TreeA1, TreeA2}
%   {op_exp, TreeA1, TreeA2}

% Program -> a
parse(Tokens) ->
  case a(Tokens) of
    error ->
      error;
    {[], Tree} ->
      Tree;
    {TokensA, _Tree} ->
      error_unexpected_token(TokensA)
  end.


% we implement the production
%   a -> x | n | a op_a a | a a | (a)
% by
%   a -> -^{0,1} a_term (+ a_term | - a_term)^*
%   a_term -> a_factor (* a_factor | / a_factor | a_factor)^*
%   a_factor -> a_base (^ a_base)^*
%   a_base -> x | n | ( a )
% to get the operator precedence working

a([{op_a, op_sub, _Col}|Tokens]) ->
  case a_term(Tokens) of
    error ->
      error;
    {Tokens2, TreeA} ->
      Tree = tree(op_minus, TreeA),
      a_repeat(Tokens2, Tree)
  end;
a(Tokens) ->
  case a_term(Tokens) of
    error ->
      error;
    {Tokens2, tree_empty} ->
      error_unexpected_token(Tokens2);
    {Tokens2, Tree} ->
      a_repeat(Tokens2, Tree)
  end.


a_repeat([{op_a, Op, _Col}|Tokens], Tree) when Op =:= op_add; Op =:= op_sub ->
  case a_term(Tokens) of
    error ->
      error;
    {Tokens2, tree_empty} ->
      error_unexpected_token(Tokens2);
    {Tokens2, Tree2} ->
      Tree3 = tree(Op, Tree, Tree2),
      a_repeat(Tokens2, Tree3)
  end;
a_repeat(Tokens, Tree) ->
  {Tokens, Tree}.


a_term(Tokens) ->
  case a_factor(Tokens) of
    error ->
      error;
    {Tokens2, Tree} ->
      a_term_repeat(Tokens2, Tree)
  end.


a_term_repeat([{op_a, Op, _Col}|Tokens], Tree) when Op =:= op_mult; Op =:= op_div ->
  case a_factor(Tokens) of
    error ->
      error;
    {Tokens2, Tree2} ->
      Tree3 = tree(Op, Tree, Tree2),
      a_term_repeat(Tokens2, Tree3)
  end;
a_term_repeat(Tokens, Tree) ->
  case a_factor(Tokens) of
    error ->
      error;
    {Tokens2, tree_empty} ->
      {Tokens2, Tree};
    {Tokens2, Tree2} ->
      Tree3 = tree(op_mult, Tree, Tree2),
      a_term_repeat(Tokens2, Tree3)
  end.


a_factor(Tokens) ->
  case a_base(Tokens) of
    error ->
      error;
    {Tokens2, Tree} ->
      a_factor_repeat(Tokens2, Tree)
  end.


a_factor_repeat([{op_a, Op, _Col}|Tokens], Tree) when Op =:= op_exp ->
  case a_base(Tokens) of
    error ->
      error;
    {Tokens2, Tree2} ->
      Tree3 = tree(Op, Tree, Tree2),
      a_factor_repeat(Tokens2, Tree3)
  end;
a_factor_repeat(Tokens, Tree) ->
  {Tokens, Tree}.


a_base([{x, _Col}|Tokens]) ->
  % a_base -> x
  Node = p(0, 1, 0, 0, 0, 0),
  Tree = tree(Node),
  {Tokens, Tree};
a_base([{num, Number, _Col}|Tokens]) ->
  % a_base -> n
  Node = p(Number, 0, 0, 0, 0, 0),
  Tree = tree(Node),
  {Tokens, Tree};
a_base([{paren_open, _Col}|Tokens]) ->
  % a_base -> ( a )
  case a(Tokens) of
    error ->
      error;
    {TokensA, TreeA} ->
      a_open(TokensA, TreeA)
  end;
a_base(Tokens) ->
  {Tokens, tree_empty}.


% "( a" already recognized
a_open([{paren_close, _Col}|Tokens], TreeA) ->
  {Tokens, TreeA};
a_open(Tokens, _TreeA) ->
  error_expected_token(paren_close, Tokens).


% Tree helpers

tree(Node) ->
  Node.

tree(Node, Tree) ->
  {Node, Tree}.

tree(Node, Tree1, Tree2) ->
  {Node, Tree1, Tree2}.


% error helpers
error_unexpected_token([]) ->
  io:format("internal error: error_unexpected_token([]) called ~n"),
  error;
error_unexpected_token([Token|_T]) ->
  {TokenType, Col} = reduce_token(Token),
  io:format("Unexpected token ~p at column ~p~n", [TokenType, Col]),
  error.


error_expected_token(Expected, []) ->
  io:format("internal error: error_expected_token(~p, []) called~n", [Expected]),
  error;
error_expected_token(Expected, [Token|_T]) ->
  {TokenType, Col} = reduce_token(Token),
  io:format("Expected token ~p but found ~p at column ~p~n", [Expected, TokenType, Col]),
  error.


reduce_token({TokenType, _Arg, Col}) ->
  {TokenType, Col};
reduce_token({TokenType, Col}) ->
  {TokenType, Col}.


% Pass 1: lexical analysis
% recognize program text as sequence of tokens or return 'error'

% List of tokens:
%   {op_a, Op, Col}  Op = op_add | op_sub | op_mult | op_div | op_exp
%   {x, Col}
%   {num, Number, Col}
%   {paren_open, Col}
%   {paren_close, Col}

tokenize(Prog) ->
  Tokens = [],
  Col = 0,
  case tokenize(Prog, Col, Tokens) of
    error ->
      error;
    Tokens2 ->
      lists:reverse(Tokens2)
  end.


% iterate over characters of a single line
tokenize([], _Col, Tokens) ->
  Tokens;
tokenize([H|T], Col, Tokens) when H =:= $x ->
  tokenize_name(T, Col, Tokens);
tokenize([H|T], Col, Tokens) when H >= $0, H =< $9 ->
  tokenize_number(T, Col, [H], Col+1, Tokens);
tokenize([H|T], Col, Tokens) when H =:= $+; H =:= $-; H =:= $*; H =:= $/; H =:= $^ ->
  case H of
    $+ ->
      Op = op_add;
    $- ->
      Op = op_sub;
    $* ->
      Op = op_mult;
    $/ ->
      Op = op_div;
    $^ ->
      Op = op_exp
  end,
  Token = {op_a, Op, Col},
  tokenize(T, Col+1, [Token|Tokens]);
tokenize([H|T], Col, Tokens) when H =:= $( ->
  Token = {paren_open, Col},
  tokenize(T, Col+1, [Token|Tokens]);
tokenize([H|T], Col, Tokens) when H =:= $) ->
  Token = {paren_close, Col},
  tokenize(T, Col+1, [Token|Tokens]);
tokenize([H|T], Col, Tokens) when H =:= $ ; H =:= $\n; H =:= $\r; H =:= $\t ->
  % ignore white space now
  tokenize(T, Col+1, Tokens);
tokenize([H|_L], Col, _Tokens) ->
  io:format("Unexpected character ~c at column ~p~n", [H, Col]),
  error.


% name
tokenize_name(L, Col, Tokens) ->
  Token = {x, Col},
  tokenize(L, Col+1, [Token|Tokens]).


% number
tokenize_number([], ColStart, Digits, _Col, Tokens) ->
  Token = digits_to_token(Digits, ColStart),
  [Token|Tokens];
tokenize_number([H|T], ColStart, Digits, Col, Tokens) when H >= $0, H =< $9 ->
  tokenize_number(T, ColStart, [H|Digits], Col+1, Tokens);
tokenize_number(L, ColStart, Digits, Col, Tokens) ->
  Token = digits_to_token(Digits, ColStart),
  tokenize(L, Col, [Token|Tokens]).


digits_to_token(Digits, Col) ->
  Number = list_to_integer(lists:reverse(Digits)),
  {num, Number, Col}.


% debug/0 defined at top of file
debug(Fmt, Args) ->
  case debug() of
    false ->
      ok;
    true ->
      io:format(Fmt, Args)
  end.
