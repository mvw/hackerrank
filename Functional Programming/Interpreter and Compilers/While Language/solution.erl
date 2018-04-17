-module(solution).
-export([main/0]).

% Interpreter for "While Language" grammar.

% This tries to be a recursive descent parser with one token look ahead.
% A pure interpreter would use combined parsing and evaluation,
% while this program produces a syntax tree and then evaluates it,
% which is more compiler like.

main() ->
  Prog = read_prog(),
  case tokenize(Prog) of
    error ->
      error;
    Tokens ->
      %io:format("Tokens=~p~n", [Tokens]),
      case parse(Tokens) of
        error ->
          io:format("error~n"),
          error;
        Tree ->
          %io:format("Tree=~p~n", [Tree]),
          Vars = eval(Tree),
          list_vars(Vars)
      end
  end.


% Pass 3: execution
% return the final values of the introduced variables
eval(Tree) ->
  % example
  Vars = maps:new(),
  eval_s(Tree, Vars).

% statement, return variables because of possible assignment
eval_s({op_assign, Name, TreeA}, Vars) ->
  A = eval_a(TreeA, Vars),
  case maps:is_key(Name, Vars) of
    false ->
      maps:put(Name, A, Vars);
    true ->
      maps:update(Name, A, Vars)
  end;
eval_s({semicolon, TreeS1, TreeS2}, Vars) ->
  Vars1 = eval_s(TreeS1, Vars),
  eval_s(TreeS2, Vars1);
eval_s({stmt_if, TreeB, TreeBlockIf, TreeBlockElse}, Vars) ->
  B = eval_b(TreeB, Vars),
  case B of
    true ->
      eval_s(TreeBlockIf, Vars);
    false ->
      eval_s(TreeBlockElse, Vars)
  end;
eval_s({stmt_while, TreeB, TreeBlock}, Vars) ->
  B = eval_b(TreeB, Vars),
  case B of
    false ->
      Vars;
    true ->
      Vars2 = eval_s(TreeBlock, Vars),
      eval_s({stmt_while, TreeB, TreeBlock}, Vars2)
  end.


% arithmetic expression
eval_a({var, Name}, Vars) ->
  maps:get(Name, Vars);
eval_a({num, Number}, _Vars) ->
  Number;
eval_a({Op, TreeA1, TreeA2}, Vars) ->
  A1 = eval_a(TreeA1, Vars),
  A2 = eval_a(TreeA2, Vars),
  case Op of 
    op_add ->
      A1+A2;
    op_sub ->
      A1-A2;
    op_mult ->
      A1*A2;
    op_div ->
      A1 div A2
  end.


% Boolean expression
eval_b(bool_false, _Vars) ->
  false;
eval_b(bool_true, _Vars) ->
  true;
eval_b({Op, TreeB1, TreeB2}, Vars) when Op =:= op_and; Op =:= op_or ->
  B1 = eval_b(TreeB1, Vars),
  B2 = eval_b(TreeB2, Vars),
  case Op of
    op_and ->
      B1 and B2;
    op_or ->
      B1 or B2
  end;
eval_b({Op, TreeA1, TreeA2}, Vars) when Op =:= op_gt; Op =:= op_lt ->
  A1 = eval_a(TreeA1, Vars),
  A2 = eval_a(TreeA2, Vars),
  case Op of
    op_gt ->
      A1 > A2;
    op_lt ->
      A1 < A2
  end.


% list variables
list_vars(Vars) ->
  Names = lists:sort(maps:keys(Vars)),
  lists:foreach(fun(Name) ->
    Value = maps:get(Name, Vars),
    io:format("~s ~p~n", [Name, Value])
  end, Names).


% Pass 2: syntactic analysis
% return a syntax tree

% List of nodes:
%   {semicolon, TreeS1, TreeS2}
%   {op_assign, Name, TreeA}
%   {stmt_if, TreeB, TreeBlockIf, TreeBlockElse]
%   {stmt_while, TreeB, TreeBlock}
%   bool_true
%   bool_false
%   {var, Name}
%   {num, Number}
%   {op_add, TreeA1, TreeA2}
%   {op_sub, TreeA1, TreeA2}
%   {op_mult, TreeA1, TreeA2}
%   {op_div, TreeA1, TreeA2}
%   {op_and, TreeB1, TreeB2}
%   {op_or, TreeB1, TreeB2}
%   {op_gt, TreeA1, TreeA2}
%   {op_lt, TreeA1, TreeA2}

% Program -> S
parse(Tokens) ->
  case s(Tokens) of
    error ->
      error;
    {[], Tree} ->
      Tree;
    {TokensS, _Tree} ->
      error_unexpected_token(TokensS)
  end.


% we implement the production
%   S -> x.. | S ; S | if.. | while..
% by
%   S' -> x.. | if.. | while..
%   S -> S' (; S')^*

% S production rule
s(Tokens) ->
  case s_prime(Tokens) of
    error -> 
      error;
    {Tokens2, Tree} ->
      s_repeat(Tokens2, Tree)
  end.

s_repeat([{semicolon, _Line, _Col}|Tokens], Tree) ->
  case s_prime(Tokens) of
    error ->
      error;
    {Tokens2, Tree2} ->
      Tree3 = tree(semicolon, Tree, Tree2),
      s_repeat(Tokens2, Tree3)
  end;
s_repeat(Tokens, Tree) ->
  {Tokens, Tree}.


% S' production rule
s_prime([{var, Name, _Line, _Col}|Tokens]) ->
  % x := a
  case Tokens of
    [{op_assign, _Line2, _Col2}|Tokens2] ->
      case a(Tokens2) of
        error ->
          error;
        {TokensA, TreeA} ->
          Node = {op_assign, Name, TreeA},
          Tree = tree(Node),
          {TokensA, Tree}
      end;
    _Else ->
      error_expected_token(op_assign, Tokens)
  end;
s_prime([{stmt_if, _Line, _Col}|Tokens]) ->
  % if b then { S1 } else { S2 }
  case b(Tokens) of
    error ->
      error;
    {TokensB, TreeB} ->
      s_prime_if(TokensB, TreeB)
  end;
s_prime([{stmt_while, _Line, _Col}|Tokens]) ->
  % while b do { S }
  case b(Tokens) of
    error ->
      error;
    {TokensB, TreeB} ->
      s_prime_while(TokensB, TreeB)
  end;
s_prime(Tokens) ->
  error_unexpected_token(Tokens).


% "if (b)" already recognized
s_prime_if([{stmt_then, _Line, _Col}| Tokens], TreeB) ->
  case block_s(Tokens) of
    error ->
      error;
    {TokensBlockIf, TreeBlockIf} ->
      s_prime_if_then(TokensBlockIf, TreeB, TreeBlockIf)
  end;
s_prime_if(Tokens, _TreeB) ->
  error_expected_token(stmt_then, Tokens).


% "if (b) then { S1 }" already recognized
s_prime_if_then([{stmt_else, _Line, _Col}| Tokens], TreeB, TreeBlockIf) ->
  case block_s(Tokens) of
    error ->
      error;
    {TokensBlockElse, TreeBlockElse} ->
      Node = {stmt_if, TreeB, TreeBlockIf, TreeBlockElse},
      Tree = tree(Node),
      {TokensBlockElse, Tree}
  end;
s_prime_if_then(Tokens, _TreeB, _TreeBlockIf) ->
  error_expected_token(stmt_else, Tokens).


% "while b" already recognized
s_prime_while([{stmt_do, _Line, _Col}|Tokens], TreeB) ->
  case block_s(Tokens) of
    error ->
      error;
    {TokensBlock, TreeBlock} ->
      Node = {stmt_while, TreeB, TreeBlock},
      Tree = tree(Node),
      {TokensBlock, Tree}
  end;
s_prime_while(Tokens, _TreeB) ->
  error_expected_token(stmt_do, Tokens).


% { S }
block_s([{curly_open, _Line, _Col}|Tokens]) ->
  case s(Tokens) of 
    error ->
      error;
    {TokensS, TreeS} ->
      block_s_open(TokensS, TreeS)
  end;
block_s(Tokens) ->
  error_expected_token(curly_open, Tokens).


% "{ S" already recognized
block_s_open([{curly_close, _Line, _Col}|Tokens], TreeS) ->
  {Tokens, TreeS};
block_s_open(Tokens, _TreeS) ->
  error_expected_token(curly_close, Tokens).


% we implement the production
%   b -> true | false | b op_b b | a op_r a | (b)
% by
%   b -> b_term (or b_term)^*
%   b_term -> b_factor (and b_factor)^*
%   b_factor -> true | false | (b) | a op_r a 
% to get the operator precedence working

b(Tokens) ->
  case b_term(Tokens) of
    error ->
      error;
    {Tokens2, Tree} ->
      b_repeat(Tokens2, Tree)
  end.


b_repeat([{op_b, Op, _Line, _Col}|Tokens], Tree) when Op =:= op_or ->
  case b_term(Tokens) of
    error ->
      error;
    {Tokens2, Tree2} ->
      Tree3 = tree(Op, Tree, Tree2),
      b_repeat(Tokens2, Tree3)
  end;
b_repeat(Tokens, Tree) ->
  {Tokens, Tree}.


b_term(Tokens) ->
  case b_factor(Tokens) of
    error ->
      error;
    {Tokens2, Tree} ->
      b_term_repeat(Tokens2, Tree)
  end.


b_term_repeat([{op_b, Op, _Line, _Col}|Tokens], Tree) when Op =:= op_and ->
  case b_factor(Tokens) of
    error ->
      error;
    {Tokens2, Tree2} ->
      Tree3 = tree(Op, Tree, Tree2),
      b_term_repeat(Tokens2, Tree3)
  end;
b_term_repeat(Tokens, Tree) ->
  {Tokens, Tree}.


b_factor([{bool_true, _Line, _Col}|Tokens]) ->
  % b_factor -> true
  Node = bool_true,
  Tree = tree(Node),
  {Tokens, Tree};
b_factor([{bool_false, _Line, _Col}|Tokens]) ->
  % b_factor -> false
  Node = bool_false,
  Tree = tree(Node),
  {Tokens, Tree};
b_factor([{paren_open, _Line, _Col}|Tokens]) ->
  % b_factor -> ( b )
  case b(Tokens) of
    error ->
      error;
    {TokensB, TreeB} ->
      b_open(TokensB, TreeB)
  end;
b_factor(Tokens) ->
  % b_factor -> a op_r a
  case a(Tokens) of
    error ->
      error;
    {[{op_r, Op, _Line, _Col}|Tokens2], TreeA1} ->
      case a(Tokens2) of
        error ->
          error;
        {TokensA2, TreeA2} ->
          TreeA = tree(Op, TreeA1, TreeA2),
          {TokensA2, TreeA}
      end;
    {TokensA, TreeA} ->
      {TokensA, TreeA}
  end.


% "( b" already recognized
b_open([{paren_close, _Line, _Col}|Tokens], TreeB) ->
  {Tokens, TreeB};
b_open(Tokens, _TreeB) ->
  error_expected_token(paren_close, Tokens).


% we implement the production
%   a -> x | n | a op_a a | (a)
% by
%   a -> a_term (+ a_term | - a_term)^*
%   a_term -> a_factor (* a_factor | / a_factor)^*
%   a_factor -> x | n | ( a )
% to get the operator precedence working

a(Tokens) ->
  case a_term(Tokens) of
    error ->
      error;
    {Tokens2, Tree} ->
      a_repeat(Tokens2, Tree)
  end.


a_repeat([{op_a, Op, _Line, _Col}|Tokens], Tree) when Op =:= op_add; Op =:= op_sub ->
  case a_term(Tokens) of
    error ->
      error;
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


a_term_repeat([{op_a, Op, _Line, _Col}|Tokens], Tree) when Op =:= op_mult; Op =:= op_div ->
  case a_factor(Tokens) of
    error ->
      error;
    {Tokens2, Tree2} ->
      Tree3 = tree(Op, Tree, Tree2),
      a_term_repeat(Tokens2, Tree3)
  end;
a_term_repeat(Tokens, Tree) ->
  {Tokens, Tree}.


a_factor([{var, Name, _Line, _Col}|Tokens]) ->
  % a_factor -> x
  Node = {var, Name},
  Tree = tree(Node),
  {Tokens, Tree};
a_factor([{num, Number, _Line, _Col}|Tokens]) ->
  % a_factor -> n
  Node = {num, Number},
  Tree = tree(Node),
  {Tokens, Tree};
a_factor([{paren_open, _Line, _Col}|Tokens]) ->
  % a_factor -> ( a )
  case a(Tokens) of
    error ->
      error;
    {TokensA, TreeA} ->
      a_open(TokensA, TreeA)
  end;
a_factor(Tokens) ->
  error_unexpected_token(Tokens).


% "( a" already recognized
a_open([{paren_close, _Line, _Col}|Tokens], TreeA) ->
  {Tokens, TreeA};
a_open(Tokens, _TreeA) ->
  error_expected_token(paren_close, Tokens).


% Tree helpers

tree(Node) ->
  Node.


tree(Node, Tree1, Tree2) ->
  {Node, Tree1, Tree2}.


% error helpers

error_unexpected_token([Token|_T]) ->
  {TokenType, Line, Col} = reduce_token(Token),
  io:format("Unexpected token ~p at line ~p, column ~p~n", [TokenType, Line, Col]),
  error.


error_expected_token(Expected, [Token|_T]) ->
  {TokenType, Line, Col} = reduce_token(Token),
  io:format("Expected token ~p but found ~p at line ~p, column ~p~n", [Expected, TokenType, Line, Col]),
  error.


reduce_token({TokenType, _Arg, Line, Col}) ->
  {TokenType, Line, Col};
reduce_token({TokenType, Line, Col}) ->
  {TokenType, Line, Col}.


% Pass 1: lexical analysis
% recognize program text as sequence of tokens or return 'error'

% List of tokens:
%   {op_a, Op, Line, Col}  Op = op_add | op_sub | op_mult | op_div
%   {op_b, Op, Line, Col}  Op = op_and | op_or
%   {op_r, Op, Line, Col}  Op = op_gt | op_lt
%   {var, Name, Line, Col}
%   {num, Number, Line, Col}
%   {paren_open, Line, Col}
%   {paren_close, Line, Col}
%   {semicolon, Line, Col}
%   {curly_open, Line, Col}
%   {curly_close, Line, Col}
%   {op_assign, Line, Col}
%   {bool_true, Line, Col}
%   {bool_false, Line, Col}
%   {stmt_if, Line, Col}
%   {stmt_then, Line, Col}
%   {stmt_else, Line, Col}
%   {stmt_while, Line, Col}
%   {stmt_do, Line, Col}

tokenize(Prog) ->
  N = maps:size(Prog),
  Line = 1,
  Reserved = reserved_names(),
  Tokens = [],
  tokenize(Line, N, Prog, Reserved, Tokens).

% iterate over all lines
tokenize(Line, N, _Prog, _Reserved, Tokens) when Line > N ->
  lists:reverse(Tokens);
tokenize(Line, N, Prog, Reserved, Tokens) ->
  L = maps:get(Line, Prog),
  Col = 0,
  tokenize(L, Line, Col, N, Prog, Reserved, Tokens).

% iterate over characters of a single line
tokenize([], Line, _Col, N, Prog, Reserved, Tokens) ->
  tokenize(Line+1, N, Prog, Reserved, Tokens);
tokenize([H|T], Line, Col, N, Prog, Reserved, Tokens) when H >= $a, H =< $z ->
  tokenize_name(T, Col, [H], Line, Col+1, N, Prog, Reserved, Tokens);
tokenize([H|T], Line, Col, N, Prog, Reserved, Tokens) when H >= $0, H =< $9 ->
  tokenize_number(T, Col, [H], Line, Col+1, N, Prog, Reserved, Tokens);
tokenize([H|T], Line, Col, N, Prog, Reserved, Tokens) when H =:= $+; H =:= $-; H =:= $*; H =:= $/ ->
  case H of
    $+ ->
      Op = op_add;
    $- ->
      Op = op_sub;
    $* ->
      Op = op_mult;
    $/ ->
      Op = op_div
  end,
  Token = {op_a, Op, Line, Col},
  tokenize(T, Line, Col+1, N, Prog, Reserved, [Token|Tokens]);
tokenize([H|T], Line, Col, N, Prog, Reserved, Tokens) when H =:= $<; H =:= $> ->
  case H of
    $> ->
      Op = op_gt;
    $< ->
      Op = op_lt
  end,
  Token = {op_r, Op, Line, Col},
  tokenize(T, Line, Col+1, N, Prog, Reserved, [Token|Tokens]);
tokenize([H|T], Line, Col, N, Prog, Reserved, Tokens) when H =:= $( ->
  Token = {paren_open, Line, Col},
  tokenize(T, Line, Col+1, N, Prog, Reserved, [Token|Tokens]);
tokenize([H|T], Line, Col, N, Prog, Reserved, Tokens) when H =:= $) ->
  Token = {paren_close, Line, Col},
  tokenize(T, Line, Col+1, N, Prog, Reserved, [Token|Tokens]);
tokenize([H|T], Line, Col, N, Prog, Reserved, Tokens) when H =:= $: ->
  tokenize_assign(T, Line, Col+1, N, Prog, Reserved, Tokens);
tokenize([H|T], Line, Col, N, Prog, Reserved, Tokens) when H =:= $; ->
  Token = {semicolon, Line, Col},
  tokenize(T, Line, Col+1, N, Prog, Reserved, [Token|Tokens]);
tokenize([H|T], Line, Col, N, Prog, Reserved, Tokens) when H =:= ${ ->
  Token = {curly_open, Line, Col},
  tokenize(T, Line, Col+1, N, Prog, Reserved, [Token|Tokens]);
tokenize([H|T], Line, Col, N, Prog, Reserved, Tokens) when H =:= $} ->
  Token = {curly_close, Line, Col},
  tokenize(T, Line, Col+1, N, Prog, Reserved, [Token|Tokens]);
tokenize([H|T], Line, Col, N, Prog, Reserved, Tokens) when H =:= $ ; H =:= $\n; H =:= $\r; H =:= $\t ->
  % ignore white space now
  tokenize(T, Line, Col+1, N, Prog, Reserved, Tokens);
tokenize([H|_L], Line, Col, _N, _Prog, _Reserved, _Tokens) ->
  io:format("Unexpected character ~c at line ~p, column ~p~n", [H, Line, Col]),
  error.

% name might be a var or a reserved name, including boolean operators
tokenize_name([], ColStart, Chars, Line, _Col, N, Prog, Reserved, Tokens) ->
  Token = chars_to_token(Chars, Reserved, Line, ColStart),
  tokenize(Line+1, N, Prog, Reserved, [Token|Tokens]);
tokenize_name([H|T], ColStart, Chars, Line, Col, N, Prog, Reserved, Tokens) when H >= $a, H =< $z ->
  tokenize_name(T, ColStart, [H|Chars], Line, Col+1, N, Prog, Reserved, Tokens);
tokenize_name(L, ColStart, Chars, Line, Col, N, Prog, Reserved, Tokens) ->
  Token = chars_to_token(Chars, Reserved, Line, ColStart),
  tokenize(L, Line, Col, N, Prog, Reserved, [Token|Tokens]).


% certain names can not be used for variables
% they are turned into their own tokens

reserved_names() ->
  % we can not use the plain "and", "if", "or" atoms because they are reserved words
  % in Erlang, so we use our own atoms for all
  % see http://erlang.org/doc/reference_manual/introduction.html, 1.5 Reserved Words
  Reserved0 = maps:new(),
  Reserved1 = maps:put("and", {op_b, op_and}, Reserved0),
  Reserved2 = maps:put("or", {op_b, op_or}, Reserved1),
  Reserved3 = maps:put("true", bool_true, Reserved2),
  Reserved4 = maps:put("false", bool_false, Reserved3),
  Reserved5 = maps:put("if", stmt_if, Reserved4),
  Reserved6 = maps:put("then", stmt_then, Reserved5),
  Reserved7 = maps:put("else", stmt_else, Reserved6),
  Reserved8 = maps:put("while", stmt_while, Reserved7),
  maps:put("do", stmt_do, Reserved8).


chars_to_token(Chars, Reserved, Line, Col) ->
  Name = lists:reverse(Chars),
  case maps:is_key(Name, Reserved) of
    true ->
      case maps:get(Name, Reserved) of
        {op_b, Operation} ->
          {op_b, Operation, Line, Col};
        TokenType ->
          {TokenType, Line, Col}
      end;
    false ->
      {var, Name, Line, Col}
  end.


% number
tokenize_number([], ColStart, Digits, Line, _Col, N, Prog, Reserved, Tokens) ->
  Token = digits_to_token(Digits, Line, ColStart),
  tokenize(Line+1, N, Prog, Reserved, [Token|Tokens]);
tokenize_number([H|T], ColStart, Digits, Line, Col, N, Prog, Reserved, Tokens) when H >= $0, H =< $9 ->
  tokenize_number(T, ColStart, [H|Digits], Line, Col+1, N, Prog, Reserved, Tokens);
tokenize_number(L, ColStart, Digits, Line, Col, N, Prog, Reserved, Tokens) ->
  Token = digits_to_token(Digits, Line, ColStart),
  tokenize(L, Line, Col, N, Prog, Reserved, [Token|Tokens]).


digits_to_token(Digits, Line, Col) ->
  Number = list_to_integer(lists:reverse(Digits)),
  {num, Number, Line, Col}.


% assignment operator :=
tokenize_assign([], Line, _Col, _N, _Prog, _Reserved, _Tokens) ->
  io:format("Unexpected end of line at line ~p~n", [Line]),
  error;
tokenize_assign([H|T], Line, Col, N, Prog, Reserved, Tokens) when H =:= $= ->
  Token = {op_assign, Line, Col-1},
  tokenize(T, Line, Col+1, N, Prog, Reserved, [Token|Tokens]);
tokenize_assign(_L, Line, Col, _N, _Prog, _Reserved, _Tokens) ->
  io:format("= expected at line ~p, column ~p~n", [Line, Col]),
  error.


% read program lines from standard input
read_prog() ->
  N = 0,
  Prog = maps:new(),
  read_prog(N, Prog).

read_prog(N, Prog) ->
  case io:get_line("") of
    eof ->       
      Prog;
    L ->
      N2 = N+1,
      Prog2 = maps:put(N2, L, Prog),
      read_prog(N2, Prog2)
  end.
