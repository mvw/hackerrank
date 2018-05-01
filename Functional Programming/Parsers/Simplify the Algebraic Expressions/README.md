# Simplify the Algebraic Expressions

## Problem
https://www.hackerrank.com/challenges/simplify-the-algebraic-expressions/problem

Given more or less complicated algebraic expressions
```julia
10x + 2x - (3x + 6)/3  
18*(2x+2) - 5  
((9x + 81)/3 + 27)/3  - 2x  
18x + (12x + 10)*(2x+4)/2 - 5x  
(2x+5) * (x*(9x + 81)/3 + 27)/(1+1+1)  - 2x  
(2x+5) * (x*(9x^3 + 81)/3 + 27)/(1+1+1)  - 2x  
```
one has to come up with simplified expressions
``` julia
11x - 2
36x + 31
-x + 18
12x^2 + 47x + 20
2x^3 + 23x^2 + 61x + 45
2x^5 + 5x^4 + 18x^2 + 61x + 45 
```

## Solution
I enjoyed my handwritten parser for 
[While Language](https://github.com/mvw/hackerrank/tree/master/Functional%20Programming/Interpreter%20and%20Compilers/While%20Language)
so I recycled it for this parser task.

### Pass 1: Lexical Analysis
Example: The line
```julia
10x + 2x - (3x + 6)/3
```
is turned into
```erlang
Tokens=[{num,10,0},
        {x,2},
        {op_a,op_add,4},
        {num,2,6},
        {x,7},
        {op_a,op_sub,9},
        {paren_open,11},
        {num,3,12},
        {x,13},
        {op_a,op_add,15},
        {num,6,17},
        {paren_close,18},
        {op_a,op_div,19},
        {num,3,20}]
```
where we have only the column number as extra argument, still only used for nicer error messages.

The list of tokens became shorter:

```erlang
%   {op_a, Op, Col}  Op = op_add | op_sub | op_mult | op_div | op_exp
%   {x, Col}
%   {num, Number, Col}
%   {paren_open, Col}
%   {paren_close, Col}
```

### Pass 2: Syntactic Analysis

The parser is written to suit this grammar:

```erlang
% we implement the production
%   a -> x | n | a op_a a | a a | (a)
% by
%   a -> -^{0,1} a_term (+ a_term | - a_term)^*
%   a_term -> a_factor (* a_factor | / a_factor | a_factor)^*
%   a_factor -> a_base (^ a_base)^*
%   a_base -> x | n | ( a )
% to get the operator precedence working
```

This is an extended version of the grammar for the arithmetic expressions from the "While Language" problem.

Exponentiation is an operation with higher precedence than multiplication, so we had to add
another production rule, the new `a_base`.

Tricky was the need to recognize
- `a a` in the sense of `a * a` (implicit multiplication)
- `-a - a` in the sense of `(-a) - a` and not `(-a)*(-a)`, so the unary minus at the start of an expression 
  has lower precedence than the minus for the substraction

The first need means that we might consider the recognition as not successful but not failed yet,
communicating this by an empty tree structure to the callers:

```erlang
a_base(Tokens) ->
  {Tokens, tree_empty}.
```

The callers must then decide if this is acceptable or not.

The second need led to a new clause for the recognition of `a` terms:

```erlang
a([{op_a, op_sub, _Col}|Tokens]) ->
  case a_term(Tokens) of
    error ->
      error;
    {Tokens2, TreeA} ->
      Tree = tree(op_minus, TreeA),
      a_repeat(Tokens2, Tree)
  end;
```

It will add the unary minus node `{op_minus, Tree}` to the syntax tree.

In total we have these nodes: 

```erlang
% List of nodes:
%   Map : k -> a_k
%   {op_minus, Tree}
%   {op_add, TreeA1, TreeA2}
%   {op_sub, TreeA1, TreeA2}
%   {op_mult, TreeA1, TreeA2}
%   {op_div, TreeA1, TreeA2}
%   {op_exp, TreeA1, TreeA2}
```

The example expression 

```julia
10x + 2x - (3x + 6)/3
```

results in
```erlang
Tree={op_sub,{op_add,{op_mult,#{0 => 10,1 => 0,2 => 0,3 => 0,4 => 0,5 => 0},
                              #{0 => 0,1 => 1,2 => 0,3 => 0,4 => 0,5 => 0}},
                     {op_mult,#{0 => 2,1 => 0,2 => 0,3 => 0,4 => 0,5 => 0},
                              #{0 => 0,1 => 1,2 => 0,3 => 0,4 => 0,5 => 0}}},
             {op_div,{op_add,{op_mult,#{0 => 3,1 => 0,2 => 0,3 => 0,4 => 0,
                                        5 => 0},
                                      #{0 => 0,1 => 1,2 => 0,3 => 0,4 => 0,
                                        5 => 0}},
                             #{0 => 6,1 => 0,2 => 0,3 => 0,4 => 0,5 => 0}},
                     #{0 => 3,1 => 0,2 => 0,3 => 0,4 => 0,5 => 0}}}
```

where the map data structure

```erlang
#{0 => 10,1 => 0,2 => 0,3 => 0,4 => 0,5 => 0}
```

holds the coefficients of the polynomial

```julia
p(x) = a_5 x^5 + a_4 x^4 + a_3 x^3 + a_2 x^2 + a_1 x + a_0
```

here
```julia
p(x) = 10
```


### Pass 3: Evaluation

## After the task
The first other accepted Erlang solution is again by [tamarit27](https://www.hackerrank.com/tamarit27).
This solution seems handwritten as well. It is more compact than my solution.

Sad to see that the other four Erlang entries were copies of tamarit27's solution. :disappointed:

One contestant stripped the comments. An attempt to fool plagiarism detection?


## Keywords
algebra, compiler, interpreter, parser, polynomial, recursive descent parser
