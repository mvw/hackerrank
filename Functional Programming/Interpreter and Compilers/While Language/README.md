# While Language

## Problem
https://www.hackerrank.com/challenges/while-language-fp/problem

## Solution
I frist wrote the tokenizer, then a recursive descent parser, finally the evaluation
routine.

### Pass 1: Lexical Analysis
Have a look at the code. 

It turned out useful for the next part to have a list of all introduced tokens.

Testcase 1

```go
a := 10 ;
b := 100 ;

if ( a < b ) then
    {
        min := a ;
        max := b
    }
else {
    min := b ;
    max := a
    }
```

then turns into

```erlang
Tokens=[{var,"a",1,0},
        {op_assign,1,2},
        {num,10,1,5},
        {semicolon,1,8},
        {var,"b",2,0},
        {op_assign,2,2},
        {num,100,2,5},
        {semicolon,2,9},
        {stmt_if,4,0},
        {paren_open,4,3},
        {var,"a",4,5},
        {op_r,op_lt,4,7},
        {var,"b",4,9},
        {paren_close,4,11},
        {stmt_then,4,13},
        {curly_open,5,4},
        {var,"min",6,8},
        {op_assign,6,12},
        {var,"a",6,15},
        {semicolon,6,17},
        {var,"max",7,8},
        {op_assign,7,12},
        {var,"b",7,15},
        {curly_close,8,4},
        {stmt_else,9,0},
        {curly_open,9,5},
        {var,"min",10,4},
        {op_assign,10,8},
        {var,"b",10,11},
        {semicolon,10,13},
        {var,"max",11,4},
        {op_assign,11,8},
        {var,"a",11,11},
        {curly_close,12,4}]
```

The last two numbers are line (starting at line 1) and column (starting at column 0),
which are useful in error messages.

I would have prefered to use `if` instead of `stmt_it`, but it turned out that
there are reserved words in Erlang, which can not be used for atoms, only
indirectly by quoting them, e.g. `'if'`, which I liked less.

### Pass 2: Syntactic Analysis
To tackle this part I had a look at my Kindle copy of 

* Niklaus Wirth: *Grundlagen und Techniken des Compilerbaus*, 3rd edition, 2011

which for some reason is not offered anymore by Amazon. Strange. 

The first few chapters have nice examples of grammars and chapter 4.1 describes
how to write a recursive descent parser. 

I probably saw this technique first in Bjarne Stroustrup's first book on C++,
where he featured such a parser for a calculator.
The striking feature was how the structure of the parser mirrored the structure
of the grammar.

This I wanted to try out for this task in Erlang as well.

One can mostly translate the given grammar into a recursive descent parser.

The two pitfalls are:
* recursive production rules, e.g. A -> A a | b, which would introduce ambiguity (non-determinism)
  into the recognition process (there would be words accepted by different rules with a
  common first symbol)
* implementing operator precedence

Solutions:
* for the first case Wirth shows how to express left recursion by repetition, e.g. using
  A -> b A^* instead
* for the second case one splits the grammar rule into several rules, from lower to higher 
  precedence

We encounter such a production here:

```erlang
% we implement the production
%   S -> x.. | S ; S | if.. | while..
% by
%   S' -> x.. | if.. | while..
%   S -> S' (; S')^*
```

Getting the repetition right took me some time, maybe because I had to express it
using recursion again. :-) Here is how it turned out:

```erlang
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
```

The correct split into `s/1` and `s_repeat/2` I got from [this code](http://www.cs.dartmouth.edu/~mckeeman/cs118/languages/erlang/exprParser.html).

For the operator precedence I used the usual split into `expression`, `term` and `factor` 
production rules, e.g. here:

```erlang
%   a -> a_term (+ a_term | - a_term)^*
%   a_term -> a_factor (* a_factor | / a_factor)^*
%   a_factor -> x | n | ( a )
```

Testcase 0 

```go
fact := 1 ;
val := 10000 ;
cur := val ;
mod := 1000000007 ;

while ( cur > 1 )
  do
   {
      fact := fact * cur ;
      fact := fact - fact / mod * mod ;
      cur := cur - 1
   } ;

cur := 0
```

then gives the syntax tree

```erlang
Tree={semicolon,
         {semicolon,
             {semicolon,
                 {semicolon,
                     {semicolon,
                         {op_assign,"fact",{num,1}},
                         {op_assign,"val",{num,10000}}},
                     {op_assign,"cur",{var,"val"}}},
                 {op_assign,"mod",{num,1000000007}}},
             {stmt_while,
                 {op_gt,{var,"cur"},{num,1}},
                 {semicolon,
                     {semicolon,
                         {op_assign,"fact",{op_mult,{var,"fact"},{var,"cur"}}},
                         {op_assign,"fact",
                             {op_sub,
                                 {var,"fact"},
                                 {op_mult,
                                     {op_div,{var,"fact"},{var,"mod"}},
                                     {var,"mod"}}}}},
                     {op_assign,"cur",{op_sub,{var,"cur"},{num,1}}}}}},
         {op_assign,"cur",{num,0}}}
```

### Pass 3: Evaluation
Have a look at the code. It is straight forward.

## After the task
The only other accepted Erlang solution is by [tamarit27](https://www.hackerrank.com/tamarit27).

He seems to have used the [yecc](http://erlang.org/doc/man/yecc.html) parser generator.

I have played with lex and yacc and ANTLR, but did not get really warm with those.
Perhaps if the grammar has to be developed too, it makes sense to invest the time to
get fluent with them.

## Keywords
compiler, interpreter, recursive descent parser
