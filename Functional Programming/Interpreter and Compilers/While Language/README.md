# While Language

## Problem
https://www.hackerrank.com/challenges/while-language-fp/problem

## Solution

### Pass 1: Lexical Analysis

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
turns into
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

### Pass 2: Syntactic Analysis

Test case 0 
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

gives

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
