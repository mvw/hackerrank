# BrainF__k interpreter

## Problem
https://www.hackerrank.com/challenges/brainf-k-interpreter-fp/problem

Write an interpreter for the BrainFuck language.

## Solution
The BrainFuck machine has a couple of state variables 
and program execution is more or less
about changing these according to the eight commands.

Here is an example:
```Erlang
run(Prog, IPMax, Input, IP, DP, Mem, Count) ->
  % decode instruction and execute
  Code = maps:get(code, Prog),
  {_Line, [C]} = maps:get(IP, Code),
  case C of
      % (..)
    $, ->
      [[Byte]|Input2] = Input,
      Mem2 = maps:update(DP, Byte, Mem),
      run(Prog, IPMax, Input2, IP+1, DP, Mem2, Count+1);
      % (..)
  end.
```
The `,` command will consume a byte of the input and change the memory location (which the data pointer DP points to)
to this value. 
We continue at the next address (which the instruction pointer IP points to), updating the count
of executed statements.

## Experience

### Round 1
The problem statement looked simple enough, so I started coding the interpreter.
When I had it ready, it tackled only 9 of the 18 test cases. 

### Round 2
I had no good idea what might be wrong. So I started downloading test cases to see how they fail.
It turned out that I had to add debuggining information to my solution candidate to help me 
understand what was going on.
I also checked my code several times. No luck.

So I turned to BrainFuck samples from the web.
This made me understand that the language specification had a few issues, where the specs were
not specific enough, leaving some freedom to implementers,
e.g. see [Portability issues](https://en.wikipedia.org/wiki/Brainfuck#Portability_issues).
These were my primary suspects for bugs. 

After some sleep, I continued with the task and the debug output started to become useful. 
Here is an example of the final version:

```asm
sp                                                 output
PROCESS TIME OUT. KILLED!!!

Count 100000: Line=10 IP=15 C=] IPMax=15 DP=0      state
Input=[]                                           remaining input
Prog=                                              program listing
001:001: ++                                        source line number, instruction address, instructions
002:003: [
003:004: >
004:005: ,
005:006: +++
006:009: .
007:010: <
008:011: -
009:012: ]
010:013: +[]
Forward=#{3 => 12,14 => 15}                        forward map
Back=#{12 => 3,15 => 14}                           back map
Mem=                                               memory dump
000:001 001:112                                    data address, byte value
```

It turned out that it probably were not the holes in the spec, but the more complicated `[` and `]` commands had
potential for bugs. So I added debug output to their execution.

Finally I got a test case with many square brackets.
When I ran it, I got only a couple of debug outputs, instead of the many expected ones.

This made me finally realize that when forwarding or backwarding to the other square bracket, it was
not correct to take the first one. I had to pick the partner bracket of the proper depth level.

I should have read the spec more carefully. The underestimated word was "matching".

## Round 3
Later that day I could add the missing code. I decided not to scan back and forth while keeping the depth levels
of open and closed square brackets. 

Instead I built the needed data structures during the first read of the source file.

```erlang
Forward=#{3 => 12,14 => 15}
Back=#{12 => 3,15 => 14}
```

The example above shows the `Forward` and `Back`maps, e.g. a `[` at offset 3 will match a `]` at offset 12.

This version of the solution tackled all test cases except one.

## Round 4
I had a look at the failed test case and it seemed that I stopped the program one step too late, in case it
executed too many instructions.

I corrected the guard clause for this case and the resulting solution was able to solve all test cases.

## After the task
As usual I had a look at the other Erlang solutions. I wanted to know how other folks approached this task.

Quite surprising: The solution by [tamarit27](https://www.hackerrank.com/tamarit27) was plagiarized FIVE times. :astonished:

Ouch!

## Keywords
BrainFuck, interpreter
