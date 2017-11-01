# Password Cracker FP

## Problem
http://www.hackerrank.com/challenges/password-cracker-fp/problem

Try to split a given sentence into words from a given set.

## Experience

I needed more attempts to get this one right and then correct than expected.

When I sketched the first solution attempt on paper, this looked like a non-deterministic finite 
automaton with some epsilon transitions:

```erlang
% We can model the verification system as epsilon-NFA:
% - The initial state 0 is connected to the first symbol of each password
%   via acceptance of that symbol
% - then we have transitions within the passwords from i-th to (i+1)-th symbol
%   via acceptance of the (i+1)-th symbol
% - from the last symbol there is an epsilon transition to the initial state
% - the last symbols of the passwords could be final states
% We could now read the login attempt string and determine symbol for symbol
% what states can be reached, and if we got a final state among the reached
% states once the LoginAttempt string has been read it can be recognized.
```

My impression was that this approach would allow to answer if a given loginAttempt string
could be split into the words or not, but that it would not give the sequence of individual
words for the successful match.

Further it looked suited for parallel recognition, so I then experimented with a set of
Erlang processes, where each process was responsible to recognize a specific word.

This was fun, but the part where I built the full string from individually recognized
words did not scale well, it took too much memory.

Then I got the idea that the recognition process could be performed by a depth-first search, 
so I returned to the sequential world. With a little bit of tweaking it turned into
a solution which tackled all the test cases. I was happy I solved the problem.

However the name "Password Cracker FP" made me wonder if the suffix "FP" meant that there 
was a non-FP version of this problem.

And indeed there is:

 [Algorithms > Recursion > Password Cracker](http://www.hackerrank.com/challenges/password-cracker/problem)

So I entered my solution from here there. Easy kill. However I was first surprised to note the 
extra test cases and then got very soon humbled by the fact that it did not make full score there. 

I had to put in some extra work to fix some undetected bugs. It also needed extra optimization to
run within the given time frame. See the comments in the source code.
