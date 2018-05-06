# Project Euler #199 Iterative Circle Packing

## Problem
http://www.hackerrank.com/contests/projecteuler/challenges/euler199

Calculate the area of an interesting recursive geometric construction.

![overview](https://github.com/mvw/hackerrank/blob/master/ProjectEuler/Project%20Euler%20%23199%20Iterative%20Circle%20Packing/xmas2017.png)

## Solution
To be published at a later time.

I am not sure yet, what time lag is appropriate for this active contest. 

## My first Erlang GUI application
Of course this problem caused an itch to draw it. :smiley_cat:

As usual with geometric problems I fiddle with the incredible [GeoGebra](https://www.geogebra.org)
software to get an idea what is going on. 

Alas programming something more complex in GeoGebra is not very comfortable
right now, one has to bring in JavaScript code by some means I can hardly
remember.

So I took a look at Erlang's GUI capabilities.
It turned out that it still features bindings for [wxWidgets](http://www.wxwidgets.org/).

Some resources:
* [wxErlang User's Guide](http://erlang.org/doc/apps/wx/users_guide.html)
* [wxWindow](http://erlang.org/doc/man/wxWindow.html) from the Erlang online docs at [erlang.org](http://www.erlang.org/)
* the above page links to [docs.wxwidgets.org](http://docs.wxwidgets.org/2.8.12/wx_wxwindow.html)
* [wx examples](https://github.com/erlang/otp/tree/master/lib/wx/examples)

I booted via [Michael Turner's tutorial](http://www.idiom.com/~turner/wxtut/wxwidgets.html)
and then looked at some of the examples.

And the result is [gui.erl](https://github.com/mvw/hackerrank/blob/master/ProjectEuler/Project%20Euler%20%23199%20Iterative%20Circle%20Packing/gui.erl)

It is not very comfortable yet, e.g. the two parameters `n` and `m`are hard-coded, but it
does its job. :smirk_cat:

The screen shots above are from this program.

I might extend it and clean it up at some later time. :joy_cat:


## Experience
Very nice task.
