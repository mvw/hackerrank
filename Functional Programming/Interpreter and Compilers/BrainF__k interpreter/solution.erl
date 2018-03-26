-module(solution).
-export([main/0]).

% BrainFuck interpreter (Hackerrank flavour)
main() ->
  {ok, [N, M]} = io:fread("", "~d~d"),
  Input = read_chars(N),
  _Dollar = read_chars(1),
  Prog = read_prog(M),
  run(Prog, Input).


% set true to enable debug information
debug_info() ->
  false.


run(Prog, Input) ->
  Code = maps:get(code, Prog),
  IPMax = maps:size(Code),
  IP = 1,
  DP = 0,
  Mem = maps:put(0, 0, maps:new()),
  Count = 0,
  run(Prog, IPMax, Input, IP, DP, Mem, Count).


run(Prog, IPMax, Input, IP, _DP, Mem, Count) when IP > IPMax ->
  % regular end of execution
  dump(Prog, IPMax, Input, IP, _DP, Mem, Count),
  ok;
run(Prog, IPMax, Input, IP, DP, Mem, Count) when Count >= 100000 ->
  % stop condition given by task
  io:format("~nPROCESS TIME OUT. KILLED!!!~n"),
  dump(Prog, IPMax, Input, IP, DP, Mem, Count),
  killed;
run(Prog, IPMax, Input, IP, DP, Mem, Count) ->
  % decode instruction and execute
  Code = maps:get(code, Prog),
  {_Line, [C]} = maps:get(IP, Code),
  case C of
    $> ->
      DP2 = DP+1,
      change_dp(Prog, IPMax, Input, IP, DP2, Mem, Count);
    $< ->
      DP2 = DP-1,
      change_dp(Prog, IPMax, Input, IP, DP2, Mem, Count);
    $+ ->
      Byte = inc(maps:get(DP, Mem)),
      change_byte(Prog, IPMax, Input, IP, DP, Byte, Mem, Count);
    $- ->
      Byte = dec(maps:get(DP, Mem)),
      change_byte(Prog, IPMax, Input, IP, DP, Byte, Mem, Count);
    $. ->
      io:format("~c", [maps:get(DP, Mem)]),
      run(Prog, IPMax, Input, IP+1, DP, Mem, Count+1);
    $, ->
      [[Byte]|Input2] = Input,
      Mem2 = maps:update(DP, Byte, Mem),
      run(Prog, IPMax, Input2, IP+1, DP, Mem2, Count+1);
    $[ ->
      case maps:get(DP, Mem) =:= 0 of
        true ->
          forward(Prog, IPMax, Input, IP, DP, Mem, Count);
        false ->
          run(Prog, IPMax, Input, IP+1, DP, Mem, Count+1)
      end;
    $] ->
      case maps:get(DP, Mem) =/= 0 of
        true ->
          back(Prog, IPMax, Input, IP, DP, Mem, Count);
        false ->
          run(Prog, IPMax, Input, IP+1, DP, Mem, Count+1)
      end;
    _Else ->
      io:format("~nUNKNOWN COMMAND.~n"),
      dump(Prog, IPMax, Input, IP, DP, Mem, Count),
      killed
  end.


% change data pointer
change_dp(Prog, IPMax, Input, IP, DP2, Mem, Count) when DP2 < 0 ->
  io:format("~nNEGATIVE MEMORY INDEX AT IP=~p.~n", [IP]),
  dump(Prog, IPMax, Input, IP, DP2, Mem, Count),
  killed;
change_dp(Prog, IPMax, Input, IP, DP2, Mem, Count) ->
  case maps:is_key(DP2, Mem) of
    true ->
      Mem2 = Mem;
    false ->
      Mem2 = maps:put(DP2, 0, Mem)
  end,
  run(Prog, IPMax, Input, IP+1, DP2, Mem2, Count+1).


% change byte
change_byte(Prog, IPMax, Input, IP, DP, Byte, Mem, Count) ->
  Mem2 = maps:update(DP, Byte, Mem),
  run(Prog, IPMax, Input, IP+1, DP, Mem2, Count+1).

inc(255) ->
  0;
inc(Byte) ->
  Byte+1.

dec(0) ->
  255;
dec(Byte) ->
  Byte-1.


% [ is like "while (*DP) { A .. } B" in a C like language
forward(Prog, IPMax, Input, IP, DP, Mem, Count) ->
  Forward = maps:get(forward, Prog),
  IP2 = maps:get(IP, Forward),
  run(Prog, IPMax, Input, IP2, DP, Mem, Count+1).

% ] provides the "}" part of the while loop
back(Prog, IPMax, Input, IP, DP, Mem, Count) ->
  Back = maps:get(back, Prog),
  IP2 = maps:get(IP, Back),
  run(Prog, IPMax, Input, IP2, DP, Mem, Count+1).


% dump some useful debugging information
dump(Prog, IPMax, Input, IP, DP, Mem, Count) ->
  case debug_info() of
    true ->
      case IP > IPMax of
        true ->
          Line = done,
          C = $-;
        false ->
          Code = maps:get(code, Prog),
          {Line, [C]} = maps:get(IP, Code)
      end,
      io:format("~nCount ~p: Line=~p IP=~p C=~c IPMax=~p DP=~p~n",
                [Count, Line, IP, C, IPMax, DP]),
      io:format("Input=~p~n", [Input]),
      dump_prog(Prog),
      dump_mem(Mem);
    false ->
      ok
  end.


dump_mem(Mem) ->
  io:format("Mem=~n"),
  L = maps:keys(Mem),
  S = lists:sort(L),
  lists:foreach(fun(DP) ->
    Byte = maps:get(DP, Mem),
    io:format("~3..0b:~3..0b ", [DP, Byte])
  end, S),
  io:format("~n").


dump_prog(Prog) ->
  io:format("Prog="),
  Code = maps:get(code, Prog),
  Forward = maps:get(forward, Prog),
  Back = maps:get(back, Prog),
  L = maps:keys(Code),
  S = lists:sort(L),
  lists:foldl(fun(IP, LineOld) ->
    {Line, C} = maps:get(IP, Code),
    case Line =/= LineOld of
      true ->
        io:format("~n~3..0b:~3..0b: ", [Line, IP]);
      false ->
        true
    end,
    io:format("~s", [C]),
    Line
  end, 0, S),
  io:format("~n"),
  io:format("Forward=~p~n", [Forward]),
  io:format("Back=~p~n", [Back]).


% as we have to read in all command characters anyway,
% we keep track of the square brackets
read_prog(M) ->
  IP0 = 1,
  Code0 = maps:new(),
  Stack0 = [],
  Forward0 = maps:new(),
  Back0 = maps:new(),
  {_IP, Code, _Stack, Forward, Back} =
  lists:foldl(fun(Line, {IP, Code, Stack, Forward, Back}) ->
    S = io:get_line(""),
    read_line(S, Line, IP, Code, Stack, Forward, Back)            
  end, {IP0, Code0, Stack0, Forward0, Back0}, lists:seq(1, M)),
  Prog0 = maps:new(),
  Prog1 = maps:put(forward, Forward, Prog0),
  Prog2 = maps:put(back, Back, Prog1),
  maps:put(code, Code, Prog2).


read_line(S, Line, IP0, Code0, Stack0, Forward0, Back0) ->
  lists:foldl(fun(C, {IP, Code, Stack, Forward, Back}) ->
    case is_bf_cmd(C) of
      false ->
        % skip
        {IP, Code, Stack, Forward, Back};
      true ->
        case C of
          $[ ->
            Stack2 = [IP|Stack],
            Forward2 = Forward,
            Back2 = Back;
          $] ->
            [IPOpen|Stack2] = Stack,
            Forward2 = maps:put(IPOpen, IP, Forward),
            Back2 = maps:put(IP, IPOpen, Back);
          _Else ->
            Stack2 = Stack,
            Forward2 = Forward,
            Back2 = Back
        end,
        % store chars in sublists to ease debugging
        Code2 = maps:put(IP, {Line, [C]}, Code),
        {IP+1, Code2, Stack2, Forward2, Back2}
    end
  end, {IP0, Code0, Stack0, Forward0, Back0}, S).


% recognize characters which are commands
is_bf_cmd($>) -> true;
is_bf_cmd($<) -> true;
is_bf_cmd($+) -> true;
is_bf_cmd($-) -> true;
is_bf_cmd($.) -> true;
is_bf_cmd($,) -> true;
is_bf_cmd($[) -> true;
is_bf_cmd($]) -> true;
is_bf_cmd(_C) -> false.


% chars are in sublists, e.g. [[65], [66], ..]
read_chars(N) ->
  Fmt = unicode:characters_to_list(lists:duplicate(N, "~c")),
  {ok, L} = io:fread("", Fmt),
  L.

