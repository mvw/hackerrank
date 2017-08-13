-module(solution).
-export([main/0]).

main() ->
  {ok, [N]} = io:fread("", "~d"),
  L = read_list(N),
  %io:format("~p~n", [L]),
  B = check_concave(N, L),
  write_bool(B),
  ok.


check_concave(3, _L) ->
  false;
check_concave(N, L) ->
  {CX, CY} = midpoint(N, L),
  %io:format("CX=~p CY=~p~n", [CX, CY]),
  S = sort(CX, CY, L),
  %write_points(CX, CY, S),
  [X1, Y1, X2, Y2 | _T] = S,
  check_concave(S ++ [X1, Y1, X2, Y2]).


% check interior angles
check_concave([_X1, _Y1, _X2, _Y2]) ->
  false;
check_concave([X1, Y1, X2, Y2, X3, Y3|T]) ->
  RelativeAngle = relative_angle(X1-X2, Y1-Y2, X3-X2, Y3-Y2),
  %io:format("X1=~p Y1=~p X2=~p Y2=~p X3=~p Y3=~p RelativeAngle=~p~n", [X1, Y1, X2, Y2, X3, Y3, RelativeAngle]),
  case RelativeAngle > math:pi() of
    false ->
      check_concave([X2, Y2, X3, Y3|T]);
    true ->
      true
  end.


% sort by angle regarding the x-axis (origin is moved to the center point)
sort(CX, CY, L) ->
  P = pairs(L),
  S = lists:sort(fun([XA, YA], [XB, YB]) ->           
    AngleA = angle(XA-CX, YA-CY),
    AngleB = angle(XB-CX, YB-CY),
    AngleA =< AngleB           
  end, P),
  lists:flatten(S).


% create a list of pairs [[X1,Y1], [X2,Y2], ..]
pairs(L) ->
  pairs(L, []).

pairs([], P) ->
  P;
pairs([X, Y|T], P) ->
  pairs(T, [[X,Y]|P]).


% find the center point
midpoint(N, L) ->
  midpoint(N, L, 0, 0).

midpoint(_N, [], CX, CY) ->
  {CX, CY};
midpoint(N, [X, Y|T], CX, CY) ->
  midpoint(N, T, CX+(X/N), CY+(Y/N)).


% relative angle
relative_angle(X1, Y1, X2, Y2) ->
  Angle1 = angle(X1, Y1),
  Angle2 = angle(X2, Y2),
  Diff = Angle1-Angle2,
  case Diff < 0.0 of
    false ->
      Diff;
    true ->
      Diff + 2.0 * math:pi()
  end.


% mathematical angle to x-axis
angle(X, Y) ->
  Atan2 = math:atan2(Y, X),
  case Atan2 >= 0.0 of
    true ->
      Atan2;
    false ->
      Atan2 + 2.0*math:pi()
  end.


% write points and their angle to the x-axis (origin moved to center)
write_points(_CX, _CY, []) ->
  ok;
write_points(CX, CY, [X, Y|T]) ->
  Angle = angle(X-CX, Y-CY),
  io:format("(~p, ~p): ~p~n", [X, Y, Angle]),
  write_points(CX, CY, T).


write_bool(B) ->
  case B of
    true ->
      io:format("YES~n");
    false ->
      io:format("NO~n")
  end.


read_list(N) ->
  Fmt = string:copies("~d~d", N),
  {ok, L} = io:fread("", Fmt),
  L.
