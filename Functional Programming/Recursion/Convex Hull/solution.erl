-module(solution).
-export([main/0]).

main() ->
  {ok, [N]} = io:fread("", "~d"),
  L = read_points(N),
  %io:format("L=~w~n", [L]),
  Hull = convex_hull(L),
  %io:format("Hull=~w~n", [Hull]),
  Perimeter = perimeter(Hull),
  io:format("~p~n", [Perimeter]).


% numerical precision
eps() ->
  1.0e-8.


% using Graham scan for determining the convex hull

convex_hull(L) ->
  {P0, P} = start_point(L),
  %io:format("P0=~p~n", [P0]),
  %io:format("P=~w~n", [P]),
  Sorted = lists:sort(fun(PA, PB) ->
    A = angle(P0, PA),
    B = angle(P0, PB),
    case abs(A - B) < eps() of
      false ->
         A =< B;
      true ->
        d2(PA) >= d2(PB)
    end
  end, P),
  %write_sorted(P0, Sorted),
  Sorted2 = remove2(P0, Sorted),
  %write_sorted(P0, Sorted2),
  [P1|S] = Sorted2,
  N = 2+length(S),
  Stack = [P1, P0],
  convex_hull(N, 2, S, Stack).

convex_hull(N, I, _S, Stack) when I >= N ->
  Stack;
convex_hull(N, I, [P3|TS], [P2, P1|TStack]) ->
  %io:format("N=~p I=~p S=~w Stack=~w~n", [N, I, [P3|TS], [P2, P1|TStack]]),
  {X1, Y1} = P1,
  {X2, Y2} = P2,
  {X3, Y3} = P3,
  Ccw = ccw(X1, Y1, X2, Y2, X3, Y3),
  %io:format("ccw(~p,~p,~p,~p,~p,~p)=~p~n", [X1, Y1, X2, Y2, X3, Y3, Ccw]),
  case (Ccw > 0) orelse (TStack =:= []) of
    true ->
      I2 = I+1,
      S2 = TS,
      Stack2 = [P3, P2, P1|TStack];
    false ->
      I2 = I,
      S2 = [P3|TS],
      Stack2 = [P1|TStack]
    end,
    %io:format("N=~p I2=~p S2=~w Stack2=~w~n", [N, I2, S2, Stack2]),
    convex_hull(N, I2, S2, Stack2).
  

angle(P0, P) ->
  {X0, Y0} = P0,
  {X, Y} = P,
  DX = X-X0,
  DY = Y-Y0,
  math:atan2(DY, DX).


% length squared of vector OP
d2(P) ->
  {X, Y} = P,
  X*X + Y*Y.


% ccw > 0: points form counter-clockwise turn
% ccw = 0: points are collinear
% ccw < 0: points form clockwise turn
ccw(X1, Y1, X2, Y2, X3, Y3) ->
  (X2-X1)*(Y3-Y1) - (Y2-Y1)*(X3-X1).


% select the start point P0 from L, 
% return list of other points as well
start_point(L) ->
  Max = 10000,
  start_point(L, Max, Max, []).

start_point([], MinX, MinY, P) ->
  P0 = {MinX, MinY},
  P2 = remove(P0, P),
  {P0, P2};
start_point([X, Y|T], MinX, MinY, P) ->
  case Y < MinY of
    true ->
      MinX2 = X,
      MinY2 = Y;
    false ->
      case Y =:= MinY of
        true ->
          MinX2 = min(MinX, X),
          MinY2 = MinY;
        false ->
          MinX2 = MinX,
          MinY2 = MinY
      end
  end,
  start_point(T, MinX2, MinY2, [{X,Y}|P]).


% remove the start point P0 from the list of points P
remove(P0, P) ->
  remove(P0, P, []).

remove(_P0, [], Result) ->
  Result;
remove(P0, [P0|T], Result) ->
  remove(P0, T, Result);
remove(P0, [P|T], Result) ->
  remove(P0, T, [P|Result]).


% we might have several consecutive points with same Cos value,
% of these keep only the first, drop the others
remove2(P0, P) ->
  remove2(P0, P, []).

remove2(_P0, [], Result) ->
  lists:reverse(Result);
remove2(P0, [P1], Result) ->
  remove2(P0, [], [P1|Result]);
remove2(P0, [P1, P2|T], Result) ->
  A1 = angle(P0, P1),
  A2 = angle(P0, P2),
  case abs(A1-A2) < eps() of 
    false ->
      remove2(P0, [P2|T], [P1|Result]);
    true ->
      remove2(P0, [P1|T], Result)
  end.


% length of the perimeter of the convex hull
perimeter(Hull) ->
  [H|_T] = Hull,
  perimeter(Hull ++ [H], 0.0).

perimeter([_P1], Sum) ->
  Sum;
perimeter([P2, P1|T], Sum) ->
  {X1, Y1} = P1,
  {X2, Y2} = P2,
  DX = X2-X1,
  DY = Y2-Y1,
  Len = math:sqrt(DX*DX + DY*DY),
  perimeter([P1|T], Sum + Len).


% write list and angles
write_sorted(P0, Sorted) ->
  io:format("Sorted=~w~n", [Sorted]),
  lists:foreach(fun(PI) ->
    Angle = angle(P0, PI), 
    io:format("P=~p, Angle=~p~n", [PI, Angle])
  end, Sorted).


% read a list of N coordinate pairs
read_points(N) ->
  Fmt = string:copies("~d~d", N),
  {ok, L} = io:fread("", Fmt),
  L.
