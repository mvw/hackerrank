-module(solution).
-export([main/0]).

main() ->
  {ok, [T]} = io:fread("", "~d"),
  L = read_list(T),
  lists:foreach(fun(N) ->
    Max = search(N),
    io:format("~p~n", [Max])
  end, L),
  true.


% a = N-b-c
% b = (N-c+sqrt(-N^2+c^2+2Nc))/2
search(N) ->
  NN = N*N,
  N2 = 2*N,
  lists:foldl(fun(C, Max) ->
    R = C*(C+N2)-NN,
    case R >= 0 of
      false ->
        Max;
      true ->
        Sqrt = math:sqrt(R),
        S = trunc(Sqrt),
        case abs(Sqrt - S) < 1.0e-8 of
          false ->
            Max;
          true ->
            B2 = N-C+S,
            case B2 rem 2 =:= 0 of
              false ->
                Max;
              true ->
                B = B2 div 2,
                A = N-C-B,
                case (A >= 1) andalso (B >= A) andalso (C >= B) of
                  false ->
                    Max;
                  true ->
                    V = A*B*C,
                    %io:format("A=~p B=~p C=~p N=~p V=~p~n", [A, B, C, N, V]),
                    max(Max, V)
                end
            end
        end
    end
  end, -1, lists:seq(1, N)).


read_list(N) ->
  Fmt = unicode:characters_to_list(lists:duplicate(N, "~d")),
  {ok, L} = io:fread("", Fmt),
  L.
