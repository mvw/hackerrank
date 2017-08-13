-module(solution).
-export([main/0]).

main() ->
  {ok, [X]} = io:fread("", "~d"),
  {ok, [N]} = io:fread("", "~d"),
  Basis = get_basis(X, N),
  %io:format("Basis=~w~n", [Basis]),
  D = length(Basis),
  %io:format("D=~p~n", [D]),
  S = get_max_size(X, Basis, D),
  %io:format("S=~p~n", [S]),
  %Sets = pow(2, D),
  %io:format("Sets=~p~n", [Sets]),
  %SetsToCheck = pow(2, S),
  %io:format("SetsToCheck=~p~n", [SetsToCheck]),
  Count = count(X, Basis, D, S),
  io:format("~p~n", [Count]),
  ok.


count(X, Basis, D, S) ->
  count(X, D, S, 0, [], Basis, 0).

count(_X, _D, S, I, _Choice, _Options, Count) when I =:= S ->
  Count;
count(X, D, S, I, Choice, Options, Count) ->
  %io:format("I=~p Choice=~w Options=~w~n", [I, Choice, Options]),
  lists:foldl(fun(OptionJ, CountJ) ->
    case (I =:= 0) orelse (lists:last(Choice) < OptionJ) of
      true ->
        Choice2 = Choice ++ [OptionJ],
        Sum = lists:sum(Choice2),
        case Sum =:= X of
          true ->
            Found = 1;
          false ->
            Found = 0
        end,
        case Sum < X of
          true ->
            Options2 = lists:delete(OptionJ, Options),
            %io:format("  J=~p Choice2=~w Options2=~w Sum=~p Found=~p~n", [J, Choice2, Options2, Sum, Found]),
            count(X, D, S, I+1, Choice2, Options2, CountJ + Found);
          false ->
            CountJ + Found
        end;
      false ->
        CountJ
    end
  end, Count, Options).


% the maximal size of a subset of the basis set which does not violate the constraints
get_max_size(X, Basis, D) ->
  get_max_size(X, Basis, D, 0, 0).

get_max_size(X, _Basis, _D, S, Sum) when Sum > X ->
  S-1;
get_max_size(_X, _Basis, D, S, _Sum) when S >= D ->
  D;
get_max_size(X, [H|T], D, S, Sum) ->
  get_max_size(X, T, D, S+1, Sum + H).


% The basis is the set of number c_i = i^n from which we can combine x:
% x = sum_{i=1}^d c_i x_i (x_i in {0, 1}, c_d <= x, c_{d+1} > x)
get_basis(X, N) ->
  get_basis(X, N, 1, []).

get_basis(X, N, I, Basis) ->
  C = pow(I, N),
  case C > X of
    true ->
      Basis;
    false ->
      get_basis(X, N, I+1, Basis ++ [C])
  end.


pow(I, 1) ->
  I;
pow(I, N) ->
  I * pow(I, N-1).
