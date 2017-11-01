-module(solution).
-export([main/0]).

main() ->
  {ok, [T]} = io:fread("", "~d"),
  lists:foreach(fun(_T) ->
    {ok, [N]} = io:fread("", "~d"),
    L = read_list(N),
    {ok, [LoginAttempt]} = io:fread("", "~s"),
    Result = check(LoginAttempt, L),
    io:format("~s~n", [Result])
  end, lists:seq(1, T)),
  true.


check(LoginAttempt, L) ->
  Words = words(L),
  Mem = maps:new(),
  {Result, _Mem2} = match(LoginAttempt, Words, Mem),
  case Result of
    no_match ->
      wrong_password();
    Recognized ->
      Space = $ ,
      lists:join(Space, Recognized)
  end.


% we use the recursive decomposition
%   match(string) = match(word, string) + match(string - word)

match([], _Words, Mem) ->
  {[], Mem};
match(String, Words, Mem) ->
  case maps:is_key(String, Mem) of
    true ->
      {maps:get(String, Mem), Mem};
    false ->
      [H|T] = String,
      case maps:is_key(H, Words) of
        false ->
          % no initial letter of the passwords matches symbol H
          {no_match, maps:put(String, no_match, Mem)};
        true ->
          Candidates = maps:get(H, Words),
          {Result, Mem2} = match(Candidates, H, T, Words, Mem),
          {Result, maps:put(String, Result, Mem2)}
      end
  end.

match([], _H, _T, _Words, Mem) ->
  % all candidate words did not match tail T
  {no_match, Mem};
match([Candidate|OtherCandidates], H, T, Words, Mem) ->
  case match_prefix(Candidate, T) of
    no_match ->
      match(OtherCandidates, H, T, Words, Mem);
    Remaining ->
      {Result, Mem2} = match(Remaining, Words, Mem),
      case Result of
        no_match ->
          match(OtherCandidates, H, T, Words, Mem2);
        ListRemaining ->
          Word = [H|Candidate],
          {[Word|ListRemaining], Mem2}
      end
  end.


match_prefix([], T) ->
  T;
match_prefix(_Candidate, []) ->
  % nothing left to check something against
  no_match;
match_prefix([H|TCandidate], [H|T]) ->
  % candidate and T have H in common, check next symbol
  match_prefix(TCandidate, T);
match_prefix(_Candidate, _T) ->
  % symbols differ
  no_match.


% map words: first symbol -> list of word tails, e.g. "w" -> ["e", "hat"]
words(L) ->
  lists:foldl(fun(Word, Map) ->
    [H|T] = Word,
    case maps:is_key(H, Map) of
      true ->
        Value = maps:get(H, Map),
        Value2 = [T|Value],
        maps:update(H, Value2, Map);
      false ->
        maps:put(H, [T], Map)
    end
  end, maps:new(), L).


wrong_password() ->
  "WRONG PASSWORD".


read_list(N) ->
  Fmt = unicode:characters_to_list(lists:duplicate(N, "~s")),
  {ok, L} = io:fread("", Fmt),
  L.
