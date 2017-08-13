-module(solution).
-export([main/0]).

main() ->
  {ok, [N]} = io:fread("", "~d"),
  Image = tree(N),
  write_image(Image).

tree(N) ->
  Image = empty_image(),
  {W, _H, _Bits} = Image,
  tree(Image, 1, N, (W div 2)-1, 0, 16).

tree(Image, I, N, _X, _Y, _L) when I > N ->
  Image;
tree(Image, I, N, X, Y, L) ->
  Image1 = lists:foldl(fun(J, ImageJ) ->
    Img1 = draw(ImageJ, X, Y+J, 1),
    Img2 = draw(Img1, X-(J+1), Y+L+J, 1),
    draw(Img2, X+(J+1), Y+L+J, 1)                     
  end, Image, lists:seq(0, L-1)),
  Image2 = tree(Image1, I+1, N, X-L, Y+2*L, L div 2),
  tree(Image2, I+1, N, X+L, Y+2*L, L div 2).


% Low level stuff (Operating on the bitstring)

% Drawing
empty_image() ->
  W = 100,
  H = 63,
  Len = W * H,
  Bits = <<0:Len>>,
  {W, H, Bits}.

draw(Image, X, Y, Value) ->
  {W, H, Bits} = Image,
  case X < 0 orelse X >= W orelse Y < 0 orelse Y >= H of
    true ->
      io:format("Out of bounds: X = ~p, Y = ~p~n", [X,Y]),
      Image;
    false ->
      LenA = (H - Y - 1) * W + X,
      LenC = W * H - LenA - 1,
      <<A:LenA, _B:1, C:LenC>> = Bits,
      Bits2 = <<A:LenA, Value:1, C:LenC>>,
      {W, H, Bits2}
  end.


% Output
write_image(Image) ->
  {W, H, Bits} = Image,
  lists:foreach(fun(I) -> 
                  LenH = (I - 1) * W,
                  <<_H:LenH/bitstring, Line:W/bitstring, _T/bitstring>> = Bits,
                  write_line(W, Line)
                end, lists:seq(1, H)).

write_line(W, Line) when is_bitstring(Line) ->
  lists:foreach(fun(J) ->
                  LenH = J - 1,
                  <<_H:LenH/bitstring, Bit:1/bitstring, _T/bitstring>> = Line,
                  Char = bit_to_char(Bit),
                  io:format("~c", [Char])
                end, lists:seq(1, W)),
  io:format("~n").

bit_to_char(Bit) when is_bitstring(Bit) ->
  case Bit of
    <<0:1>> ->
      $\_;  % _
    <<1:1>> ->
      $1;
    _Else ->
      $?
  end.
