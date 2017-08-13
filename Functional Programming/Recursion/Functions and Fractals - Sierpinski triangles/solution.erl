-module(solution).
-export([main/0]).

main() ->
  {ok, [N]} = io:fread("", "~d"),
  Image = sierpinski(N),
  write_image(Image).

sierpinski(N) ->
  Image = empty_image(),
  {W, H, _Bits} = Image,
  sierpinski(0, N, Image, 0, 0, W, H).

sierpinski(0, N, Image, X, Y, TW, TH) ->
  Image1 = draw_up_triangle(Image, X, Y, TW, TH),
  sierpinski(1, N, Image1, X, Y, TW, TH);
sierpinski(I, N, Image, _X, _Y, _TW, _TH) when I > N ->
  Image;
sierpinski(I, N, Image, X, Y, TW, TH) ->
  Image1 = draw_down_triangle(Image, X, Y, TW, TH),
  TW2 = TW div 2,
  TH2 = TH div 2,
  TW3 = TW2 div 2,
  X1 = X + TW2 + 1,
  X2 = X + TW3 + 1,
  Y1 = Y + TH2,
  Image2 = sierpinski(I + 1, N, Image1, X, Y, TW2, TH2),
  Image3 = sierpinski(I + 1, N, Image2, X1, Y, TW2, TH2),
  sierpinski(I + 1, N, Image3, X2, Y1, TW2, TH2).

draw_up_triangle(Image, X, Y, TW, TH) ->
  lists:foldl(fun(J, Img) ->
                draw_hline(Img, X + J, X + TW - 1 - J, Y + J, 1)
              end, Image, lists:seq(0, TH - 1)).

draw_down_triangle(Image, X, Y, TW, TH) ->
  TW2 = TW div 2,
  TH2 = TH div 2,
  TW3 = TW2 div 2,
  X1 = X + TW3 + 1,
  X2 = X1 + TW2 - 1,
  Y1 = Y + TH2 - 1,
  lists:foldl(fun(J, Img) ->
                draw_hline(Img, X1 + J, X2 - J, Y1 - J, 0)
              end, Image, lists:seq(0, TH2 - 1)).


draw_hline(Image, X1, X2, Y, Value) ->
  lists:foldl(fun(X, Img) ->
                draw(Img, X, Y, Value)
              end, Image, lists:seq(X1, X2)).


% Low level stuff (Operating on the bitstring)

% Drawing
% Image has dimensions 63 x 23
empty_image() ->
  W = 63,
  H = 32,
  Len = W * H,
  Bits = <<0:Len>>,
  {W, H, Bits}.

draw(Image, X, Y, Value) ->
  {W, H, Bits} = Image,
  if
    X < 0; X >= W; Y < 0; Y >= H ->
      io:format("Out of bounds: X = ~p, Y = ~p~n", [X,Y]),
      Image;
    true ->
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
