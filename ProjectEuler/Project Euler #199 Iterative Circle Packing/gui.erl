%% GUI Version of Project Euler #199
%%
%% derived from hello2.erl and ex_canvas.erl examples
%% by Marc van Woerkom <mvanwoerkom@acm.org>
%%
%% usage: gui:start()

-module(gui).

-include_lib("wx/include/wx.hrl").

-export([start/0,
         init/1, handle_info/2, handle_sync_event/3, 
         handle_event/2, handle_call/3,
         code_change/3, terminate/2]).

-behaviour(wx_object).

-record(state, 
	{
	  win, 
	  bitmap, 
	  canvas, 
	  overlay,
	  pos,
	  n,
	  m
	}).

start() ->
    wx_object:start_link(?MODULE, [], []).

%% Init is called in the new process.
init([]) ->
    N = 3,
    M = 3,

    wx:new(),
    Frame = wxFrame:new(wx:null(), 
			-1, % window id
			"Euler #199: Iterative Circle Packing", % window title
			[{size, {600,400}}]),
    

    %% if we don't handle this ourselves, wxwidgets will close the window
    %% when the user clicks the frame's close button, but the event loop still runs
    wxFrame:connect(Frame, close_window),

    Panel = wxPanel:new(Frame, []),
    
    Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Panel, 
				 [{label, 
				   "Input Data: n=" ++ integer_to_list(N) ++ 
				       ", m=" ++ integer_to_list(M)}]),

    Canvas = wxPanel:new(Panel, [{style, ?wxFULL_REPAINT_ON_RESIZE}]),

    wxPanel:connect(Canvas, paint, [callback]),
    wxPanel:connect(Canvas, size),
    wxPanel:connect(Canvas, left_down),
    wxPanel:connect(Canvas, left_up),
    wxPanel:connect(Canvas, motion),

    wxSizer:add(Sizer, Canvas, [{flag, ?wxEXPAND},
				{proportion, 1}]),

    wxPanel:setSizer(Panel, Sizer),
    wxSizer:layout(Sizer),

    {W, H} = wxPanel:getSize(Canvas),
    Bitmap = wxBitmap:new(max(W, 30), max(30, H)),
    
    wxFrame:createStatusBar(Frame, []),
    ok = wxFrame:setStatusText(Frame, "Computo ergo sum!", []),

    wxWindow:show(Frame),

    % This seems to be the initial state
    {Frame, #state{win = Frame, 
		   bitmap = Bitmap, 
		   canvas = Canvas, 
		   overlay = wxOverlay:new(),
		   n = N,
		   m = M
		  }}.


%% Handled as in normal gen_server callbacks
handle_info(Msg, State) ->
    io:format("Got Info ~p~n",[Msg]),
    {noreply,State}.

handle_call(Msg, _From, State) ->
    io:format("Got Call ~p~n",[Msg]),
    {reply,ok,State}.


%% Sync event from callback events, paint event must be handled in callbacks
%% otherwise nothing will be drawn on windows.
handle_sync_event(#wx{event = #wxPaint{}}, _wxObj,
		  #state{canvas=Canvas, bitmap=Bitmap}) ->
    DC = wxPaintDC:new(Canvas),
    redraw(DC, Bitmap),
    wxPaintDC:destroy(DC),
    ok.

%% Async Events are handled in handle_event as in handle_info

handle_event(#wx{event = #wxSize{size={W,H}}},
	     State = #state{bitmap=Prev, canvas=Canvas, n=N, m=M}) ->
    if W > 0 andalso H > 0 ->
	    Bitmap = wxBitmap:new(W, H),
	    Fun = fun(DC) ->
		    draw(DC, W, H, N, M)
		  end,
	    draw(Canvas, Bitmap, Fun),
	    wxBitmap:destroy(Prev),
	    {noreply, State#state{bitmap = Bitmap}};
       true ->
	    {noreply, State}
    end;
handle_event(#wx{event = #wxMouse{type=left_down, x=X, y=Y}}, State) ->
    {noreply, State#state{pos={X,Y}}};
handle_event(#wx{event = #wxMouse{type=motion, x=X1, y=Y1}},
	     #state{pos=Start, overlay=Overlay, canvas=Canvas} = State) ->
    case Start of
	undefined -> ignore;
	{X0,Y0} ->
	    DC = wxClientDC:new(Canvas),
	    DCO = wxDCOverlay:new(Overlay, DC),
	    wxDCOverlay:clear(DCO),
	    wxDC:setPen(DC, ?wxLIGHT_GREY_PEN),
	    wxDC:setBrush(DC, ?wxTRANSPARENT_BRUSH),
	    wxDC:drawRectangle(DC, {X0,Y0, X1-X0, Y1-Y0}),
	    wxDCOverlay:destroy(DCO),
	    wxClientDC:destroy(DC)
    end,
    {noreply, State};
handle_event(#wx{event = #wxMouse{type=left_up}},
	     #state{overlay=Overlay, canvas=Canvas} = State) ->
    DC = wxClientDC:new(Canvas),
    DCO = wxDCOverlay:new(Overlay, DC),
    wxDCOverlay:clear(DCO),
    wxDCOverlay:destroy(DCO),
    wxClientDC:destroy(DC),
    wxOverlay:reset(Overlay),
    {noreply, State#state{pos=undefined}};

handle_event(#wx{event=#wxClose{}}, State = #state{win=Frame}) ->
    io:format("~p Closing window ~n",[self()]),
    ok = wxFrame:setStatusText(Frame, "Closing...",[]),
    wxWindow:destroy(Frame),
    {stop, normal, State};
handle_event(Ev = #wx{}, State = #state{}) ->
    io:format("Got Event ~p\n", [Ev]),
    {noreply, State}.


code_change(_, _, State) ->
    {stop, not_yet_implemented, State}.

terminate(_Reason, _State) ->
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Buffered makes it all appear on the screen at the same time
draw(Canvas, Bitmap, Fun) ->
    MemoryDC = wxMemoryDC:new(Bitmap),
    Fun(MemoryDC),

    CDC = wxWindowDC:new(Canvas),
    wxDC:blit(CDC, {0,0},
	      {wxBitmap:getWidth(Bitmap), wxBitmap:getHeight(Bitmap)},
	      MemoryDC, {0,0}),    
    wxWindowDC:destroy(CDC),
    wxMemoryDC:destroy(MemoryDC).

redraw(DC, Bitmap) ->
    MemoryDC = wxMemoryDC:new(Bitmap),
    wxDC:blit(DC, {0,0},
	      {wxBitmap:getWidth(Bitmap), wxBitmap:getHeight(Bitmap)},
	      MemoryDC, {0,0}),
    wxMemoryDC:destroy(MemoryDC).


%%
%% And now the solution ..
%%

draw(DC, W, H, N, M) ->
    CX = W div 2,
    CY = H div 2,
    
    RoutS = min(CX, CY) - 5,  % radius 1.0 on screen
    
    wxDC:clear(DC),
    
    % outer circle
    wxDC:setBrush(DC, ?wxTRANSPARENT_BRUSH),
    wxDC:drawCircle(DC, {CX, CY}, round(RoutS)),

    % circles on the rim
    R0 = r0(N),
    R0S = R0*RoutS,
    Rrim = 1.0-R0,
    RrimS = Rrim*RoutS,
    TwoPi = 2.0*math:pi(),
    wxDC:setBrush(DC, ?wxRED_BRUSH),
    lists:foreach(fun(K) ->
			  PhiK = K*TwoPi/N,
			  X = x(RrimS, PhiK),
			  Y = y(RrimS, PhiK),
			  wxDC:drawCircle(DC, {CX+round(X), CY-round(Y)}, round(R0S))
		  end, lists:seq(0, N-1)),
    wxDC:setBrush(DC, ?wxGREEN_BRUSH),
    K0 = 1.0/R0,
    Kout = -1.0,
    K1 = descartes(K0, K0, Kout),
    lists:foreach(fun(K) ->
			  PhiK = K*TwoPi/N,
			  X = x(RrimS, PhiK),
			  Y = y(RrimS, PhiK),
			  PhiK2 = (K+1)*TwoPi/N,
			  XP = x(RrimS, PhiK2),
			  YP = y(RrimS, PhiK2),
			  {X2, Y2} = descartes(K0, K0, Kout, K1, {X, Y}, {XP, YP}, {0.0, 0.0}),
			  R1 = 1.0/K1,
			  R1S = R1*RoutS,
			  wxDC:drawCircle(DC, {CX+round(X2), CY-round(Y2)}, round(R1S))
		  end, lists:seq(0, N-1)),
    Rin = 1.0-2.0*R0,
    RinS = Rin*RoutS,
    wxDC:drawCircle(DC, {CX, CY}, round(RinS)),
    ok.


% radius of a circle on the rim
r0(N) ->
  Phi2 = math:pi()/N,
  SinPhi2 = math:sin(Phi2),  % sin phi/2 = r0/(1-r0)
  SinPhi2/(1.0+SinPhi2).


% return only the solution with larger curvature
descartes(K1, K2, K3) ->
  Sum = K1+K2+K3,
  TwoRoot = 2.0*math:sqrt(K1*K2+K2*K3+K3*K1),
  Sum+TwoRoot.

descartes(K1, K2, K3, K4, Z1, Z2, Z3) ->
  K1Z1 = smult(K1, Z1),
  K2Z2 = smult(K2, Z2),
  K3Z3 = smult(K3, Z3),
  Sum = add(K1Z1, K2Z2, K3Z3),
  K1K2Z1Z2 = mult(K1Z1, K2Z2),
  K2K3Z2Z3 = mult(K2Z2, K3Z3),
  K3K1Z3Z1 = mult(K3Z3, K1Z1),
  Sum2 = add(K1K2Z1Z2, K2K3Z2Z3, K3K1Z3Z1),
  Root = sqrt(Sum2),
  TwoRootP = smult(2.0, Root),
  TwoRootN = smult(-2.0, Root),
  Z4P = smult(1.0/K4, add(Sum, TwoRootP)),
  Z4N = smult(1.0/K4, add(Sum, TwoRootN)),
  {X4P, Y4P} = Z4P,
  {X4N, Y4N} = Z4N,
  R4P = r(X4P, Y4P),
  R4N = r(X4N, Y4N),
  % TODO check this criterion to pick the correct solution
  case R4P >= R4N of
    true -> 
      Z4P;
    false ->
      Z4N
  end.


% complex math

add(Z1, Z2) ->
  {X1, Y1} = Z1,
  {X2, Y2} = Z2,
  X = X1+X2,
  Y = Y1+Y2,
  {X, Y}.

add(Z1, Z2, Z3) ->
  add(add(Z1, Z2), Z3).

smult(S, Z1) ->
  {X1, Y1} = Z1,
  X = S*X1,  
  Y = S*Y1,
  {X, Y}.

mult(Z1, Z2) ->
  {X1, Y1} = Z1,
  {X2, Y2} = Z2,
  X=X1*X2-Y1*Y2,
  Y=X1*Y2+X2*Y1,
  {X, Y}.

r(X, Y) ->
  math:sqrt(X*X+Y*Y).

phi(X, Y) ->
  Phi = math:atan2(Y, X),
  case Phi >= 0.0 of
    true ->  
      Phi;
    false ->
      2.0*math:pi()+Phi
  end.

x(R, Phi) ->
  R*math:cos(Phi).

y(R, Phi) ->
  R*math:sin(Phi).

% https://math.stackexchange.com/a/44500/86776
sqrt2(Z) ->
  {X, Y} = Z,
  R = r(X, Y),
  R2 = math:sqrt(R),
  ZplusR = add(Z, {R, 0.0}),
  {XP, YP} = ZplusR,
  smult(R2/r(XP, YP), ZplusR).

sqrt(Z) ->
  {X, Y} = Z,
  R = r(X, Y),
  R2 = math:sqrt(R),
  Phi = phi(X, Y),
  Phi2 = Phi/2.0,  
  X2 = x(R2, Phi2),  
  Y2 = y(R2, Phi2),
  {X2, Y2}.  
