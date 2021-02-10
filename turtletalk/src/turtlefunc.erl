-module(turtlefunc).
-behavior(gen_statem).

-export([start/1, forward/2, anti/2, clock/2, setpen/2, clone/2, position/1]).
-export([init/1, terminate/3, callback_mode/0, code_change/4]).
-export([alive/3, dead/3]).

% gen_statem functions ------

terminate(_Reason, _State, _Data) -> void.
callback_mode() -> state_functions.
code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.
init(Data) -> 
    process_flag(trap_exit, true),
    {ok, alive, Data}.

% called to start turtle statem

start(Data) ->
    gen_statem:start_link(?MODULE, Data, []).

% API functions -------------

forward(T, N) ->
    gen_statem:call(T, {forward, N}).
anti(T, D) ->
    gen_statem:call(T, {anti, D}).
clock(T, D) ->
    gen_statem:call(T, {clock, D}).
setpen(T, P) ->
    gen_statem:call(T, {setpen, P}).
clone(T, N) -> 
    gen_statem:call(T, {clone, N}).
position(T) ->
    gen_statem:call(T, position).

% alive state handling ------

alive({call, From}, {forward, N}, {{X, Y}, PenDown, Angle, C}) ->
    if 
    (N > 0) and is_integer(N) ->
        case Angle of
            0 -> X1 = X + N,
            if 
            PenDown -> turtletalk:addLine(C, self(), {{X, Y}, {X1, Y}}),
                {keep_state, {{X1, Y}, PenDown, Angle, C}, [{reply, From, ok}]};
            true -> {keep_state, {{X1, Y}, PenDown, Angle, C}, [{reply, From, ok}]}
            end;
            90 -> Y1 = Y + N,
            if 
            PenDown -> turtletalk:addLine(C, self(), {{X, Y}, {X, Y1}}),
                {keep_state, {{X, Y1}, PenDown, Angle, C}, [{reply, From, ok}]};
            true -> {keep_state, {{X, Y1}, PenDown, Angle, C}, [{reply, From, ok}]}
            end;
            180 -> X1 = X - N,
            if 
            PenDown -> turtletalk:addLine(C, self(), {{X, Y}, {X1, Y}}),
                {keep_state, {{X1, Y}, PenDown, Angle, C}, [{reply, From, ok}]};
            true -> {keep_state, {{X1, Y}, PenDown, Angle, C}, [{reply, From, ok}]}
            end;
            270 -> Y1 = Y - N,
            if 
            PenDown -> turtletalk:addLine(C, self(), {{X, Y}, {X, Y1}}),
                {keep_state, {{X, Y1}, PenDown, Angle, C}, [{reply, From, ok}]};
            true -> {keep_state, {{X, Y1}, PenDown, Angle, C}, [{reply, From, ok}]}
            end
        end;
    true -> 
        if
        N =:= 0 -> {keep_state, {{X, Y}, PenDown, Angle, C}, [{reply, From, ok}]};
        true -> turtletalk:killTurtle(C, self()),
            {next_state, dead, {{X, Y}, PenDown, Angle, C}, [{reply, From, {error, bad_N_value}}]}
        end
    end;

alive({call, From}, {anti, D}, {{X, Y}, PenDown, Angle, C}) ->
    if 
    (D =:= 0) or (D =:= 90) or (D =:= 180) or (D =:= 270) ->
        Angle1 = (Angle + D) rem 360,
        {keep_state, {{X, Y}, PenDown, Angle1, C}, [{reply, From, ok}]};
    true -> 
        turtletalk:killTurtle(C, self()),
        {next_state, dead, {{X, Y}, PenDown, Angle, C}, [{reply, From, {error, bad_D_value}}]}
    end;

alive({call, From}, {clock, D}, {{X, Y}, PenDown, Angle, C}) ->
    if 
    (D =:= 0) or (D =:= 90) or (D =:= 180) or (D =:= 270) ->
        Angle1 = (Angle - D + 360) rem 360,
        {keep_state, {{X, Y}, PenDown, Angle1, C}, [{reply, From, ok}]};
    true -> 
        turtletalk:killTurtle(C, self()),
        {next_state, dead, {{X, Y}, PenDown, Angle, C}, [{reply, From, {error, bad_D_value}}]}
    end;

alive({call, From}, {setpen, P}, {{X, Y}, PenDown, Angle, C}) ->
    if 
    (P =:= up) or (P =:= down) ->
        case P of
            up -> {keep_state, {{X, Y}, false, Angle, C}, [{reply, From, ok}]};
            down -> {keep_state, {{X, Y}, true, Angle, C}, [{reply, From, ok}]}
        end;
    true -> 
        turtletalk:killTurtle(C, self()),
        {next_state, dead, {{X, Y}, PenDown, Angle, C}, [{reply, From, {error, bad_P_value}}]}
    end;

alive({call, From}, {clone, N}, {{X, Y}, PenDown, Angle, C}) ->
    if 
    (N >= 0) and is_integer(N) ->
        TL = turtletalk:cloneTurtle(C, N, {X, Y}, PenDown, Angle),
        {keep_state, {{X, Y}, PenDown, Angle, C}, [{reply, From, {ok, TL}}]};
    true -> 
        turtletalk:killTurtle(C, self()),
        {next_state, dead, {{X, Y}, PenDown, Angle, C}, [{reply, From, {error, bad_N_value}}]}
    end;

alive({call, From}, position, {{X, Y}, PenDown, Angle, C}) ->
    {keep_state, {{X, Y}, PenDown, Angle, C}, [{reply, From, {ok, {X, Y}}}]}.

% dead state handling -------

dead({call, From}, _, {{X, Y}, PenDown, Angle, C}) ->
    {keep_state, {{X, Y}, PenDown, Angle, C}, [{reply, From, {error, turtle_dead}}]}.


