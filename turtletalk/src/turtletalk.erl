-module(turtletalk).
-behaviour(gen_server).

-export([new_canvas/0, blast/1, new_turtle/1, picture/1, turtles/1, graveyard/1]).
-export([forward/2, anti/2, clock/2, setpen/2, clone/2, position/1]).
-export([addLine/3, killTurtle/2, cloneTurtle/5]).
-export([init/1, handle_call/3, handle_cast/2]).

% -type position() :: {integer(), integer()}.
% -type line_seg() :: {position(), position()}.
% -type picture()  :: [line_seg()].

% calls from turtle ---------

addLine(C, T, Line) ->
    gen_server:cast(C, {addline, T, Line}).
killTurtle(C, T) -> 
    gen_server:cast(C, {killturtle, T}).
cloneTurtle(C, N, Pos, PenDown, Angle) ->
    gen_server:call(C, {clones, N, Pos, PenDown, Angle}).

%% Turtle API ---------------

forward(T, N) -> 
    turtlefunc:forward(T, N).
anti(T, D) -> 
    turtlefunc:anti(T, D).
clock(T, D) -> 
    turtlefunc:clock(T, D).
setpen(T, P) -> 
    turtlefunc:setpen(T, P).
clone(T, N) -> 
    turtlefunc:clone(T, N).
position(T) -> 
    turtlefunc:position(T).

% Canvas API ----------------

new_canvas() -> 
    gen_server:start(?MODULE, #{}, []).
blast(C) -> 
    gen_server:cast(C, blast).
new_turtle(C) -> 
    gen_server:call(C, newturtle).
picture(C) -> 
    gen_server:call(C, picture).
turtles(C) ->
    gen_server:call(C, turtles).
graveyard(C) -> 
    gen_server:call(C, graveyard).

% gen_server functions ------

init(Data) ->
    {ok, Data}.

% calls ---------------------

handle_call(newturtle, _From, Map) ->
    {ok, T} = turtlefunc:start({{0, 0}, false, 0, self()}),
    {reply, {ok, T}, maps:put(T, {alive, []}, Map)};

handle_call(picture, _From, Map) ->
    Keys = maps:keys(Map),
    {{_Alive, PicA}, {_Dead, _PicB}} = aliveAndDead(Keys, Map, {{[], []}, {[], []}}),
    {reply, {ok, PicA}, Map};

handle_call(turtles, _From, Map) ->
    Keys = maps:keys(Map),
    {{Alive, _PicA}, {Dead, _PicB}} = aliveAndDead(Keys, Map, {{[], []}, {[], []}}),
    {reply, {ok, {Alive, length(Dead)}}, Map};

handle_call(graveyard, _From, Map) ->
    Keys = maps:keys(Map),
    {{_Alive, _PicA}, {_Dead, PicB}} = aliveAndDead(Keys, Map, {{[], []}, {[], []}}),
    {reply, {ok, PicB}, Map};

handle_call({clones, N, Pos, PenDown, Angle}, _From, Map) ->
    {TL, NewMap} = makeClones(self(), N, Pos, PenDown, Angle, [], #{}),
    {reply, TL, maps:merge(NewMap, Map)}.

% casts ---------------------

handle_cast(blast, _Map) ->
    gen_server:stop(self());

handle_cast({addline, T, Line}, Map) ->
    {Life, Pic} = maps:get(T, Map),
    Pic1 = [Line|Pic],
    {noreply, maps:update(T, {Life, Pic1}, Map)};

handle_cast({killturtle, T}, Map) ->
    {_Life, Pic} = maps:get(T, Map),
    {noreply, maps:update(T, {dead, Pic}, Map)}.

% helper functions ----------

makeClones(_C, 0, _Pos, _PenDown, _Angle, TL, Map) -> {TL, Map};
makeClones(C, N, Pos, PenDown, Angle, TL, Map) ->
    {ok, T} = turtlefunc:start({Pos, PenDown, Angle, C}),
    Map1 = maps:put(T, {alive, []}, Map),
    makeClones(C, N-1, Pos, PenDown, Angle, [T|TL], Map1).

aliveAndDead([], _Map, {{Alive, PicA}, {Dead, PicB}}) -> {{Alive, PicA}, {Dead, PicB}};
aliveAndDead([Turtle|Ts], Map, {{Alive, PicA}, {Dead, PicB}}) ->
    {Life, Pic} = maps:get(Turtle, Map),
    case Life of
        alive -> aliveAndDead(Ts, Map, {{[Turtle|Alive], Pic ++ PicA}, {Dead, PicB}});
        dead -> aliveAndDead(Ts, Map, {{Alive, PicA}, {[Turtle|Dead], Pic ++ PicB}})
    end.