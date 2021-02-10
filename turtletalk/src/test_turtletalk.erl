-module(test_turtletalk).
-compile(export_all).

-include_lib("eqc/include/eqc.hrl").

turtle_moves() ->
    [setpen, forward, position, clock, anti, clone].

angles() ->
    [0,90,180,270].

pen() -> 
    [up, down].

turtle_generator() ->
    elements(turtle_moves()).

angle_generator() ->
    elements(angles()).

pen_generator() ->
    elements(pen()).
int_generator() ->
    ?SUCHTHAT(I, int(), I > 0).

get_corresponding_generator(X) ->
    A = #{setpen=> {setpen, [pen_generator()]},
        anti => {anti, [angle_generator()]},
        forward => {forward, [int_generator()]},
        clone => {forward, [int_generator()]},
        position => {position, []},
        clock => {clock, [angle_generator()]}},
    maps:get(X, A).

turtle_cmd() ->
    ?LET(I, turtle_generator(), get_corresponding_generator(I)).

turtle_cmds() -> 
    ?SUCHTHAT(I, list(turtle_cmd()), length(I) < 20).


cmds_to_picture(L) ->
    {ok, X} = turtletalk:new_canvas(),
    {ok, X1} = turtletalk:new_turtle(X),
    {ok, Pic} = recurse(L, X, X1),
    Pic.

recurse([], X, _X1) -> turtletalk:picture(X);
recurse(L, X, X1) ->
    [Cmd | Rest] = L,
    {Command, Args} = Cmd,
    eval({call, turtletalk, Command, [X1 | Args]}),
    recurse(Rest, X, X1).

prop_no_empty() ->
    ?FORALL(Picture, turtle_cmds(), no_empty_line(cmds_to_picture(Picture)) == true).

no_empty_line([]) -> true;
no_empty_line([Line|Lines]) ->
    {{X, Y}, {X1, Y1}} = Line,
    if
    (X =:= X1) and (Y =:= Y1) ->
        false;
    true ->
        no_empty_line(Lines)
    end.

test_all() ->
    eqc:module(test_turtletalk).

test_everything () ->
    eunit_test:test_all(),
    test_all().

