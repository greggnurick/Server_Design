-module(eunit_test).
-export([test_all/0]).

-include_lib("eunit/include/eunit.hrl").
test_all() ->
  eunit:test(
    [
      pic(),
      startc(),
      startt(),
      east(),
      eastclock(),
      move(),
      zero(),
      deadcount()
    ], [verbose]).

deadcount() ->
  {"deads", 
  fun() ->
      {ok, X} = turtletalk:new_canvas(),
      {ok, T} = turtletalk:new_turtle(X),
      {ok, T1} = turtletalk:new_turtle(X),
      {ok, T2} = turtletalk:new_turtle(X),
      turtletalk:forward(T1,-5),
      turtletalk:forward(T,-5),
      {ok, {_, Dead_Count}} = turtletalk:turtles(X),
      ?assertMatch(2, Dead_Count)
  end}.
startc() ->
  {"Start", 
  fun() ->
    ?assertMatch({ok,_} ,turtletalk:new_canvas())
  end}.

startt() ->
  {"Start t", 
  fun() ->
      {ok, X} = turtletalk:new_canvas(),
    ?assertMatch({ok, _}, turtletalk:new_turtle(X))
  end}.


zero() ->
  {"og pos", 
  fun() ->
      {ok, X} = turtletalk:new_canvas(),
      {ok, X1} = turtletalk:new_turtle(X),
      ?assertMatch({ok, {0,0}}, turtletalk:position(X1))
  end}.

move() ->
  {"og pos", 
  fun() ->
      {ok, X} = turtletalk:new_canvas(),
      {ok, X1} = turtletalk:new_turtle(X),
      turtletalk:forward(X1, 5),
      ?assertMatch({ok, {5,0}}, turtletalk:position(X1))
  end}.

pic() ->
  {"move draw", 
  fun() ->
      {ok, X} = turtletalk:new_canvas(),
      {ok, X1} = turtletalk:new_turtle(X),
      turtletalk:setpen(X1, down),
      turtletalk:forward(X1, 5),
      turtletalk:clock(X1, 180),
      turtletalk:forward(X1, 5),
      ?assertMatch({ok, [{{5,0}, {0,0}},{{0,0},{5,0}}]}, turtletalk:picture(X))
  end}.

east() ->
  {"move position", 
  fun() ->
      {ok, X} = turtletalk:new_canvas(),
      {ok, X1} = turtletalk:new_turtle(X),
      turtletalk:forward(X1, 5),
      turtletalk:clock(X1, 180),
      turtletalk:forward(X1, 5),
      ?assertMatch({ok, {0,0}}, turtletalk:position(X1))
  end}.

eastclock() ->
  {"east move", 
  fun() ->
      {ok, X} = turtletalk:new_canvas(),
      {ok, X1} = turtletalk:new_turtle(X),
      turtletalk:forward(X1, 5),
      turtletalk:anti(X1, 90),
      turtletalk:forward(X1, 5),
      ?assertMatch({ok, {5,5}}, turtletalk:position(X1))
  end}.
