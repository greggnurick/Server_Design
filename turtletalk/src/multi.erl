-module(multi).
-export([draw/0, draw_to/2]).

draw() ->
    {ok, C1} = turtletalk:new_canvas(),
    {ok, T1} = turtletalk:new_turtle(C1),

    {ok, C2} = turtletalk:new_canvas(),
    {ok, T2} = turtletalk:new_turtle(C2),

    Pics =
        lists:map(
          fun({C, T, S}) ->
                  turtletalk:setpen(T, down),
                  {ok, [TC]} = turtletalk:clone(T, 1),
                  turtletalk:forward(T, S),
                  turtletalk:anti(T, 90),
                  turtletalk:forward(T, S),

                  turtletalk:anti(TC, 90),
                  turtletalk:forward(TC, S),
                  turtletalk:clock(TC, 90),
                  turtletalk:forward(TC, S),

                  {ok, Pic} = turtletalk:picture(C),
                  turtletalk:blast(C),
                  Pic
          end, [{C1, T1, 50}, {C2, T2, 75}]),
    Pics.

draw_to(File1, File2) ->
    [Pic1, Pic2] = draw(),
    svg:write(File1, Pic1),
    svg:write(File2, Pic2).
