-module(svg).
-export([write/2, pic_to_svg/1]).

write(File, Picture) ->
    {ok, Fd} = file:open(File, [write]),
    Svg = pic_to_svg(Picture),
    file:write(Fd, Svg),
    file:close(Fd).

pic_to_svg(Picture) ->
    Points = lists:append(tuple_to_list(lists:unzip(Picture))),
    {Minx,Maxy} = lists:foldl (fun({X1,Y1}, {X2,Y2}) ->
                                       {min(X1, X2), max(Y1, Y2)} end,
                               hd(Points), tl(Points)),
    Xdir = 1 + if Minx < 0 -> abs(Minx); true -> 0 end,
    Ydir = 1 + Maxy,
    Lines = lists:map(fun svgpath/1, Picture),
    ["<svg xmlns=\"http://www.w3.org/2000/svg\">",
     string_format("<g transform=\"translate(~B, ~B) scale(1,-1)\">~n",
                   [Xdir, Ydir]),
     "<path d=\"",Lines, "\" stroke=\"black\"/>"
     "</g></svg>"].

svgpath ({{X1,Y1}, {X2,Y2}}) ->
    io_lib:format("M ~B ~B L ~B ~B " , [X1, Y1, X2, Y2]).

string_format(S, L) ->
    lists:flatten(io_lib:format(S, L)).
