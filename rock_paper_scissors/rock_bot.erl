-module(rock_bot).

-export([queue_up_and_play/1]).

queue_up_and_play(Broker) ->
    {ok, _Other, Coor} = rps:queue_up(Broker, "Rock bot(tom)", 3),
    move_to_game_over(Coor).

move_to_game_over(Coor) ->
    case rps:move(Coor, rock) of
        {game_over, Me, SomeLoser} ->
            {ok, Me, SomeLoser};
        server_stopping ->
            server_stopping;
        _ -> move_to_game_over(Coor)
    end.
