-module(rps).

-behavior(gen_statem).

%api
-export([drain/3, try_play/0, find_player/2, move/2, play/1,
	 queue_up/3, spawn_coordinator/3, start/0, statistics/1,
	 try_it/0]).

%callback functions
-export([callback_mode/0, code_change/4, init/1,
	 terminate/3]).

%state functions
-export([wait_move1/3, wait_move2/3]).

start() ->
    try spawn(fun () -> loop(#{},0, 0, 0, #{}, nothing) end) of
      BrokerPid -> {ok, BrokerPid}
    catch
      _:Reason -> {error, Reason}
    end.

%synchronous/blocking
queue_up(BrokerRef, Name, Rounds) ->
    BrokerRef ! {Rounds, Name, self(), queue_up},
    receive
      {ok, OtherPlayer, Coord} -> {ok, OtherPlayer, Coord};
      {error, Reason} -> {error, Reason}
    end.

drain(BrokerRef, Pid, Msg) ->  
  BrokerRef ! {drain},
  case Pid =/= none of 
    true -> Pid ! Msg
  end.

loop(Players, LongestGame, NumberOfGames, InQueue, CoordMap, Drain) -> 
  receive 
    {Rounds, P1Name, P1Pid, queue_up} ->
      if 
      Drain =:= drain ->
        P1Pid ! {"no game is being played"};
      true -> 
        P1 = {Rounds, {P1Name, P1Pid}},
        {P2, UpdatedPlayers} = find_player(P1, Players),
        case P2 of
          {match, {P2Name, P2Pid}} ->
            Coord = spawn_coordinator(Rounds, P1Pid,P2Pid),
            P1Pid ! {ok, P2Name, Coord}, 
            P2Pid ! {ok, P1Name, Coord},
            CoordMap1 = maps:merge(#{Coord => {P1Pid, P2Pid}}, CoordMap),
            loop(UpdatedPlayers, LongestGame, NumberOfGames + 1, InQueue - 1, CoordMap1, Drain);
          {waiting} -> 
            loop(UpdatedPlayers, LongestGame, NumberOfGames, InQueue + 1, CoordMap, Drain)
        end
      end;
    {Pid, {statistics}} ->
      Pid ! {ok, LongestGame, NumberOfGames, InQueue},
      loop(Players, LongestGame, NumberOfGames, InQueue, CoordMap, Drain);
    {game_over, Count} ->
      LongestGame1 = case Count > LongestGame of
        true -> Count;
        false -> LongestGame
      end,
      loop(Players, LongestGame1, NumberOfGames-1, InQueue, CoordMap, Drain);
    {drain} -> 
      Drain1 = drain,
      drainInQueue(Players),
      draining(CoordMap),
      loop(Players, LongestGame, NumberOfGames, InQueue, CoordMap, Drain1)
  end.

%get all keys from queue and send them a message that we are draining
drainInQueue(Players) ->
  case maps:size(Players) of
    0 -> #{};
    _ ->
      [H|_] = maps:keys(Players),
      {Player, UpdatedPlayers} = maps:take(H, Players),
      Player ! {server_stopping},
      drainInQueue(UpdatedPlayers)
  end.

draining(CoordMap) -> 
  case maps:size(CoordMap) of
    0 -> #{};
    _ -> 
      MapKeys = maps:keys(CoordMap),
      [H|_] = MapKeys,
      {{P1Pid, P2Pid}, CoordMap1} = maps:take(H, CoordMap),
      gen_statem:cast(H, {drain, {P1Pid, P2Pid}, H, none}),
      draining(CoordMap1)
    end.

move(Coordinator, Choice) ->
    gen_statem:cast(Coordinator, {none, {self(), nothing}, Coordinator, Choice}),
    receive 
      {Reply} -> Reply
    end.

statistics(BrokerRef) ->
    BrokerRef ! {self(), {statistics}},
    receive
      {ok, LongestGame, InQueue, Ongoing} ->
	      {ok, LongestGame, InQueue, Ongoing};
      {_, _, _, _} -> nothing
    end.

%finds a player that wants to play same amount of rounds and removes from map or add the player to the map
find_player(Player, Players) ->
    {Rounds, P1} = Player,
    case maps:take(Rounds, Players) of
      {P2, UpdatedPlayers} ->
	      {{match, P2}, UpdatedPlayers};
      error ->
	      {{waiting}, maps:merge(#{Rounds => P1}, Players)}
    end.

%needs to be linked with the broker
spawn_coordinator(NumberOfRounds, P1, Player2) ->
  {ok, Coord} = gen_statem:start_link({local, coordinator}, ?MODULE,
		     [self(), NumberOfRounds, P1, Player2], []),
  Coord.

%mandatory callback functions
terminate(_Reason, _State, _Data) -> void.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

callback_mode() -> state_functions.

init([BrokerPid, NumberOfRounds, P1, P2]) ->
    {ok, wait_move1,
     {nothing, 0, BrokerPid, P1, P2, NumberOfRounds, nothing, []}}.

check_valid(From, P1Pid, P2Pid) ->
  case From of
    {Pid, nothing} ->
      case (Pid =:= P1Pid) or (Pid =:= P2Pid) of 
        true -> true;
        false -> false
      end;
    {Pid1, Pid2} ->
      case (({Pid1, Pid2} =:= {P1Pid, P2Pid}) or ({Pid1, Pid2} =:= {P2Pid, P1Pid})) of
        true -> true;
        false -> false
      end
  end. 

%state callbacks
wait_move1(cast, {Drain, From, _Coord, Move},
	     {DrainP,Count, BrokerPid, P1Pid, P2Pid, Rounds, Current, Total}) ->
  {From1,_} = From,
  case check_valid(From, P1Pid, P2Pid) of 
    false -> {keep_state, {DrainP, Count, BrokerPid, P1Pid, P2Pid, Rounds, Current, Total}};
    true ->
      Current1 = 
        case From1 =:= P1Pid of
          true -> {Move, nothing};
          false -> {nothing, Move}
        end,
      io:format(Move),
      if
      Drain =:= drain -> 
        {P1, P2} = From,
        DrainT = {drain, P1, P2}, 
        {next_state, wait_move2, {DrainT, Count, BrokerPid, P1Pid, P2Pid, Rounds, Current1, Total}};
      true ->
        {next_state, wait_move2, {DrainP, Count, BrokerPid, P1Pid, P2Pid, Rounds, Current1, Total}}
      end
  end.

wait_move2(cast, {Drain, From, Coord, Move}, 
               {DrainT, Count, BrokerPid, P1Pid, P2Pid, Rounds, Current, Total}) ->
  if
  Drain =:= drain -> 
    {From1,From2} = From,
    From1 ! {server_stopping},  
    From2 ! {server_stopping},  
    gen_statem:stop(Coord);
  true ->   
    %check if drain was cast from state move1
    if 
    DrainT =:= {drain, P1Pid, P2Pid} -> 
      {_, P1, P2} = DrainT,
      P1 ! {server_stopping},  
      P2 ! {server_stopping},  
      gen_statem:stop(Coord);
    true ->
      case check_valid(From, P1Pid, P2Pid) of
        false -> {keep_state, {DrainT, Count, BrokerPid, P1Pid, P2Pid, Rounds, Current, Total}};
        true ->
          Current1 = case Current of
            {nothing, OtherMove} -> {Move, OtherMove};
            {OtherMove, nothing} -> {OtherMove, Move}
          end,
          Result = play(Current1),
          case Result of
            tie ->
              P1Pid ! {tie}, 
              P2Pid ! {tie},
              {next_state, wait_move1, {DrainT, Count+1, BrokerPid, P1Pid, P2Pid, Rounds, nothing, Total}};
            player1 ->
              P1Pid ! {win}, 
              P2Pid ! {lose},
              Total1 = Total ++ [player1],
              case length(Total1) > Rounds/2 of
                true -> terminate_game(Count+1, BrokerPid, Total1, P1Pid, P2Pid, Coord);
                false -> 
                  {next_state, wait_move1, {DrainT, Count+1, BrokerPid, P1Pid, P2Pid, Rounds, nothing, Total1}}
              end;
            player2 ->
              P1Pid ! {lose},
              P2Pid ! {win},
              Total1 = Total ++ [player2],
              case length(Total1) > Rounds/2 of
                true -> terminate_game(Count+1, BrokerPid, Total1, P1Pid, P2Pid, Coord);
                false -> 
                  {next_state, wait_move1, {DrainT, Count+1, BrokerPid, P1Pid, P2Pid, Rounds, nothing, Total1}}
              end
          end
      end
    end
  end.

sum_total([], P1, P2) -> {P1, P2};
sum_total(Total, P1, P2) ->
  [H|T] = Total,
  case H of
    player1 -> sum_total(T, P1+1, P2);
    player2 -> sum_total(T, P1, P2+1)
  end.

terminate_game(Count, BrokerPid, Total, P1Pid, P2Pid, Coord) ->
  {P1, P2} = sum_total(Total, 0, 0),
  P1Pid ! {game_over, P1, P2},
  P2Pid ! {game_over, P2, P1},
  BrokerPid ! {game_over, Count},
  gen_statem:stop(Coord).

play(Current) ->
    case Current of
      {rock, rock} -> tie;
      {paper, paper} -> tie;
      {scissors, scissors} -> tie;
      {rock, paper} -> player2;
      {rock, scissors} -> player1;
      {paper, scissors} -> player2;
      {paper, rock} -> player1;
      {scissors, rock} -> player2;
      {scissors, paper} -> player1;
      {scissors, _} -> player1;
      {paper, _} -> player1;
      {rock, _} -> player1;
      {_, paper} -> player2;
      {_, scissors} -> player2;
      {_, rock} -> player2
    end.

try_it() -> 
  {ok, F} = spawn_coordinator(3, self(), self()), 
  F.

try_play() ->
  {ok, B} = start(),
  spawn(fun() -> p1(B, "player1", 5) end),
  spawn(fun() -> p2(B, "player2", 5) end),
  B.

p1(Broker, Name, Rounds) -> 
  {ok, OtherPlayer, Coordinator} = queue_up(Broker, Name, Rounds),
  joining(Coordinator, rock).

p2(Broker, Name, Rounds) ->
  {ok, OtherPlayer, Coordinator} = queue_up(Broker, Name, Rounds),
  joining(Coordinator, paper).

joining(Coordinator, Move) ->
  move(Coordinator, Move),
  move(Coordinator, Move).
