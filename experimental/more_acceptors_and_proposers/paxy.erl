-module(paxy).
-export([start/1, start/0, stop/0, stop/1]).

-define(RED, {255,0,0}).
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).
-define(GRAY, {100,100,100}).
-define(ORCHID, {218,112,214}).

% Sleep is a list with the initial sleep time for each proposer
start(Sleep) ->
  AcceptorNames = ["Acceptor a", "Acceptor b", "Acceptor c", "Acceptor d", 
                   "Acceptor e", "Acceptor f"],
  AccRegister = [a, b, c, d, e, f],
  ProposerNames = [{"Proposer kurtz", ?RED}, 
                   {"Proposer kilgore", ?GREEN}, 
                   {"Proposer willard", ?BLUE},
                   {"Proposer caesar", ?GRAY},
                   {"Proposer vladimir", ?ORCHID}],
  PropInfo = [{kurtz, ?RED}, {kilgore, ?GREEN}, {willard, ?BLUE}, {caesar, ?GRAY}, {vladimir, ?ORCHID}],
  register(gui, spawn(fun() -> gui:start(AcceptorNames, ProposerNames) end)),
  gui ! {reqState, self()},
  receive
    {reqState, State} ->
      {AccIds, PropIds} = State,
      start_acceptors(AccIds, AccRegister),
      start_proposers(PropIds, PropInfo, AccRegister, Sleep)
  end,
  true.

start() ->
  Sleep = [100,200,350,215,450],
  start(Sleep).
    
start_acceptors(AccIds, AccReg) ->
  case AccIds of
    [] ->
      ok;
    [AccId|Rest] ->
      [RegName|RegNameRest] = AccReg,
      register(RegName, acceptor:start(RegName, AccId)),
      start_acceptors(Rest, RegNameRest)
  end.

start_proposers(PropIds, PropInfo, Acceptors, Sleep) ->
  case PropIds of
    [] ->
      ok;
    [PropId|Rest] ->
      [{RegName, Colour}|RestInfo] = PropInfo,
      [FirstSleep|RestSleep] = Sleep,
      proposer:start(RegName, Colour, Acceptors, FirstSleep, PropId),	
      start_proposers(Rest, RestInfo, Acceptors, RestSleep)
  end.

stop() ->
  stop(a),
  stop(b),
  stop(c),
  stop(d),
  stop(e),
  stop(gui).

stop(Name) ->
  case whereis(Name) of
    undefined ->
      ok;
    Pid ->
      Pid ! stop
  end.