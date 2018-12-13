-module(paxy).
-export([start_proposers/2, start_acceptors/0, stop/0, stop/1]). 

-define(RED, {255,0,0}). 
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).


% Entry point of the application. Sleep is a list with the initial sleep time for each proposer
start_proposers(Sleep, AcceptorsNode) ->
    %Node='paxy-acc@cod009',
    AccRegister = [{a, AcceptorsNode}, {b, AcceptorsNode},{c, AcceptorsNode},{d, AcceptorsNode},{e, AcceptorsNode}],
    PropInfo = [{kurtz, ?RED}, {kilgore, ?GREEN}, {willard, ?BLUE}],
    {gui, AcceptorsNode} ! {reqState, self()}, 
    receive
        {reqState, State} ->
            {_, PropIds} = State,
            start_proposers(PropIds, PropInfo, AccRegister, Sleep)
    end,
    true.

start_acceptors() ->
    AcceptorNames = ["Acceptor a", "Acceptor b", "Acceptor c", "Acceptor d", "Acceptor e"],
    AccRegister = [a, b, c, d, e],
    ProposerNames = [{"Proposer kurtz", ?RED}, {"Proposer kilgore", ?GREEN}, {"Proposer willard", ?BLUE}],
    register(gui, spawn(fun() -> gui:start(AcceptorNames, ProposerNames) end)), % create new gui process
    gui ! {reqState, self()}, % send reference of self (own PID) to GUI
    receive
        {reqState, State} ->
            {AccIds, _} = State,
            start_acceptors(AccIds, AccRegister)
    end,
    true.


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
            pers:delete(Name),
            Pid ! stop
    end.