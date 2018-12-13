
-module(acceptor).
-export([start/2]).
-define(delay, 200).
-define(drop, 3).

start(Name, PanelId) ->
  spawn(fun() -> init(Name, PanelId) end).
        
  init(Name, PanelId) ->
  pers:open(Name),
  {Promised,Voted,Value,PanelIdR}=pers:read(Name),
  pers:close(Name),
  if PanelIdR==na ->
      acceptor(Name, Promised, Voted, Value, PanelId);
    true ->
      acceptor(Name, Promised, Voted, Value, PanelIdR)
  end.
  
acceptor(Name, Promised, Voted, Value, PanelId) ->
    R11 = rand:uniform(?delay),
    timer:sleep(R11),
  receive
    {prepare, Proposer, Round} ->
      R1 = rand:uniform(?delay),
      timer:sleep(R1),
      P1 = rand:uniform(10),
      if P1 =< ?drop ->
          io:format("promise message from proposer dropped~n"),
          acceptor(Name, Promised, Voted, Value, PanelId);
        true ->
          case order:gr(Round, Promised) of
            true ->
            P2 = rand:uniform(10),
              
              pers:open(Name),
              pers:store(Name,Round,Voted,Value,PanelId),
              pers:close(Name),              
              io:format("[Acceptor ~w] Phase 1: promised ~w voted ~w colour ~w~n",
                     [Name, Round, Voted, Value]),
              % Update gui
              Colour = case Value of na -> {0,0,0}; _ -> Value end,
              PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Voted]), 
                         "Promised: " ++ io_lib:format("~p", [Round]), Colour},
              if P2 =< ?drop ->
                io:format("promise message from acceptor dropped~n"),
                paxy:crash(Name),
                 acceptor(Name, Round, Voted, Value, PanelId);
                true ->
                  Proposer ! {promise, Round, Voted, Value}, 
                  acceptor(Name, Round, Voted, Value, PanelId)
              end;                                        
            false ->
             % Proposer ! {sorry, {prepare, Round}},
              acceptor(Name, Promised, Voted, Value, PanelId)
          end
      end;
    {accept, Proposer, Round, Proposal} ->
      R2 = rand:uniform(?delay),
      timer:sleep(R2),
      P3 = rand:uniform(10),
      if P3 =< ?drop ->
          io:format("accept message from proposer dropped~n"),
          acceptor(Name, Promised, Voted, Value, PanelId);
      true ->
        case order:goe(Round, Promised) of
          true ->
            P4 = rand:uniform(10),
            if P4 =< ?drop ->
              io:format("accept message from acceptor dropped~n");
            true ->
              Proposer ! {vote, Round}
            end,
            case order:goe(Round, Voted) of
              true ->
              pers:open(Name),
              pers:store(Name,Promised,Round,Proposal,PanelId),
              pers:close(Name),   
        io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n",
                   [Name, Promised, Round, Proposal]),
                % Update gui
                PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Round]), 
                           "Promised: " ++ io_lib:format("~p", [Promised]), Proposal},
                acceptor(Name, Promised, Round, Proposal, PanelId);
              false ->
                acceptor(Name, Promised, Voted, Value, PanelId)
            end;                            
          false ->
            %Proposer ! {sorry, {accept, Round}},
            acceptor(Name, Promised, Voted, Value, PanelId)
        end
      end;
    stop ->
      PanelId ! stop,
      ok
  end.
  