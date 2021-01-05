- module(node).
- export([init/8,passive/6,active/7,listen/0]).
- import(node_functions,[permute/1,moveOldest/2,select/6,increaseAge/1,getNeigh/2]).
- record(state, {id, pid, buffer, view}).

% First Listen Fonction, before Node are Initialized
listen() ->
  receive
    {init,Id,C,H,S,PushPull,T,PeerS,ListPid} ->
      init(Id,C,H,S,PushPull,T,PeerS,ListPid)
    end.

% Initilasiation with C,H,S,PushPull,Peers as arguments for the algorithm. List is the Server Pid and allows to retrieve Node's Neighbours
init(Id, C, H, S, PushPull,T, PeerS, List) ->
  State = #state{id = Id, pid = self(), buffer = [], view = getNeigh(List,Id)},
  ActivePid = spawn(node, active, [State,H,S,C,PushPull,T,PeerS]),
  PassivePid = spawn(node, passive, [State,H,S,C,PushPull,PeerS]),
  node_hub(ActivePid,PassivePid,true).

%The hub of the Node. Receive information from the servers/other_nodes and dispatch them depending on the action to be taken
node_hub(ActivePid,PassivePid,Alive) ->
  if
    %Alive check.If dead, wait a recover message.
    Alive ->
      receive
        {getView,From} ->
          ActivePid ! {getView,From};
        {kill} ->
          %io:format("node ~p killed! ~n",[self()]),
          node_hub(ActivePid,PassivePid,false);
        {recover,_,From} -> From ! {alive};
        {timer,Counter} ->
          ActivePid ! {timer,Counter},
          node_hub(ActivePid,PassivePid,Alive);
        {push,BufferP,P} -> PassivePid ! {push, BufferP,P};
        {pull,BufferP,P} -> ActivePid ! {pull,BufferP,P};
        %update View for passive or active to force them to have the same View.
        {update,NewView,passive} -> PassivePid ! {update,NewView};
        {update,NewView,active} -> ActivePid ! {update,NewView}
      end,
      node_hub(ActivePid,PassivePid,Alive);
    true ->
      receive
        {getView,From} ->
          From ! {ko},
          node_hub(ActivePid,PassivePid,Alive);
        {recover,NewView,From} ->
          %io:format("node ~p recover! ~n",[self()]),
          PassivePid ! {update,NewView},
          ActivePid ! {update,NewView},
          From ! {ok},
          node_hub(ActivePid,PassivePid,true);
        {_} ->
          node_hub(ActivePid,PassivePid,Alive);
        {_,_} ->
            node_hub(ActivePid,PassivePid,Alive);
        {_,_,_} ->
            node_hub(ActivePid,PassivePid,Alive)
      end
  end.

%Passive part of the node, waiting a push to update his View
passive(State,H,S,C,PushPull,PeerS) ->
  receive
    {update,NewView} ->
      NewState = #state{id= State#state.id, pid = State#state.pid, buffer = [], view = NewView},
      passive(NewState,H,S,C,PushPull,PeerS);
    %classic algortihm progress
    {push, BufferP, P} ->
      if
        PushPull ->
          PermutedView = permute(State#state.view),
          View = moveOldest(PermutedView,H),
          Buffer = lists:append([[0,State#state.pid,State#state.id]], lists:sublist(View, round(abs(C/2-1))+1)),
          P ! {pull, Buffer, self()}
      end,
    View_select = select(C,H,S,BufferP,State#state.view,State#state.pid),
    NewView = increaseAge(View_select),
    NewState = #state{id = State#state.id, pid = State#state.pid, buffer = Buffer, view = NewView},
    State#state.pid ! {update,NewView,active},
    passive(NewState,H,S,C,PushPull,PeerS)
  end.

%active part of the node
active(State,H,S,C,PushPull,T,PeerS) ->
  receive
    %return View, useful in case of a recover try (we need a view of an alive Node)
    {getView, From} ->
      From ! {ok,State#state.view},
      active(State,H,S,C,PushPull,T,PeerS);
    {update,NewView} ->
      NewState = #state{id= State#state.id, pid = State#state.pid, buffer = [], view = NewView},
      active(NewState,H,S,C,PushPull,T,PeerS);
    {timer,Counter} ->
      ToPrint  = lists:map(fun([_,_,B]) -> B end,State#state.view),
      io:format("log:: ~p ~p ~w ~n", [Counter,State#state.id, ToPrint ]),
      if
        %A node can be alive but with an empty view. It will then be isolated and will not be able to send any information
        State#state.view =/= [] ->
          if
            PeerS =:= tail ->
              Peer = lists:last(State#state.view);
            PeerS =:= rand ->
              Peer = lists:nth(rand:uniform(length(State#state.view)), State#state.view)
          end,
          PermutedView = permute(State#state.view),
          View = moveOldest(PermutedView,H),
          Buffer = lists:append([[0,State#state.pid,State#state.id]],lists:sublist(View, round(abs(C/2-1))+1)),
          lists:nth(2,Peer) ! {push,Buffer,self()},
          if
            PushPull ->
              receive
                {pull, BufferP, _} ->
                  View_select = select(C,H,S,BufferP,State#state.view,State#state.pid),
                  NewView = increaseAge(View_select),
                  NewState = #state{id = State#state.id,pid= State#state.pid, buffer = Buffer, view = NewView}
              after T ->
                  %if Timeout, the target node is considered dead and therefore removed from the view
                  NewView = increaseAge(lists:delete(Peer,State#state.view)),
                  NewState = #state{id = State#state.id,pid= State#state.pid, buffer = Buffer, view = NewView}
              end;
            true ->
              NewView = increaseAge(State#state.view),
              NewState = #state{id= State#state.id, pid = State#state.pid, buffer = Buffer, view = NewView}
          end,
          State#state.pid ! {update,NewView, passive},
          active(NewState,H,S,C,PushPull,T,PeerS);
        true ->
          active(State,H,S,C,PushPull,T,PeerS)
    end
  end.
