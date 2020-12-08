-module(test).
-import(network,[listen/1]).
-export([launch/7,healer_rand/0,healer_tail/0,swap_rand/0,swap_tail/0]).

% def : main function to lauch in erlang noshell
% parameters :
% H : integer (0-7)
% S : integer (0-7)
% C : max lenght of the view
% Peers : rand or tail for the time of launch
% Pushpull : boolean true = pushpull, false = push
% Time : integer, times between cycles in milliseconds
% N : number of nodes
launch(H,S,C,Peers,PushPull,Time,N) ->
  ListPid = spawn(network,listen,[[]]),
  ListPid ! {init,floor(N*0.4)},
  ListPid ! {launchNodes,C,H,S,PushPull,floor(Time/2),Peers},
  cycle(ListPid,1,Time,1),
  ListPid ! {init,floor(N*0.2)},
  ListPid ! {launchNodes,C,H,S,PushPull,floor(Time/2),Peers},
  cycle(ListPid,1,Time,31),
  ListPid ! {init,floor(N*0.2)},
  ListPid ! {launchNodes,C,H,S,PushPull,floor(Time/2),Peers},
  cycle(ListPid,1,Time,61),
  ListPid ! {init,floor(N*0.2)},
  ListPid ! {launchNodes,C,H,S,PushPull,floor(Time/2),Peers},
  cycle(ListPid,1,Time,91),
  Killed = floor(N*0.6),
  ListPid ! {kill,Killed},
  cycle(ListPid,1,Time,121),
  ListPid ! {recover,floor(Killed*0.6)},
  cycle(ListPid,1,Time,151).

% cycles launch
% parameters :
% ListPID : spawn the process nodes PID
% N : Integer
% Time : Integer in milliseconds
% Counter : Integer of the last cycle treated
cycle(ListPid,N,Time,Counter) ->
  if
    N =< 30 ->
      %io:format("Cycle ~p~n",[Counter]),
      ListPid ! {timer,Counter},
      timer:sleep(Time),
      cycle(ListPid,N+1,Time,Counter+1);
    true ->
      true
  end.

% healer with rand policy
healer_rand() ->
  launch(4,3,7,rand,true,3000,128).
% healer with tail policy
healer_tail() ->
  launch(4,3,7,tail,true,3000,128).
% swapper with rand policy
swap_rand() ->
  launch(3,4,7,rand,true,3000,128).
% swapper with tail policy
swap_tail() ->
  launch(3,4,7,tail,true,3000,128).
