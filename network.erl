-module(network).
-export([test_getN/0,test_getPID/0,test_network/0,listen/1,test_circular/0]).
-import(node,[listen/0]).

% Add function for the first node with the max ID in the Double-linked list
% parameters : ID and PID of the new nodes started and empty list (network)
add(ID,PID,[])->
  [#{id => ID,pid => PID, linked_node_list => []}];
% Add function for the other nodes when the network has allready 1 node inside
add(ID,PID,[ #{id := ID_before,pid := PID_before , linked_node_list := List_before} |T ])->
  % append the new node in the Double-linked list in first position and append the id of the new nodes
  % in the Neighbors list of the node who was first in the list before before insertion
  [#{id =>ID, pid => PID , linked_node_list => [#{id=>ID_before}]},
  #{id => ID_before,pid => PID_before ,linked_node_list => lists:append([#{id => ID}],List_before)} |T].

% function who sort and make the Double-linked list circular => first and last element links
% need filter list with le ID_max in first position and id = 1 in last with lists:reverse(lists:sort(LIST))
make_circular2([#{id := 1, linked_node_list := List, pid := PID}|T])->
  lists:reverse([ #{id => 1 , linked_node_list => lists:append([#{id =>(length(T)+1)}],List),  pid => PID} | T]).

make_circular([ #{id := ID_max , linked_node_list := List, pid := PID} |T])->
  make_circular2(lists:reverse([ #{id => ID_max , linked_node_list => lists:append([#{id => 1}],List),pid => PID} | T])).

% function who sort and undo the cycles on the Double-linked list => first and last element not links
% parameters : List which is the Double-linked list
% need filter list with le ID_max in first position and id = 1 in last with lists:reverse(lists:sort(LIST))
unmake_circular2([#{id := 1,pid := PID}|T])->
  lists:reverse([ #{id => 1, linked_node_list =>[#{id => 2}],pid => PID} | T]).

unmake_circular([]) ->
   [];

unmake_circular([#{id := ID_max,pid := PID}|T])->
  unmake_circular2(lists:reverse([ #{id => ID_max, linked_node_list =>[#{id => (ID_max-1)}],pid =>PID} | T])).

% network function who create the Double-linked list by adding recursively node by node and start process with spawn
% parameters : int for accumulator for the recursion, list which is the Double-linked list
network_list(0,List_netw)->List_netw;

network_list(Node,[]) ->
  network_list(Node,[],0);
network_list(Node,List)->
  network_list(Node, List, length(List)).

network_list(0,List,_) ->
  List;
% recursion function who append itself and make the function add in each iteration
% add a node with IDmax+1 as ID
network_list(Node,List,N) ->
  NodePid = spawn(node,listen,[]),
  network_list(Node-1, add(1+N,NodePid,List),N+1).

%Return the PID corresponding to the node
getPID(_,[])-> "Error, give empty list";
getPID(ID, [#{id := ID,pid := PID}|_])-> PID;
getPID(ID, [_|T])-> getPID(ID, T).


%Return Neighbors of an Element in the LinkedList
getNeighbors(_,[])-> "Error, the node is not in the list";
getNeighbors(ID, [#{id := ID,linked_node_list:= Neighbors}|_])-> Neighbors;
getNeighbors(ID, [_|T])-> getNeighbors(ID, T).

%Initialize the Nodes
launchNodes([A],C,H,S,PushPull,T,PeerS) ->
  maps:get(pid,A) ! {init,maps:get(id,A),C,H,S,PushPull,T,PeerS,self()};

launchNodes([A|B],C,H,S,PushPull,T,PeerS) ->
  maps:get(pid,A) ! {init,maps:get(id,A),C,H,S,PushPull,T,PeerS,self()},
  launchNodes(B,C,H,S,PushPull,T,PeerS).

%Notify the nodes to communicate
time([A],Counter) ->
  maps:get(pid,A) ! {timer,Counter};

time([A|B],Counter) ->
  maps:get(pid,A) ! {timer,Counter},
  time(B,Counter).

%Kill N nodes in the Network
kill_N_nodes(0,_) -> true;
kill_N_nodes(_,[]) -> true;
kill_N_nodes(N,List) ->
  Node = lists:nth(rand:uniform(length(List)),List),
  maps:get(pid,Node) ! {kill},
  kill_N_nodes(N-1,lists:delete(Node,List)).

%Recover N nodes in the Network
alive(_,_,[]) ->
  true;

alive(0,_,_) ->
  true;

alive(N,View,List) ->
  Node = lists:nth(1,List),
  Pid = maps:get(pid,Node),
  Pid ! {recover,View,self()},
  receive
    {alive} ->
      alive(N,View,lists:delete(Node,List));
    {ok} ->
      alive(N-1,View,lists:delete(Node,List))
  end.

%Found a Node alive in the Network to send its View to dead Nodes
choose_peer_recover(_,[],_) ->
  %io:format("No Alive Left");
  true;

choose_peer_recover(N,List,Full_List) ->
  Node = lists:nth(rand:uniform(length(List)),List),
  Pid = maps:get(pid,Node),
  Pid ! {getView,self()},
  receive
    {ko} ->
      choose_peer_recover(N,lists:delete(Node,List),Full_List);
    {ok,View} ->
      alive(N,View,Full_List)
  end.


%Network Server HUB
listen(LinkedList) ->
  receive
    {init,N} ->
      L = network_list(N,unmake_circular(lists:reverse(lists:sort(LinkedList)))),
      H = make_circular(lists:reverse(lists:sort(L))),
      %io:format("linkedlist ~p ~n", [H]),
      listen(H);
    {getNeigh,Id, From} ->
      Neighbors = getNeighbors(Id,LinkedList),
      From ! {neigh,Neighbors};
    {launchNodes,C,H,S,PushPull,T,PeerS} ->
      launchNodes(LinkedList,C,H,S,PushPull,T,PeerS);
    {getPID,Id,From} ->
      From ! {pid,getPID(Id,LinkedList)};
    {timer,Counter} ->
      time(LinkedList,Counter);
    {kill,N} ->
      kill_N_nodes(N,LinkedList);
    {recover,N} ->
       choose_peer_recover(N,LinkedList,LinkedList)
  end,
  listen(LinkedList).


%----------------- TEST -------------------------------------------------------
test_getN() ->
  List = network_list(5,[]),
  [getNeighbors(1,List),getNeighbors(4,List)]
  .

test_getPID() ->
  List = network_list(5,[]),
  [getPID(1,List),getPID(4,List)]
  .

test_network() ->
  network_list(5,[])
  .

test_circular() ->
  L = make_circular(lists:reverse(lists:sort(network_list(3,[])))),
  H = unmake_circular(lists:reverse(lists:sort(L))),
  L2 = network_list(6,H),
  L3 = make_circular(lists:reverse(lists:sort(L2))),
  io:format("LIST 1 ~p~n", [L]),
  io:format("LIST 2 ~p~n", [H]),
  io:format("LIST 3 ~p~n", [L2]),
  io:format("LIST 4 ~p~n", [L3]).
