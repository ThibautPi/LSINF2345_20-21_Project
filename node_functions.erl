- module (node_functions).
- export([permute/1,moveOldest/2,increaseAge/1,getNeigh/2,select/6]).


%Permute a list randomly
permute(View) ->
  shuffle(View).

shuffle(List) -> shuffle(List, []).

shuffle([], Acc) -> Acc;

shuffle(List, Acc) ->
{Leading, [H | T]} = lists:split(rand:uniform(length(List)) - 1, List),
shuffle(Leading ++ T, [H | Acc]).

%Move H oldest node to the end of a list
moveOldest([],_,Acc) ->
  Acc;

moveOldest(View, 0, Acc) ->
  lists:append(View,Acc);

moveOldest(View,H,Acc) ->
  Oldest = lists:max(View),
  moveOldest(lists:delete(Oldest,View),H-1,lists:append([Oldest],Acc)).

moveOldest(View,H) -> moveOldest(View,H,[]).

%Remove H oldest node of a list
remove_Oldest([],_) ->
  [];

remove_Oldest(View,0) ->
  View;

remove_Oldest(View,H) ->
  if H > 0 ->
      Oldest = lists:max(View),
      remove_Oldest(lists:delete(Oldest,View),H-1);
    true -> View
  end.

%remove S first element of a list
remove_Head(View,S) ->
  if S > 0 ->
      lists:nthtail(S,View);
    true -> View
  end.

%Remove N element in a list randomly
remove_Random([],_) ->
  [];
remove_Random(View,0) ->
  View;
remove_Random(View,N) ->
  RandomElement = lists:nth(rand:uniform(length(View)), View),
  remove_Random(lists:delete(RandomElement,View),N-1).

%Remove Node with the same PID in a list
remove_Dup(View) ->
  remove_Dup(View,View).

remove_Dup([],NewView) ->
  NewView;

remove_Dup([_],NewView) ->
  NewView;

%For each Element in the List, compare each of the items in the list to that item
%If a duplicate is found, update the list and do it with the next element
%Otherwise, just continue to progress in the list
remove_Dup([A|B],NewView) ->
  L = remove_Dup(B,A,NewView),
  if
    length(L) =:= length(NewView) ->
      remove_Dup(B,L);
    true ->
      remove_Dup(L,L)
  end.

remove_Dup([],_,Acc) ->
  Acc;

remove_Dup([A],C,Acc) ->
  X = lists:nth(2,A),
  Y = lists:nth(2,C),
  if
    X =:= Y ->
      lists:delete(A,Acc);
    true ->
      Acc
  end;

remove_Dup([A|B],C,Acc) ->
  X = lists:nth(2,A),
  Y = lists:nth(2,C),
  if
    X =:= Y ->
      remove_Dup(B,C,lists:delete(A,Acc));
    true ->
      remove_Dup(B,C,Acc)
  end.

%Remove items with a specific PID from a list
remove_himself([],_,View) ->
  View;

remove_himself([A],Pid,View) ->
  X = lists:nth(2,A),
  if
    X =:= Pid ->
      lists:delete(A,View);
    true ->
      View
  end;

remove_himself([A|B],Pid,View) ->
  X = lists:nth(2,A),
  if
    X =:= Pid ->
      remove_himself(B,Pid,lists:delete(A,View));
    true ->
      remove_himself(B,Pid,View)
  end.

%The select Function:
%append Buffer
%Remove duplicates
%Remove Oldest
%Remove Head
%Remove randomly
%Remove iteration of the node himself
select(C,H,S,BufferP,View,Pid) ->
  View_append = lists:append(View, BufferP),
  View_no_dup = remove_Dup(View_append),
  View_remove_old = remove_Oldest(View_no_dup,min(H,length(View_no_dup)-C)),
  View_remove_head = remove_Head(View_remove_old,min(S,length(View_remove_old)-C)),
  View_remove_random = remove_Random(View_remove_head,max(0,length(View_remove_head)-C)),
  remove_himself(View_remove_random,Pid,View_remove_random).

%Increase Age of element in a list
increaseAge(View) ->
    lists:map(fun([A,B,C]) -> [A+1,B,C] end, View).

%transform Element return by the network to obtain the desired structure: a list of [Age,Pid]
transform([],_,Acc) ->
  Acc;

transform([X],List,Acc) ->
  List ! {getPID,maps:get(id,X),self()},
  receive {pid,Pid} ->
    Neigh = [[0,Pid,maps:get(id,X)]],
    lists:append(Acc,Neigh)
  end;

transform([X|Y],List,Acc) ->
  List ! {getPID,maps:get(id,X),self()},
  receive {pid,Pid} ->
    Neigh = [[0,Pid,maps:get(id,X)]],
    transform(Y,List,lists:append(Acc,Neigh))
  end.

%Recover Neighbours of a Node in a list
getNeigh(List,Id) ->
  List ! {getNeigh,Id,self()},
  receive
    {neigh,Neighbors} ->
      transform(Neighbors,List,[])
  end.
