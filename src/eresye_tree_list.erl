%
% eres_tree_list.erl
%
% ----------------------------------------------------------------------
%
%%
%%  ERESYE, an ERlang Expert SYstem Engine
%%
%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     * Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     * Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     * Neither the name of Francesca Gangemi, Corrado Santoro may be used
%%       to endorse or promote products derived from this software without
%%       specific prior written permission.
%%
%%
%% THIS SOFTWARE IS PROVIDED BY Francesca Gangemi AND Corrado Santoro ``AS
%% IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
%% THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
%% PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL <copyright holder> BE LIABLE FOR
%% ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
%% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
%% SUCH DAMAGE.
%
-module (eresye_tree_list).
-export ([new/0,
          insert/4,
          child/3,
          get_last_insert/1,
          get_root/1,
          lookup_all/2,
          children/2,
          update_node/2,
          refresh/2,
          is_root/1,
          get_beta/1,
          update_beta/3,
          get_key/1,
          update_key/3,
          have_child/1,
          get_parent/2,
          remove_node/2,
          is_present/2,
          keysearch/2,
          set_child/3,
          get_node/2,
          get_id/1,
          remove_child/3]).

new () ->
    [{root, nil, [], 0, 1}].


insert (Key, Value, Parent_node, List) ->
    {Parent_key, Parent_value, Children, Parent, Pos} = Parent_node,
    Next = length(List) + 1,
    Node = {Key, Value, [], Pos, Next},
    List1 = lists:append(List, [Node]),
    Children1 = lists:append(Children, [Next]),
    Parent_node1 = {Parent_key, Parent_value, Children1, Parent, Pos},
    L1 = lists:keyreplace(Pos, 5, List1, Parent_node1),
    {Node, L1}.

	

% cerca tra i figli del Parent_node un nodo la cui chiave (tupla) sia uguale a Key
% se Key non e' una tupla cerca il nodo il cui primo elemento e' {Key,_}
% Restituisce il nodo stesso  o false se non trova alcuna tupla

child (Key, Parent_node, List) ->
    Children = element(3, Parent_node),
    search (Key, List, Children).

search (Key, List, []) ->
    false;
search (Key, List, [Nth_element | T]) ->
    Elem = lists:nth(Nth_element, List),
    Res = search(Key, Elem),
    case Res of
	true ->
	    Elem;
	false ->
	    search (Key, List, T)
    end.

search ({p_node, Fun},{{p_node,{Fun,_}},_,_,_,_}) ->
    true;
search ({p_node, Fun,_},{{p_node,{Fun,_}},_,_,_,_})->
    true;
search ({Tab, Fun},{{Tab, Fun},_,_,_,_})->
    true;
search (Tab,{{Tab,_},_,_,_,_})->
    true;
search (K, E) ->
    false.


test (X, X) ->
    true;
test (X, Y) ->
    false.



get_root ([]) ->
    nil;
get_root ([Root | T]) ->
    case is_root(Root) of
	true ->
	    Root;
	false ->
	    nil
    end.


is_root ({root, _, _, _, _}) ->
    true;
is_root (Node) ->
    false.

% controlla se esiste un join_node legato all'alfa-memory Tab
% restituisce true se ne esiste almeno uno, altrimenti false
is_present (Tab, []) ->
    false;
is_present(Tab, [Node | T]) ->
    case catch  element(1, element(1, Node)) of % il primo elemento della lista
	{'EXIT', {badarg,_}} ->                 % genera un'eccezione
	    is_present(Tab, T);
	Tab_elem ->
	    case test(Tab_elem, Tab) of
		true ->
		    true;
		false ->
		    is_present(Tab, T)
	    end
    end.

% restituisce la lista di tutti gli elementi in List associati alla
% chiave Key. Se non esiste nessun elemento con la chiave richiesta
% il risultato e' una lista vuota
% Key = Tab
% Key = {Tab, Join_fun}
% Key = {p_node, Fun}
% Key = {p_node, {Fun, _}}
lookup_all (Key, List) ->
    [Root | L] = List,
    lists:foldl(fun(Elem, Result_list) ->
			%Tab_elem =  element(1, element(1, Elem)),
			case search(Key, Elem) of
			    true ->
				%lists:append(Result_list, [Elem]);
				[Elem | Result_list];
			    false ->
				Result_list
			end
		end, [], L).

% Cerca un elemento con chiave Key e restituisce il primo trovato o false
keysearch (Key, []) ->
    false;
keysearch (Key, [Node | OtherNode]) ->
    case search(Key, Node) of
	false ->
	    keysearch(Key, OtherNode);
	true ->
	    Node
    end.

% restituisce tutti i nodi figli di Join_node
% o una lista vuota se Join_node non ha successori
children (Join_node, List) ->
    Pos_list = element(3, Join_node),
    lists:foldl(fun(Pos, Children)->
			Elem = lists:nth(Pos, List),
			lists:append(Children, [Elem])
		end, [], Pos_list).


update_node (Join_node, List) ->
    Pos = element (5, Join_node),
    lists:keyreplace(Pos, 5, List, Join_node).

get_node (Id, List) ->
    lists:nth(Id, List).

get_last_insert (List) ->
    N = length (List),
    lists:nth (N, List).

get_id (Node) ->
    element(5, Node).

refresh (Node, List) ->
    Pos = element(5, Node),
    lists:nth(Pos, List).


get_beta (Node) ->
    element(2, Node).

update_beta (Beta_new, Node, List) ->
    {Key, Beta_old, Children, Parent, Pos} = Node,
    New_node = {Key, Beta_new, Children, Parent, Pos},
    lists:keyreplace(Pos, 5, List, New_node).


get_key (Node) ->
    element(1, Node).

update_key (NewKey, Node, List) ->
    {Key, Beta, Children, Parent, Pos} = Node,
    New_node = {NewKey, Beta, Children, Parent, Pos},
    lists:keyreplace(Pos, 5, List, New_node).


get_parent (Node, List) ->
    Parent = element (4, Node),
    lists:nth(Parent, List).

have_child (Node) ->
    Children = element(3, Node),
    case Children of
	[] ->
	    false;
	Other ->
	    true
    end.

remove_child (Child, Parent_Node, List) ->
    ChildId = element(5, Child),
    {Key, Value, Children, Parent, Pos} = Parent_Node,
    New_node = {Key, Value, Children -- [ChildId], Parent, Pos},
    lists:keyreplace(Pos, 5, List, New_node).


set_child (Child, Parent_node, List) ->
    {Key, Value, Children, Parent, Pos} = Parent_node,
    ChildId = element(5, Child),
    New_node = {Key, Value, Children ++ [ChildId], Parent, Pos},
    lists:keyreplace(Pos, 5, List, New_node).



remove_node (Node, List) ->
    Pos = element (5, Node),
    Parent_node = get_parent(Node, List),
    List1 = remove_child (Node, Parent_node, List),
    Head = lists:sublist(List1, Pos-1),
    Tail = lists:nthtail(Pos, List1),
    update_list (Head, Tail, Pos).

update_list (List, [], N) ->
    List;
update_list (List, [Node | T], N) ->
%    io:format("Node=~w~n",[Node]),
    New_pos = length(List)+1,
    {Key, Value, Children, Parent, Pos} = Node,
    case Parent > N of
	true ->
	    New_parent = Parent -1,
	    List1 = List;
	false ->
	    New_parent = Parent,
	    {Key1, Value1, Children1, Parent1, Pos1} = lists:nth(New_parent, List),
	    Parent_node = {Key1, Value1, (Children1 -- [Pos])++ [New_pos], Parent1, Pos1},
	%    io:format(">ParentNode=~w~n",[Parent_node]),
	    List1 = lists:keyreplace(Parent, 5, List, Parent_node)
    end,
    New_children = lists:map(fun(X)->
			       case X > N of
				   true -> X-1;
				   false ->X
			       end
		       end, Children),
    New_node = {Key, Value, New_children, New_parent, New_pos},
%    io:format("NewNode=~w~n",[New_node]),
    List2 = List1 ++ [New_node],
    update_list(List2, T, N).

