%%%  ERESYE, an ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
-module(eresye_tree_list).

-export([child/3, children/2, get_beta/1, get_id/1,
         get_key/1, get_last_insert/1, get_node/2, get_parent/2,
         get_root/1, have_child/1, insert/4, is_present/2,
         is_root/1, keysearch/2, lookup_all/2, new/0, refresh/2,
         remove_child/3, remove_node/2, set_child/3,
         update_beta/3, update_key/3, update_node/2]).

new() -> [{root, nil, [], 0, 1}].

insert(Key, Value, Parent_node, List) ->
    {Parent_key, Parent_value, Children, Parent, Pos} =
        Parent_node,
    Next = length(List) + 1,
    Node = {Key, Value, [], Pos, Next},
    List1 = List ++ [Node],
    Children1 = Children ++ [Next],
    Parent_node1 = {Parent_key, Parent_value, Children1,
                    Parent, Pos},
    L1 = lists:keyreplace(Pos, 5, List1, Parent_node1),
    {Node, L1}.

%% @doc Parent_node search for children of a node whose key (tuple) is equal to Key
%% if Key is not 'look for the node a tuple whose first element' {Key, _}
%% Returns the node itself or false if there is no tuple
child(Key, Parent_node, List) ->
    Children = element(3, Parent_node),
    search(Key, List, Children).

search(_Key, _List, []) -> false;
search(Key, List, [Nth_element | T]) ->
    Elem = lists:nth(Nth_element, List),
    Res = search(Key, Elem),
    case Res of
        true -> Elem;
        false -> search(Key, List, T)
    end.

search({p_node, Fun},
       {{p_node, {Fun, _}}, _, _, _, _}) ->
    true;
search({p_node, Fun, _},
       {{p_node, {Fun, _}}, _, _, _, _}) ->
    true;
search({Tab, Fun}, {{Tab, Fun}, _, _, _, _}) -> true;
search(Tab, {{Tab, _}, _, _, _, _}) -> true;
search(_K, _E) -> false.

test(X, X) -> true;
test(_X, _Y) -> false.

get_root([]) -> nil;
get_root([Root | _T]) ->
    case is_root(Root) of
        true -> Root;
        false -> nil
    end.

is_root({root, _, _, _, _}) -> true;
is_root(_Node) -> false.

%% @doc Check if there is an alpha-memory join_node
%% true if there is at least one, otherwise false
is_present(_Tab, []) -> false;
is_present(Tab, [Node | T]) ->
    case catch element(1,
                       element(1, Node)) % The first element of the list
    of
        {'EXIT',
         {badarg, _}} ->                 % throws an exception
            is_present(Tab, T);
        Tab_elem ->
            case test(Tab_elem, Tab) of
                true -> true;
                false -> is_present(Tab, T)
            end
    end.

%% @doc returns a list of all the elements associated with the List
%% Key Key. If there is no element with the requested key
%% The result is' an empty list  = Tab Key
%% Key =% {Tab} Join_fun
%% Key =% {p_node, Fun}
%% Key =% {p_node, Fun {, _}}
lookup_all(Key, [_Root | L]) ->
    lists:foldl(fun (Elem, Result_list) ->
                        case search(Key, Elem) of
                            true ->
                                [Elem | Result_list];
                            false -> Result_list
                        end
                end,
                [], L).

%% @doc Search for an item with key Key and returns the first found or false
keysearch(_Key, []) -> false;
keysearch(Key, [Node | OtherNode]) ->
    case search(Key, Node) of
        false -> keysearch(Key, OtherNode);
        true -> Node
    end.

%% @doc returns all child nodes of Join_node
%% Or an empty list if no successors Join_node
children(Join_node, List) ->
    Pos_list = element(3, Join_node),
    lists:foldl(fun (Pos, Children) ->
                        Elem = lists:nth(Pos, List), Children ++ [Elem]
                end,
                [], Pos_list).

update_node(Join_node, List) ->
    Pos = element(5, Join_node),
    lists:keyreplace(Pos, 5, List, Join_node).

get_node(Id, List) -> lists:nth(Id, List).

get_last_insert(List) ->
    N = length(List), lists:nth(N, List).

get_id(Node) -> element(5, Node).

refresh(Node, List) ->
    Pos = element(5, Node), lists:nth(Pos, List).

get_beta(Node) -> element(2, Node).

update_beta(Beta_new, {Key, _Beta_old, Children, Parent, Pos}, List) ->
    New_node = {Key, Beta_new, Children, Parent, Pos},
    lists:keyreplace(Pos, 5, List, New_node).

get_key(Node) -> element(1, Node).

update_key(NewKey, {_Key, Beta, Children, Parent, Pos}, List) ->
    New_node = {NewKey, Beta, Children, Parent, Pos},
    lists:keyreplace(Pos, 5, List, New_node).

get_parent(Node, List) ->
    Parent = element(4, Node), lists:nth(Parent, List).

have_child(Node) ->
    Children = element(3, Node),
    case Children of
        [] -> false;
        _Other -> true
    end.

remove_child(Child, Parent_Node, List) ->
    ChildId = element(5, Child),
    {Key, Value, Children, Parent, Pos} = Parent_Node,
    New_node = {Key, Value, Children -- [ChildId], Parent,
                Pos},
    lists:keyreplace(Pos, 5, List, New_node).

set_child(Child, Parent_node, List) ->
    {Key, Value, Children, Parent, Pos} = Parent_node,
    ChildId = element(5, Child),
    New_node = {Key, Value, Children ++ [ChildId], Parent,
                Pos},
    lists:keyreplace(Pos, 5, List, New_node).

remove_node(Node, List) ->
    Pos = element(5, Node),
    Parent_node = get_parent(Node, List),
    List1 = remove_child(Node, Parent_node, List),
    Head = lists:sublist(List1, Pos - 1),
    Tail = lists:nthtail(Pos, List1),
    update_list(Head, Tail, Pos).

update_list(List, [], _N) -> List;
update_list(List, [Node | T], N) ->
    New_pos = length(List) + 1,
    {Key, Value, Children, Parent, Pos} = Node,
    {New_parent, List1} =
        case Parent > N of
            true ->
                {Parent - 1, List};
            false ->
                New_parent0 = Parent,
                {Key1, Value1, Children1, Parent1, Pos1} =
                    lists:nth(New_parent0, List),
                Parent_node = {Key1, Value1,
                               (Children1 -- [Pos]) ++ [New_pos], Parent1, Pos1},
                {New_parent0, lists:keyreplace(Parent, 5, List, Parent_node)}
        end,
    New_children = [update_list_1(V1, N) || V1 <- Children],
    New_node = {Key, Value, New_children, New_parent,
                New_pos},
    List2 = List1 ++ [New_node],
    update_list(List2, T, N).

update_list_1(X, N) ->
    case X > N of
        true -> X - 1;
        false -> X
    end.
