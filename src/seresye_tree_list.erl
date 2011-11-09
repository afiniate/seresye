%%%  SERESYE, an Swarm oriented ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%%% Copyright (c) 2011 Afiniate, Inc.
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
-module(seresye_tree_list).

-export([child/3, children/2, get_beta/1, get_id/1,
         get_key/1, get_last_insert/1, get_node/2, get_parent/2,
         get_root/1, have_child/1, insert/4, is_present/2,
         is_root/1, keysearch/2, lookup_all/2, new/0, refresh/2,
         remove_child/3, remove_node/2, set_child/3,
         update_beta/3, update_key/3, update_node/2]).

new() -> [{root, nil, [], 0, 1}].

insert(Key, Value, ParentNode, List) ->
    {ParentKey, ParentValue, Children, Parent, Pos} =
        ParentNode,
    Next = length(List) + 1,
    Node = {Key, Value, [], Pos, Next},
    List1 = List ++ [Node],
    Children1 = Children ++ [Next],
    ParentNode1 = {ParentKey, ParentValue, Children1,
                    Parent, Pos},
    L1 = lists:keyreplace(Pos, 5, List1, ParentNode1),
    {Node, L1}.

%% @doc Parent_node search for children of a node whose key (tuple) is equal to Key
%% if Key is not 'look for the node a tuple whose first element' {Key, _}
%% Returns the node itself or false if there is no tuple
child(Key, ParentNode, List) ->
    Children = element(3, ParentNode),
    search(Key, List, Children).

search(_Key, _List, []) -> false;
search(Key, List, [NthElement | T]) ->
    Elem = lists:nth(NthElement, List),
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
        TabElem ->
            case test(TabElem, Tab) of
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
    lists:foldl(fun (Elem, ResultList) ->
                        case search(Key, Elem) of
                            true ->
                                [Elem | ResultList];
                            false -> ResultList
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

update_node(JoinNode, List) ->
    Pos = element(5, JoinNode),
    lists:keyreplace(Pos, 5, List, JoinNode).

get_node(Id, List) -> lists:nth(Id, List).

get_last_insert(List) ->
    N = length(List), lists:nth(N, List).

get_id(Node) -> element(5, Node).

refresh(Node, List) ->
    Pos = element(5, Node), lists:nth(Pos, List).

get_beta(Node) -> element(2, Node).

update_beta(BetaNew, {Key, _BetaOld, Children, Parent, Pos}, List) ->
    NewNode = {Key, BetaNew, Children, Parent, Pos},
    lists:keyreplace(Pos, 5, List, NewNode).

get_key(Node) -> element(1, Node).

update_key(NewKey, {_Key, Beta, Children, Parent, Pos}, List) ->
    NewNode = {NewKey, Beta, Children, Parent, Pos},
    lists:keyreplace(Pos, 5, List, NewNode).

get_parent(Node, List) ->
    Parent = element(4, Node), lists:nth(Parent, List).

have_child(Node) ->
    Children = element(3, Node),
    case Children of
        [] -> false;
        _Other -> true
    end.

remove_child(Child, ParentNode, List) ->
    ChildId = element(5, Child),
    {Key, Value, Children, Parent, Pos} = ParentNode,
    NewNode = {Key, Value, Children -- [ChildId], Parent,
                Pos},
    lists:keyreplace(Pos, 5, List, NewNode).

set_child(Child, ParentNode, List) ->
    {Key, Value, Children, Parent, Pos} = ParentNode,
    ChildId = element(5, Child),
    NewNode = {Key, Value, Children ++ [ChildId], Parent,
                Pos},
    lists:keyreplace(Pos, 5, List, NewNode).

remove_node(Node, List) ->
    Pos = element(5, Node),
    ParentNode = get_parent(Node, List),
    List1 = remove_child(Node, ParentNode, List),
    Head = lists:sublist(List1, Pos - 1),
    Tail = lists:nthtail(Pos, List1),
    update_list(Head, Tail, Pos).

update_list(List, [], _N) -> List;
update_list(List, [Node | T], N) ->
    NewPos = length(List) + 1,
    {Key, Value, Children, Parent, Pos} = Node,
    {NewParent, List1} =
        case Parent > N of
            true ->
                {Parent - 1, List};
            false ->
                NewParent0 = Parent,
                {Key1, Value1, Children1, Parent1, Pos1} =
                    lists:nth(NewParent0, List),
                ParentNode = {Key1, Value1,
                              (Children1 -- [Pos]) ++ [NewPos], Parent1, Pos1},
                {NewParent0, lists:keyreplace(Parent, 5, List, ParentNode)}
        end,
    NewChildren = [update_list_1(V1, N) || V1 <- Children],
    NewNode = {Key, Value, NewChildren, NewParent,
               NewPos},
    List2 = List1 ++ [NewNode],
    update_list(List2, T, N).

update_list_1(X, N) ->
    case X > N of
        true -> X - 1;
        false -> X
    end.
