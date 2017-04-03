%%%  SERESYE, a Swarm oriented ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%%% Copyright (c) 2011 Afiniate, Inc.
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
-module(seresye_engine).

-ifdef(debug).
-define(LOG(F, X), io:format(F, X)).
-else.
-define(LOG(F, X), true).
-endif.

%%====================================================================
%% Include files
%%====================================================================
-include("internal.hrl").

%%====================================================================
%% External exports
%%====================================================================

-export([new/0, new/1, serialize/1, restore/1, cleanup/1,
         set_hooks/2, get_fired_rule/1,
         add_rules/2, add_rule/2, add_rule/3, assert/2, get_kb/1,
         get_rules_fired/1, get_client_state/1, set_client_state/2,
         query_kb/2, remove_rule/2, retract/2]).

%%====================================================================
%% External functions
%%====================================================================
new() ->
    new([]).

new(ClientState) ->
    seresye_agenda:new(#seresye{kb=[], alfa=[],
                              join=seresye_tree_list:new(),
                              pending_actions=[],
                              client_state=ClientState}).

set_hooks(EngineState, Hooks) when is_list(Hooks) ->
    EngineState#seresye{ hooks = Hooks }.

set_client_state(EngineState, NewState) ->
    EngineState#seresye{client_state=NewState}.

get_client_state(#seresye{client_state=State}) ->
    State.

cleanup(#seresye{ alfa = Alfa0, join = Join0 }) ->
    [ begin (catch ets:delete(Tab)), Tab end || {_, Tab, _} <- Alfa0 ] ++
    [ begin (catch ets:delete(Tab)), Tab end || {{Tab, _}, _, _, _, _} <- Join0, is_integer(Tab) ].         

restore(#seresye{ alfa = Alfa0, join = Join0 } = Engine) ->
    TabCache = ets:new(tab_cache, []),
    Alfa = [ {Cond, restore_tab(TabCache, Tab), Alfa_fun}
             || {Cond, Tab, Alfa_fun} <- Alfa0 ],
    Join = [ {case Key of
                  {Tab,V} when element(1,Tab) == ets_table ->
                      {restore_tab(TabCache, Tab), V};
                  _ ->
                      Key
              end, Beta, Children, Parent, Pos} ||
               {Key, Beta, Children, Parent, Pos} <- Join0 ],
    ets:delete(TabCache),
    Engine#seresye{ alfa = Alfa, join = Join }.

serialize(#seresye{ alfa = Alfa0, join = Join0 } = Engine) ->
    Alfa = [ {Cond, serialize_tab(Tab), Alfa_fun} 
             || {Cond, Tab, Alfa_fun} <- Alfa0 ],
    Join = [ {case Key of
                  {Tab,V} when is_integer(Tab) ->
                      {serialize_tab(Tab), V};
                  _ ->
                      Key
              end, Beta, Children, Parent, Pos} ||
               {Key, Beta, Children, Parent, Pos} <- Join0 ],
    Engine#seresye{ alfa = Alfa, join = Join }.
                 
% where
serialize_tab(Tab) ->
    case ets:info(Tab) of
        undefined ->
            Tab;
        Info ->
            {ets_table, Tab, Info, ets:tab2list(Tab)}
    end.
restore_tab(Cache, {ets_table, Tab, Info, Content}) ->
    case ets:lookup(Cache, Tab) of
        [{Tab, NewTab}] ->
            NewTab;
        [] ->
            NewTab = ets:new(proplists:get_value(name, Info),
                             [ proplists:get_value(Opt, Info) ||
                                 Opt <- [type,protection] ] ++
                             [ {Opt, proplists:get_value(Opt, Info)} || 
                                 Opt <- [keypos] ]),
            ets:insert(NewTab, Content),
            ets:insert(Cache, {Tab, NewTab}),
            NewTab
    end;
restore_tab(_Cache, Tab) ->
    Tab.


%% @doc Insert a fact in the KB.
%% It also checks if the fact verifies any condition,
%% if this is the case the fact is also inserted in the alpha-memory
assert(EngineState0, Facts) when is_list(Facts) ->
    lists:foldl(fun(Fact, EngineState1) ->
                        assert(EngineState1, Fact)
                end, EngineState0, Facts);
assert(EngineState = #seresye{kb=Kb, alfa=Alfa}, Fact) when is_tuple(Fact) ->
    execute_pending(case lists:member(Fact, Kb) of
                        false ->
                            Kb1 = [Fact | Kb],
                            check_cond(EngineState#seresye{kb=Kb1}, Alfa,
                                       {Fact, plus});
                        true -> EngineState
                    end).

%% @doc removes a 'fact' in the Knowledge Base and if something occurs
%% Condition is also deleted from the corresponding alpha-memory
retract(EngineState0, Facts) when is_list(Facts) ->
    lists:foldl(fun(Fact, EngineState1) ->
                        retract(EngineState1, Fact)
                end, EngineState0, Facts);
retract(EngineState = #seresye{kb=Kb, alfa=Alfa}, Fact) when is_tuple(Fact) ->
    execute_pending(case lists:member(Fact, Kb) of
                        true ->
                            Kb1 = Kb -- [Fact],
                            check_cond(EngineState#seresye{kb=Kb1}, Alfa,
                                       {Fact, minus});
                        false -> EngineState
                    end).


add_rules(EngineState0, RulesList) when is_list(RulesList) ->
    lists:foldl(fun(Rule, EngineState1) ->
                        add_rule(EngineState1, Rule)
                end, EngineState0, RulesList);
add_rules(EngineState0, Module) when is_atom(Module) ->
    AST = get_abstract_code(Module),
    case get_rules(Module, AST) of
        [] ->
            erlang:throw({seresye, {no_rules_specified, Module}});
        RulesList ->
            lists:foldl(fun(Rule, EngineState1) ->
                                add_rule(EngineState1, Rule)
                        end, EngineState0, RulesList)
    end.

add_rule(EngineState, {Module, Fun, ClauseID, Salience}) ->
    add_rule(EngineState, {Module, Fun}, ClauseID, Salience);
add_rule(EngineState, Fun) ->
    add_rule(EngineState, Fun, 0).

add_rule(EngineState, {Module, Fun, ClauseID}, Salience) ->
    add_rule(EngineState, {Module, Fun}, ClauseID, Salience);
add_rule(EngineState, {Module, Fun}, Salience) ->
    add_rule(EngineState, {Module, Fun}, 0, Salience).

add_rule(EngineState0, {Module, Fun}, ClauseID, Salience) ->
    AST = get_abstract_code(Module),
    case get_conds(Fun, ClauseID, AST) of
        error -> erlang:throw({seresye, {error_extracting_conditions, Fun}});
        CondsList ->
            execute_pending(
              lists:foldl(fun (X, EngineState1) ->
                                  case X of
                                      {error, Msg} ->
                                          erlang:throw({seresye, {error_adding_rule,
                                                         [Module, Fun, Msg]}});
                                      {PConds, NConds} ->
                                          ?LOG(">> PConds=~p~n", [PConds]),
                                          ?LOG(">> NConds=~p~n", [NConds]),
                                          add_rule__(EngineState1,
                                                     {{Module, Fun}, Salience},
                                                     {PConds, NConds})
                                  end
                          end,
                          EngineState0,
                          CondsList))
    end.

remove_rule(EngineState0, Rule) ->
    execute_pending(remove_prod(seresye_agenda:delete_rule(EngineState0, Rule), Rule)).

get_fired_rule(#seresye{ fired_rule = Rule }) ->
    Rule.

get_rules_fired(EngineState) ->
    seresye_agenda:get_rules_fired(EngineState).

get_kb(#seresye{kb=Kb}) ->
    Kb.

query_kb(EngineState, Pattern) when is_tuple(Pattern) ->
    PList = tuple_to_list(Pattern),
    PatternSize = length(PList),
    FunList = [if is_function(X) -> X;
                  X == '_' -> fun (_) -> true end;
                  true -> fun (Z) -> Z == X end
               end
               || X <- PList],
    MatchFunction = fun (P) ->
                            FunPatPairs = lists:zip(FunList, tuple_to_list(P)),
                            FunEval = [F(X) || {F, X} <- FunPatPairs],
                            MatchResult = lists:foldr(fun (X, Y) -> X and Y end,
                                                      true, FunEval),
                            MatchResult
                    end,
    KB = [X
          || X <- get_kb(EngineState), size(X) == PatternSize],
    lists:filter(MatchFunction, KB);

query_kb(Name, F) when is_function(F) ->
    lists:filter(F, get_kb(Name)).

%%====================================================================
%% Internal functions
%%====================================================================
execute_pending(EngineState0 = #seresye{pending_actions=[PA | Rest]}) ->
    EngineState1 = PA(EngineState0#seresye{pending_actions=Rest}),
    execute_pending(EngineState1);
execute_pending(EngineState0 = #seresye{pending_actions=[]}) ->
    EngineState0.

add_rule__(EngineState0 = #seresye{join=Join},
           Rule,
           {PConds, NConds}) when is_tuple(Rule)->
    Root = seresye_tree_list:get_root(Join),
    case NConds of
        [] ->
            make_struct(EngineState0, Rule, PConds, [], Root,
                        nil);
        _ ->
            {EngineState1, Np_node} = make_struct(EngineState0, Rule, NConds, [],
                                                  Root, ng),
            Root1 = seresye_tree_list:refresh(Root,
                                             EngineState1#seresye.join),
            make_struct(EngineState1, Rule, PConds, [], Root1,
                        {Np_node, NConds})
    end.

%% @doc initializes the memory with a new alpha-present the facts
%% In the Knowledge Base that satisfy the condition Cond
initialize_alfa(_, _, []) -> nil;
initialize_alfa(Cond, Tab, [Fact | Other_fact]) ->
    Fun = prepare_match_alpha_fun(Cond),
    case Fun(Fact) of
        Fact ->
            ets:insert(Tab, Fact),
            initialize_alfa(Cond, Tab, Other_fact);
        false -> initialize_alfa(Cond, Tab, Other_fact)
    end.

prepare_match_alpha_fun(Cond) ->
    Expr =
        [{'fun',1,
          {clauses,[{clause,1,
                     [{match,1,Cond,{var,1,'X__x__X'}}],
                     [],
                     [{var,1,'X__x__X'}]},
                    {clause,1,[{var,1,'_'}],[],[{atom,1,false}]}]}}],
    eval(Expr).

get_abstract_code(Module) ->
    case beam_lib:chunks(code:which(Module), [abstract_code]) of
        {error, beam_lib, {file_error, File, enoent}} ->
            erlang:throw({seresye,
                          {unable_to_find_file, Module, File}});
        {ok, {Module, [{abstract_code,no_abstract_code}]}} ->
            erlang:throw({seresye,
                          {no_abstract_code, Module,
                           "module must be compiled with +debug_info"}});
        {ok, {Module, [{abstract_code, {raw_abstract_v1, Forms}}]}} ->
            Forms
    end.

get_conds(FunName, ClauseID, AST) ->
    Records = get_records(AST, []),
    case search_fun(FunName, AST) of
        {error, Reason} ->
            erlang:throw({seresye, {error_parsing_forms,
                                   FunName, Reason}});
        {ok, CL} ->
            if ClauseID > 0 ->
                    [read_clause(FunName, ClauseID,
                                 lists:nth(ClauseID, CL), Records, AST)];
               true ->
                    [read_clause(FunName, ClauseID, Clause, Records, AST) ||
                        Clause <- CL]
            end
    end.

get_records([], Acc) ->
    Acc;
get_records([{attribute, _, record,
              {RecordName, RecordFields}}
             | Tail],
            Acc) ->
    NewAcc = [{RecordName,
               get_record_fields(RecordFields, [])}
              | Acc],
    get_records(Tail, NewAcc);
get_records([_ | Tail], Acc) ->
    get_records(Tail, Acc).

get_record_fields([], Acc) -> lists:reverse(Acc);
get_record_fields([{typed_record_field, RecordField, _T} | Tail], Acc) ->
    get_record_fields([RecordField | Tail], Acc);
get_record_fields([{record_field, _,
                    {atom, _, FieldName}, {nil, _}}
                   | Tail],
                  Acc) ->
    NewAcc = [{FieldName, {nil, []}} | Acc],
    get_record_fields(Tail, NewAcc);
get_record_fields([{record_field, _,
                    {atom, _, FieldName}, {Type, _, DefaultValue}}
                   | Tail],
                  Acc) ->
    NewAcc = [{FieldName, {Type, DefaultValue}} | Acc],
    get_record_fields(Tail, NewAcc);
get_record_fields([{record_field, _,
                    {atom, _, FieldName}}
                   | Tail],
                  Acc) ->
    NewAcc = [{FieldName, {atom, undefined}} | Acc],
    get_record_fields(Tail, NewAcc).


get_rules(Module, AST) ->
    get_rules(Module, AST, []).

get_rules(_Module, [], Acc) ->
    lists:reverse(Acc);
get_rules(Module, [{attribute, _, rule, Rule} | Rest], Acc) ->
    get_rules(Module, Rest, [resolve_rule(Module, Rule) | Acc]);
get_rules(Module, [{attribute, _, rules, RuleList0} | Rest], Acc)
  when is_list(RuleList0) ->
    RuleList1 = [resolve_rule(Module, Rule) || Rule <- RuleList0],
    get_rules(Module, Rest, RuleList1 ++ Acc);
get_rules(Module, [_ | Rest], Acc) ->
    get_rules(Module, Rest, Acc).


resolve_rule(Module, Rule) when is_atom(Rule) ->
    {Module, Rule, 0, 0};
resolve_rule(Module, {Rule, Salience}) ->
    {Module, Rule, 0, Salience};
resolve_rule(Module, {Rule, Salience, ClauseID}) ->
    {Module, Rule, ClauseID, Salience};
resolve_rule(Module, Rule) ->
    erlang:throw({seresye, {invalid_rule_definition, Module, Rule}}).


search_fun([], _) ->
    {error, "function not found"};
search_fun(Func, [{function, _, Func, _, ClauseList} | _Rest]) ->
    {ok, ClauseList};
search_fun( Func, [_Tuple | Rest]) ->
    search_fun(Func, Rest).

read_clause(FunName, ClauseID, Clause, RecordList, AST) ->
    {clause, _, ParamList, _, _} = Clause,
    PosConds = read_parameters(ParamList, RecordList),
    {PosConds, get_neg_conds(FunName, ClauseID, AST)}.

get_neg_conds(FunName0, ClauseID0, AST) ->
    lists:foldl(fun({attribute,_,rule_neg, {FunName1, ClauseID1, Detail}}, _)
                   when ClauseID0 == ClauseID1, FunName0 == FunName1 ->
                        neg_to_list(rewrite_negs(Detail), []);
                   (_, Acc) ->
                        Acc
                end, [], AST).

neg_to_list({cons, _, C1, Rest}, Acc) ->
    neg_to_list(Rest, [C1 | Acc]);
neg_to_list({nil, _}, Acc) ->
    lists:reverse(Acc).


rewrite_negs({tuple, _, Elements}) ->
    {tuple, 0, lists:map(fun rewrite_negs/1, Elements)};
rewrite_negs(C = {cons, _, _, _}) ->
    rewrite_cons(C);
rewrite_negs({atom, _, '___IGNORE___'}) ->
    {var, 0, '_'};
rewrite_negs(Else) ->
    remove_line_numbers(Else).

rewrite_cons({cons, _, Element, nil}) ->
    {cons, 0, rewrite_negs(Element), nil};
rewrite_cons({cons, _, E1, Rest}) ->
    {cons, 0, rewrite_negs(E1), rewrite_negs(Rest)}.

read_parameters([{var, _, _} | Tail], RecordList) ->
    extract_parameters(Tail, RecordList, []).

extract_parameters([], _, Acc) ->
    lists:reverse(Acc);
extract_parameters([{match, _, {record, _, _, _} = R,
                     {var, _, _}}
                    | Tail],
                   RecordList,
                   Acc) ->
    extract_parameters([R | Tail], RecordList, Acc);
extract_parameters([{match, _, Condition,
                     {var, _, _}}
                    | Tail],
                   RecordList,
                   Acc) ->
    extract_parameters(Tail, RecordList, [remove_line_numbers(Condition) | Acc]);
extract_parameters([{match, _, {var, _, _},
                     {record, _, _, _} = R}
                    | Tail],
                   RecordList,
                   Acc) ->
    extract_parameters([R | Tail], RecordList, Acc);
extract_parameters([{match, _, {var, _, _},
                     Condition}
                    | Tail],
                   RecordList,
                   Acc) ->
    extract_parameters(Tail, RecordList, [remove_line_numbers(Condition) | Acc]);
extract_parameters([{record, _, RecordName, Condition}
                    | Tail],
                   RecordList,
                   Acc) ->
    RecordDefinition = get_record_def(RecordName,
                                      RecordList),
    RecordDefaults = make_record_default(RecordDefinition,
                                         []),
    Pattern = {tuple, 0, [{atom, 0, RecordName}
                          | make_record_pattern(Condition, RecordDefaults,
                                                RecordDefinition)]},
    extract_parameters(Tail, RecordList, [Pattern | Acc]);
extract_parameters([{tuple, L, Elements}| Tail], RecordList, Acc) ->
    Pattern = {tuple, L, 
               extract_parameters(Elements, RecordList, [])},
    extract_parameters(Tail, RecordList, [Pattern | Acc]);
extract_parameters([Condition | Tail], RecordList, Acc) ->
    extract_parameters(Tail, RecordList, [remove_line_numbers(Condition) | Acc]).

%% @doc removing the line numbers make it easier to compare conditions
%% for equality
remove_line_numbers(Values) when is_list(Values) ->
    lists:map(fun remove_line_numbers/1, Values);
remove_line_numbers({op, _, Op, T1, T2}) ->
    {op, 0, Op, remove_line_numbers(T1), remove_line_numbers(T2)};
remove_line_numbers({Type, L})
    when is_integer(L), is_atom(Type) ->
    {Type, 0};
remove_line_numbers({string, _, String}) ->
    {string, 0, String};
remove_line_numbers({Type, L, Values})
  when is_integer(L), is_atom(Type), is_list(Values) ->
    {Type, 0, lists:map(fun remove_line_numbers/1, Values)};
remove_line_numbers({Type, L, Value})
  when is_integer(L), is_atom(Type) ->
    {Type, 0, Value};
remove_line_numbers({Type, L, C1, C2})
  when is_integer(L), is_atom(Type) ->
    {Type, 0, remove_line_numbers(C1), remove_line_numbers(C2)}.




get_record_def(_, []) -> nil;
get_record_def(Name, [{Name, Definition} | _Rest]) ->
    Definition;
get_record_def(Name, [_ | Rest]) ->
    get_record_def(Name, Rest).

make_record_default([], Acc) -> lists:reverse(Acc);
make_record_default([{_,_} | Tail], Acc) ->
    make_record_default(Tail, [{var, 0, '_'} | Acc]).

make_record_pattern([], Pattern, _RecordDefinition) ->
    Pattern;
make_record_pattern([{record_field, _,
                      {atom, _, FieldName}, MatchValue}
                     | Tail],
                    Pattern, RecordDefinition) ->
    FieldPosition = get_record_field_pos(FieldName,
                                         RecordDefinition, 1),
    NewPattern = lists:sublist(Pattern, FieldPosition - 1)
        ++
        [MatchValue] ++
        lists:sublist(Pattern, FieldPosition + 1,
                      length(Pattern) - FieldPosition),
    make_record_pattern(Tail, NewPattern, RecordDefinition).

get_record_field_pos(Name, [], _) ->
    exit({bad_record_field, Name});
get_record_field_pos(Name, [{Name, _} | _], Index) ->
    Index;
get_record_field_pos(Name, [{Name} | _], Index) ->
    Index;
get_record_field_pos(Name, [_ | Tail], Index) ->
    get_record_field_pos(Name, Tail, Index + 1).


%% @doc P and a list containing the conditions prior to Cond Of the
%% same production
make_struct(EngineState0 = #seresye{join=Join}, _Rule, [], _P, Cur_node, ng) ->
    %% create production node
    Key = {np_node, nil},
    {Node, {Join2, EngineState1}} =
        case seresye_tree_list:child(Key, Cur_node, Join) of
            false ->
                Value = [],
                {Node0, Join1} = seresye_tree_list:insert(Key, Value,
                                                         Cur_node, Join),
                {Node0, update_new_node(EngineState0, Node0, Cur_node,
                                        Join1)};
            Node0 -> {Node0, {Join, EngineState0}}
        end,
    {EngineState1#seresye{join=Join2}, Node};
make_struct(EngineState0 = #seresye{join=Join}, Rule, [], _P, Cur_node, nil) ->
    %% create production node
    Key = {p_node, Rule},
    {Join2, EngineState1} = case seresye_tree_list:child(Key, Cur_node, Join) of
                                false ->
                                    Value = [],
                                    {Node, Join1} = seresye_tree_list:insert(Key, Value,
                                                                            Cur_node, Join),
                                    update_new_node(EngineState0, Node, Cur_node,
                                                    Join1);
                                Node ->
                                    {Fun, _Salience} = Rule,
                                    Key1 = element(1, Node),
                                    Sal = element(2, element(2, Key1)),
                                    io:format(">> Rule (~w) already present ~n", [Fun]),
                                    io:format(">> with salience = ~w~n", [Sal]),
                                    io:format(">> To change salience use 'set_salience()'.~n"),
                                    {Join, EngineState0}
                            end,
    EngineState1#seresye{join=Join2};
make_struct(EngineState0 = #seresye{join=Join}, Rule, [], P, Cur_node, {Nod, NConds}) ->
    Id = seresye_tree_list:get_id(Nod),
    {Cur_node1, Join1, EngineState1} = make_join_node(EngineState0, Join,
                                                      {old, {n_node, Id}}, NConds, P,
                                                      Cur_node),
    Nod1 = seresye_tree_list:refresh(Nod, Join1),
    Join2 = seresye_tree_list:set_child(Cur_node1, Nod1,
                                       Join1),
    Key = {p_node, Rule},
    {Join4, EngineState2} =
        case seresye_tree_list:child(Key, Cur_node1, Join2) of
            false ->
                Value = [],
                {Node, Join3} = seresye_tree_list:insert(Key, Value,
                                                        Cur_node1, Join2),
                Cur_node2 = seresye_tree_list:refresh(Cur_node1, Join3),
                update_new_node(EngineState1, Node, Cur_node2,
                                Join3);
            Node ->
                {Fun, _Salience} = Rule,
                Key1 = element(1, Node),
                Sal = element(2, element(2, Key1)),
                io:format(">> Rule (~w) already present ~n", [Fun]),
                io:format(">> with salience = ~w~n", [Sal]),
                io:format(">> To change salience use 'set_salience()'.~n"),
                {Join2, EngineState1}
        end,
    EngineState2#seresye{join=Join4};
make_struct(EngineState0 = #seresye{join=Join}, Rule, [Cond | T], P, Cur_node, Nod) ->
    {Alfa1, Tab} = add_alfa(EngineState0, Cond),
    {Cur_node1, Join1, EngineState1} = make_join_node(EngineState0, Join, Tab,
                                                      Cond, P, Cur_node),
    P1 = P ++ [Cond],
    make_struct(EngineState1#seresye{alfa=Alfa1, join=Join1}, Rule, T,
                P1, Cur_node1, Nod).

add_alfa(#seresye{kb=Kb, alfa=Alfa}, Cond) ->
    case is_present(Cond, Alfa) of
        false ->
            Tab = ets:new(alfa, [bag]),
            Fun = [{'fun',1,
                    {clauses,
                     [{clause,1,
                       [Cond],[],[{atom,1,true}]}]}}],
            Alfa_fun = eval(Fun),
            Alfa1 = [{Cond, Tab, Alfa_fun} | Alfa],
            initialize_alfa(Cond, Tab, Kb),
            {Alfa1, {new, Tab}};
        {true, Tab} -> {Alfa, {old, Tab}}
    end.

remove_prod(EngineState0 = #seresye{join=Join}, Fun) ->
    case seresye_tree_list:keysearch({p_node, Fun}, Join) of
        false -> EngineState0;
        Node ->
            Parent_node = seresye_tree_list:get_parent(Node, Join),
            Join1 = seresye_tree_list:remove_node(Node, Join),
            Parent_node1 = seresye_tree_list:refresh(Parent_node,
                                                    Join1),
            EngineState1 = remove_nodes(Parent_node1,
                                        EngineState0#seresye{join=Join1}),
            remove_prod(EngineState1, Fun)
    end.

remove_nodes(Node, EngineState0 = #seresye{join=Join, alfa=Alfa}) ->
    case seresye_tree_list:have_child(Node) of
        false ->
            case seresye_tree_list:is_root(Node) of
                false ->
                    Parent_node = seresye_tree_list:get_parent(Node, Join),
                    Join1 = seresye_tree_list:remove_node(Node, Join),
                    Parent_node1 = seresye_tree_list:refresh(Parent_node,
                                                            Join1),
                    {First, _} = seresye_tree_list:get_key(Node),
                    case First of
                        {n_node, IdNp_node} ->
                            ParentKey = seresye_tree_list:get_key(Parent_node1),
                            %% delete all nodes of the conditions negated
                            Np_node = seresye_tree_list:get_node(IdNp_node, Join1),
                            Join2 = seresye_tree_list:remove_child(Node, Np_node,
                                                                  Join1),
                            Np_node1 = seresye_tree_list:refresh(Np_node, Join2),
                            EngineState1 = remove_nodes(Np_node1, EngineState0#seresye{join=Join2}),
                            %% Recovery of the parent node of the node passed as an argument n_node
                            %% The parent can now have a different id, but has the same key
                            Join3 = EngineState1#seresye.join,
                            Parent_node2 = seresye_tree_list:keysearch(ParentKey,
                                                                      Join3),
                            remove_nodes(Parent_node2, EngineState1);
                        np_node ->
                            remove_nodes(Parent_node1, EngineState0#seresye{join=Join1});
                        Tab ->
                            Alfa1 = case seresye_tree_list:is_present(Tab, Join1) of
                                        false ->
                                            ets:delete(Tab),
                                            lists:keydelete(Tab, 2, Alfa);
                                        true ->
                                            Alfa
                                    end,
                            remove_nodes(Parent_node1,
                                         EngineState0#seresye{alfa=Alfa1,
                                                             join=Join1})
                    end;
                true -> EngineState0
            end;
        true -> EngineState0
    end.

%% @doc occurs and if the condition Cond is present
is_present(_Cond, []) -> false;
is_present(Cond, [{C1, Tab, _Alfa_fun} | Other_cond]) ->
    case same_cond(Cond, C1) of
        true -> {true, Tab};
        false -> is_present(Cond, Other_cond)
    end.

same_cond(Cond, Cond) -> true;
same_cond(_Cond1, _Cond2) -> false.

eval(Expr) ->
    ?LOG("FUN = ~p~n", [String]),
    case catch erl_eval:exprs(Expr, erl_eval:new_bindings()) of
        {'EXIT', _} -> false;
        {value, Value, _Bindings} -> Value;
        _ -> false
    end.

prepare_fun(_Cond, []) -> [{atom, 1, nil}];
prepare_fun(Cond0, Cond1)
  when not is_list(Cond0), is_list(Cond1) ->
       [{'fun',1,
         {clauses,
          [{clause,1,
            [Cond0,
             prepare_rest(Cond1)],
            [],
            [{atom,1,true}]}]}}];
prepare_fun(Cond0, Cond1)
  when is_list(Cond0), is_list(Cond1) ->
       [{'fun',1,
         {clauses,
          [{clause,1,
            [prepare_rest(Cond0),
             prepare_rest(Cond1)],
            [],
            [{atom,1,true}]}]}}].

prepare_rest([Element | Rest]) ->
    {cons, 1, Element, prepare_rest(Rest)};
prepare_rest([]) ->
    {nil, 1}.

%% @doc join_node entering any new updates in the beta-token memory
update_new_node(EngineState0, Node, Parent_node, Join) ->
    case seresye_tree_list:is_root(Parent_node) of
        false ->
            Children = seresye_tree_list:children(Parent_node, Join),
            case Children -- [Node] of
                [] ->
                    case seresye_tree_list:get_key(Parent_node) of
                        {{n_node, _IdNp_node}, _Join_fun} ->
                            Beta = seresye_tree_list:get_beta(Parent_node),
                            update_from_n_node(EngineState0, Parent_node, Beta, Join);
                        {Tab, _} ->
                            Fact_list = ets:tab2list(Tab),
                            update_new_node(EngineState0, Node, Parent_node, Join, Fact_list)
                    end;
                [Child | _Other_child] ->
                    Beta = seresye_tree_list:get_beta(Child),
                    Join1 = seresye_tree_list:update_beta(Beta, Node, Join),
                    EngineState1 =
                        case seresye_tree_list:get_key(Node) of
                            {p_node, Rule} ->
                                update_agenda(EngineState0, Beta, Rule);
                            _Key ->
                                EngineState0
                        end,
                    {Join1, EngineState1}
            end;
        true -> {Join, EngineState0}
    end.

update_agenda(EngineState, [], _Rule) -> EngineState;
update_agenda(EngineState, [Tok | OtherTok], Rule) ->
    EngineState1 = signal(EngineState, Tok, plus, Rule),
    update_agenda(EngineState1, OtherTok, Rule).

update_new_node(EngineState0, _Node, _Parent_node, Join, []) ->
    {Join, EngineState0};
update_new_node(EngineState0, Node, Parent_node, Join,
                [Fact | Other_fact]) ->
    Tok_list = right_act({Fact, plus}, Parent_node),
    {Join1, EngineState1} = pass_tok(EngineState0, Tok_list, [Node], Join),
    Node1 = seresye_tree_list:refresh(Node, Join1),
    update_new_node(EngineState1, Node1, Parent_node, Join1, Other_fact).

update_from_n_node(EngineState0, Parent_node, [], Join) ->
    Join1 = seresye_tree_list:update_node(Parent_node, Join),
    {Join1, EngineState0};
update_from_n_node(EngineState0, Parent_node, [Tok | OtherTok], Join) ->
    {Join1, EngineState1} = left_act(EngineState0, {Tok, plus}, [Parent_node],
                                     Join),
    update_from_n_node(Parent_node, OtherTok, Join1,
                       EngineState1).

signal(EngineState0 = #seresye{pending_actions=PAList}, Token, Sign, {Fun, Salience}) ->
    NewPA =
        case Sign of
            plus ->
                fun(EngineState1) ->
                        seresye_agenda:add_activation(EngineState1, Fun,
                                                     Token, Salience)
                end;
            minus ->
                ActivationId = seresye_agenda:get_activation(EngineState0,
                                                            {Fun, Token}),
                fun(EngineState1) ->
                        seresye_agenda:delete_activation(EngineState1, ActivationId)
                end
        end,
    EngineState0#seresye{pending_actions=PAList ++ [NewPA]}.

%%====================================================================
%% Fact Assertion/Retraction Functions
%%====================================================================
check_cond(EngineState, [], {_Fact, _Sign}) -> EngineState;
check_cond(EngineState0, [{_C1, Tab, Alfa_fun} | T],
           {Fact, Sign}) ->
    case catch Alfa_fun(Fact) of
        true ->
            case Sign of
                plus -> ets:insert(Tab, Fact);
                minus -> ets:delete_object(Tab, Fact)
            end,
            EngineState1 = pass_fact(EngineState0, Tab, {Fact, Sign}),
            check_cond(EngineState1, T, {Fact, Sign});
        {'EXIT', {function_clause, _}} ->
            check_cond(EngineState0, T, {Fact, Sign});
        _Other -> check_cond(EngineState0, T, {Fact, Sign})
    end.

%% @doc propagates the 'done' to all the nodes that follow the alpha-memory
%% With an index tab
pass_fact(EngineState0 = #seresye{join=Join}, Tab, {Fact, Sign}) ->
    Succ_node_list = seresye_tree_list:lookup_all(Tab, Join),
    {Join1, EngineState1} = propagate(EngineState0, Succ_node_list,
                                      {Fact, Sign}, Join),
    EngineState1#seresye{join=Join1}.

propagate(EngineState0, [], {_Fact, _Sign}, Join) ->
    {Join, EngineState0};
propagate(EngineState0, [Join_node | T], {Fact, Sign}, Join) ->
    Join_node1 = seresye_tree_list:refresh(Join_node, Join),
    Tok_list = right_act({Fact, Sign}, Join_node1),
    {Join1, EngineState1} =
        case seresye_tree_list:get_key(Join_node1) of
            {{n_node, _}, _} ->
                propagate_nnode(EngineState0, Join_node1, Tok_list,
                                Sign, Join);
            {_Tab, _Fun} ->
                Children_list = seresye_tree_list:children(Join_node1,
                                                          Join),
                pass_tok(EngineState0, Tok_list, Children_list,
                         Join)
        end,
    propagate(EngineState1, T, {Fact, Sign}, Join1).

propagate_nnode(EngineState0, _Join_node, [], _, Join) ->
    {Join, EngineState0};
propagate_nnode(EngineState0, Join_node, Tok_list, Sign, Join) ->
    Children_list = seresye_tree_list:children(Join_node,
                                              Join),
    Toks = [make_toks(Tok, Sign) || Tok <- Tok_list],
    case Sign of
        plus -> pass_tok(EngineState0, Toks, Children_list, Join);
        minus ->
            {{n_node, IdNp_node}, Join_fun} =
                seresye_tree_list:get_key(Join_node),
            test_nnode(EngineState0, Toks, IdNp_node, Join_fun, Join_node, Join)
    end.

make_toks(Tk, Sign) ->
    L = element(1, Tk),
    T1 = lists:sublist(L, length(L) - 1),
    case Sign of
        plus -> {T1, minus};
        minus -> {T1, plus}
    end.

%% @doc Token propagates all the nodes in Children_list
pass_tok(EngineState0, [], _Children_list, Join) ->
    {Join, EngineState0};
pass_tok(EngineState0, [Tok | T], Children_list, Join) ->
    {Join1, EngineState1} = left_act(EngineState0, Tok, Children_list, Join),
    Children_list1 = refresh(Children_list, Join1),
    pass_tok(EngineState1, T, Children_list1, Join1).

%% @doc Insert the token in Tok Beta_memory of Join_node, Token Tok
%% compares the present with all the facts nell'alfa memory Associated
%% with Join_node and if you have a successful propagates Token to the
%% child nodes
left_act(EngineState0, {_Token, _Sign}, [], Join) ->
    {Join, EngineState0};
left_act(EngineState0, {Token, Sign}, [Join_node | T], Join) ->
    Beta = seresye_tree_list:get_beta(Join_node),
    Beta1 =
        case Sign of
            plus -> Beta ++ [Token];
            minus -> Beta -- [Token]
        end,
    Join1 = seresye_tree_list:update_beta(Beta1, Join_node,
                                         Join),
    case seresye_tree_list:get_key(Join_node) of
        {p_node, Rule} ->
            left_act(signal(EngineState0, Token, Sign, Rule),
                     {Token, Sign}, T, Join1);
        {{n_node, IdNp_node}, Join_fun} ->
            left_act_nnode(EngineState0, {Token, Sign}, IdNp_node, Join_fun,
                           [Join_node | T], Join1);
        {np_node, nil} ->
            Children_list = seresye_tree_list:children(Join_node,
                                                      Join1),
            {Join2, EngineState1} = propagate(EngineState0, Children_list,
                                              {Token, Sign}, Join1),
            left_act(EngineState1, {Token, Sign}, T, Join2);
        {Tab, Join_fun} ->
            Alfa_mem = ets:tab2list(Tab),
            Tok_list = join_left({Token, Sign}, Alfa_mem, Join_fun),
            Children_list = seresye_tree_list:children(Join_node,
                                                      Join1),
            {Join2, EngineState1} = pass_tok(EngineState0, Tok_list, Children_list,
                                             Join1),
            left_act(EngineState1, {Token, Sign}, T, Join2)
    end.

join_left({Tok, Sign}, Mem, Join_fun) ->
    lists:foldl(fun (WmeOrTok, Tok_list) ->
                        Tok1 = match(WmeOrTok, Tok, Join_fun),
                        case Tok1 of
                            [] -> Tok_list;
                            _Other -> Tok_list ++ [{Tok1, Sign}]
                        end
                end,
                [], Mem).

left_act_nnode(EngineState0, {Token, Sign}, IdNp_node, Join_fun,
               [Join_node | T], Join) ->
    Np_node = seresye_tree_list:get_node(IdNp_node, Join),
    BetaN = seresye_tree_list:get_beta(Np_node),
    Tok_list = join_left({Token, Sign}, BetaN, Join_fun),
    case Tok_list of
        [] ->
            Children_list = seresye_tree_list:children(Join_node,
                                                      Join),
            {Join1, EngineState1} = pass_tok(EngineState0, [{Token, Sign}],
                                             Children_list, Join),
            left_act(EngineState1, {Token, Sign}, T, Join1);
        Tok_list -> left_act(EngineState0, {Token, Sign}, T, Join)
    end.

test_nnode(EngineState0, [], _, _, _, Join) -> {Join, EngineState0};
test_nnode(EngineState0, [Tok | OtherTok], IdNpNode, Join_fun,
           Join_node, Join) ->
    {Join1, EngineState1} = left_act_nnode(EngineState0, Tok, IdNpNode,
                                           Join_fun, [Join_node], Join),
    test_nnode(EngineState1, OtherTok, IdNpNode, Join_fun, Join_node,
               Join1).


%% @doc WME compares with the new all tokens in the beta-memory Parent
%% and returns a list of new tokens ([+ Tok1 WME, WME + Tok2,...])  Or
%% an empty list if the WME not 'match' with tokens of Beta-memory
right_act({Fact, Sign}, Join_node) ->
    Beta = element(2, Join_node),
    Join_fun = element(2, element(1, Join_node)),
    join_right({Fact, Sign}, Beta, Join_fun).

join_right({Fact, Sign}, Beta, nil) ->
    append(Beta, {Fact, Sign});
join_right({_Fact, _Sign}, [], _Join_fun) -> [];
join_right({Fact, Sign}, Beta, Join_fun) ->
    join_right({Fact, Sign}, Beta, Join_fun, []).

join_right({_Fact, _Sign}, [], _Join_fun, PassedTok) ->
    PassedTok;
join_right({Fact, Sign}, [Tok | T], Join_fun,
           PassedTok) ->
    Pass = match(Fact, Tok, Join_fun),
    case Pass of
        [] -> join_right({Fact, Sign}, T, Join_fun, PassedTok);
        _New_tok ->
            L = PassedTok ++ [{Pass, Sign}],
            join_right({Fact, Sign}, T, Join_fun, L)
    end.

refresh([], _Join, L) -> lists:reverse(L);
refresh([Node | OtherNode], Join, L) ->
    Node1 = seresye_tree_list:refresh(Node, Join),
    L1 = [Node1 | L],
    refresh(OtherNode, Join, L1).

refresh(N, Join) -> refresh(N, Join, []).

%% @doc WME is the match between the token and Tok and returns an
%% empty list in case Negative or a new token (Tok + WME) if so
match(Wme, Tok, Join_fun) ->
    case catch Join_fun(Wme, Tok) of
        true -> Tok ++ [Wme];
        {'EXIT', {function_clause, _}} -> [];
        _Other -> []
    end.

append([], {Fact, Sign}) -> [{[Fact], Sign}];
append(Beta, {Fact, Sign}) ->
    lists:foldl(fun (Tok, New_Beta) ->
                        Tok1 = Tok ++ [Fact], New_Beta ++ {Tok1, Sign}
                end,
                [], Beta).

%% @doc shares or create a join-node
make_join_node(EngineState0, J, {new, Tab}, Cond, P, Parent_node) ->
    Join_fun = eval(prepare_fun(Cond, P)),
    new_join(EngineState0, J, Tab, Join_fun, Parent_node);
make_join_node(EngineState0, J, {old, Tab}, Cond, P, Parent_node) ->
    Result = seresye_tree_list:child(Tab, Parent_node, J),
    case Result of
        false ->
            Join_fun = eval(prepare_fun(Cond, P)),
            new_join(EngineState0, J, Tab, Join_fun, Parent_node);
        Node -> {Node, J, EngineState0}
    end.

new_join(EngineState0, J, Tab, Join_fun, Parent_node) ->
    Key = {Tab, Join_fun},
    Value = [],
    {Node, J1} = seresye_tree_list:insert(Key, Value,
                                         Parent_node, J),
    {J2, EngineState1} = update_new_node(EngineState0, Node, Parent_node, J1),
    Node1 = seresye_tree_list:refresh(Node, J2),
    {Node1, J2, EngineState1}.
