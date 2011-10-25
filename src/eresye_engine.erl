%%%  ERESYE, an ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
-module(eresye_engine).

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

-export([new/0, new/1,
         add_rules/2, add_rule/2, add_rule/3, assert/2, get_kb/1,
         get_rules_fired/1, get_client_state/1, set_client_state/2,
         query_kb/2, remove_rule/2, retract/2]).

%%====================================================================
%% External functions
%%====================================================================
new() ->
    new(undefined).

new(ClientState) ->
    eresye_agenda:new(#eresye{kb=[], alfa=[],
                              join=eresye_tree_list:new(),
                              pending_actions=[],
                              client_state=ClientState}).

set_client_state(EngineState, NewState) ->
    EngineState#eresye{client_state=NewState}.

get_client_state(#eresye{client_state=State}) ->
    State.

%% @doc Insert a fact in the KB.
%% It also checks if the fact verifies any condition,
%% if this is the case the fact is also inserted in the alpha-memory
assert(EngineState0, Facts) when is_list(Facts) ->
    lists:foldl(fun(Fact, EngineState1) ->
                        assert(EngineState1, Fact)
                end, EngineState0, Facts);
assert(EngineState = #eresye{kb=Kb, alfa=Alfa}, Fact) when is_tuple(Fact) ->
    execute_pending(case lists:member(Fact, Kb) of
                        false ->
                            Kb1 = [Fact | Kb],
                            check_cond(EngineState#eresye{kb=Kb1}, Alfa,
                                       {Fact, plus});
                        true -> EngineState
                    end).

%% @doc removes a 'fact' in the Knowledge Base and if something occurs
%% Condition is also deleted from the corresponding alpha-memory
retract(EngineState0, Facts) when is_list(Facts) ->
    lists:foldl(fun(Fact, EngineState1) ->
                        retract(EngineState1, Fact)
                end, EngineState0, Facts);
retract(EngineState = #eresye{kb=Kb, alfa=Alfa}, Fact) when is_tuple(Fact) ->
    execute_pending(case lists:member(Fact, Kb) of
                        true ->
                            Kb1 = Kb -- [Fact],
                            check_cond(EngineState#eresye{kb=Kb1}, Alfa,
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
            erlang:throw({eresye, {no_rules_specified, Module}});
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
        error -> erlang:throw({eresye, {error_extracting_conditions, Fun}});
        CondsList ->
            execute_pending(
              lists:foldl(fun (X, EngineState1) ->
                                  case X of
                                      {error, Msg} ->
                                          erlang:throw({eresye, {error_adding_rule,
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
    execute_pending(remove_prod(eresye_agenda:delete_rule(EngineState0, Rule), Rule)).

get_rules_fired(EngineState) ->
    eresye_agenda:get_rules_fired(EngineState).

get_kb(#eresye{kb=Kb}) ->
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
execute_pending(EngineState0 = #eresye{pending_actions=[PA | Rest]}) ->
    EngineState1 = PA(EngineState0#eresye{pending_actions=Rest}),
    execute_pending(EngineState1);
execute_pending(EngineState0 = #eresye{pending_actions=[]}) ->
    EngineState0.

add_rule__(EngineState0 = #eresye{join=Join},
           Rule,
           {PConds, NConds}) when is_tuple(Rule)->
    Root = eresye_tree_list:get_root(Join),
    case NConds of
        [] ->
            make_struct(EngineState0, Rule, PConds, [], Root,
                        nil);
        _ ->
            {EngineState1, Np_node} = make_struct(EngineState0, Rule, NConds, [],
                                                  Root, ng),
            Root1 = eresye_tree_list:refresh(Root,
                                             EngineState1#eresye.join),
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
    FunString =
        lists:flatten(io_lib:format("fun (~s = X__x__X) -> X__x__X;    (_) "
                                    " -> false end.",
                                    [Cond])),
    evaluate(FunString).

get_abstract_code(Module) ->
    case beam_lib:chunks(code:which(Module), [abstract_code]) of
        {error, beam_lib, {file_error, File, enoent}} ->
            erlang:throw({eresye,
                          {unable_to_find_file, Module, File}});
        {ok, {Module, [{abstract_code,no_abstract_code}]}} ->
            erlang:throw({eresye,
                          {no_abstract_code, Module,
                           "module must be compiled with +debug_info"}});
        {ok, {Module, [{abstract_code, {raw_abstract_v1, Forms}}]}} ->
            Forms
    end.

get_conds(Func, ClauseID, AST) ->
    Records = get_records(AST, []),
    case search_fun(AST, Func, Records) of
        {error, Reason} ->
            erlang:throw({eresye, {error_parsing_forms,
                                   Func, Reason}});
        {ok, CL} ->
            ClauseList =
                if ClauseID > 0 ->
                        [lists:nth(ClauseID, CL)];
                   true -> CL
                end,
            case read_clause(ClauseList, [], Records) of
                {error, Reason} ->
                    erlang:throw({eresye, {unable_to_read_clauses,
                                           Func, Reason}});
                CondsList ->
                    CondsList
            end
    end.

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
    erlang:throw({eresye, {invalid_rule_definition, Module, Rule}}).


get_records([], Acc) -> lists:reverse(Acc);
get_records([{attribute, _, record,
              {RecordName, RecordFields}}
             | Tail],
            Acc) ->
    NewAcc = [{RecordName,
               get_record_fields(RecordFields, [])}
              | Acc],
    get_records(Tail, NewAcc);
get_records([_ | Tail], Acc) -> get_records(Tail, Acc).

get_record_fields([], Acc) -> lists:reverse(Acc);
get_record_fields([{record_field, _,
                    {atom, _, FieldName}, {atom, _, DefaultValue}}
                   | Tail],
                  Acc) ->
    NewAcc = [{FieldName, DefaultValue} | Acc],
    get_record_fields(Tail, NewAcc);
get_record_fields([{record_field, _,
                    {atom, _, FieldName}}
                   | Tail],
                  Acc) ->
    NewAcc = [{FieldName} | Acc],
    get_record_fields(Tail, NewAcc).

search_fun([], _, _RecordList) ->
    {error, "function not found"};
search_fun([{function, _, _Func, _, ClauseList} | _Other],
           _Func, _RecordList) ->
    {ok, ClauseList};
search_fun([_Tuple | Other], Func, RecordList) ->
    search_fun(Other, Func, RecordList).

read_clause([], CondsList, _RecordList) -> CondsList;
read_clause([Clause | OtherClause], CondsList,
            RecordList) ->
    {clause, _, ParamList, GuardList, _} = Clause,
    PosConds = read_parameters(ParamList, RecordList),
    NegConds = read_guard(GuardList),
    CondsList1 = CondsList ++ [{PosConds, NegConds}],
    read_clause(OtherClause, CondsList1, RecordList).

read_guard([]) -> [];
read_guard([[{op, _, 'not', Conds} | _]
            | _OtherGuard]) ->
    Nc = get_neg_cond(Conds, []), lists:reverse(Nc);
read_guard(_Guard) -> [].

get_neg_cond({nil, _}, Nc) -> Nc;
get_neg_cond({cons, _, {string, _, C}, OtherC}, Nc) ->
    Nc1 = [C | Nc], get_neg_cond(OtherC, Nc1);
get_neg_cond(X, Nc) ->
    erlang:throw({eresye, {invalid_condition, {X, Nc},
                           ">> Negated Conditions must be a list "
                           "of String~n"}}).

read_parameters([{var, _, _} | Tail], RecordList) ->
    P = extract_parameters(Tail, [], RecordList),
    Conditions = [build_string_condition(X, []) || X <- P],
    Conditions.

extract_parameters([], Acc, _RecordList) ->
    lists:reverse(Acc);
extract_parameters([{match, _, {tuple, _, Condition},
                     {var, _, _}}
                    | Tail],
                   Acc, RecordList) ->
    extract_parameters(Tail, [Condition | Acc], RecordList);
extract_parameters([{match, _, {var, _, _},
                     {tuple, _, Condition}}
                    | Tail],
                   Acc, RecordList) ->
    extract_parameters(Tail, [Condition | Acc], RecordList);
extract_parameters([{match, _, {record, _, _, _} = R,
                     {var, _, _}}
                    | Tail],
                   Acc, RecordList) ->
    extract_parameters([R | Tail], Acc, RecordList);
extract_parameters([{match, _, {var, _, _},
                     {record, _, _, _} = R}
                    | Tail],
                   Acc, RecordList) ->
    extract_parameters([R | Tail], Acc, RecordList);
extract_parameters([{record, _, RecordName, Condition}
                    | Tail],
                   Acc, RecordList) ->
    RecordDefinition = get_record_def(RecordName,
                                      RecordList),
    RecordDefaults = make_record_default(RecordDefinition,
                                         []),
    Pattern = [{atom, 0, RecordName}
               | make_record_pattern(Condition, RecordDefaults,
                                     RecordDefinition)],
    extract_parameters(Tail, [Pattern | Acc], RecordList);
extract_parameters([{tuple, _, Condition} | Tail], Acc,
                   RecordList) ->
    extract_parameters(Tail, [Condition | Acc], RecordList).

get_record_def(_, []) -> nil;
get_record_def(Name, [{Name, Definition} | _Rest]) ->
    Definition;
get_record_def(Name, [_ | Rest]) ->
    get_record_def(Name, Rest).

make_record_default([], Acc) -> lists:reverse(Acc);
make_record_default([{_} | Tail], Acc) ->
    make_record_default(Tail, [{var, 0, '_'} | Acc]);
make_record_default([{_, Value} | Tail], Acc) ->
    make_record_default(Tail, [{atom, 0, Value} | Acc]).

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

build_string_condition([], Acc) ->
    lists:concat(["{",
                  tl(lists:flatten(lists:reverse(Acc))), "}"]);
build_string_condition([{atom, _, Value} | Tail],
                       Acc) ->
    Term = lists:concat([",'", atom_to_list(Value), "'"]),
    build_string_condition(Tail, [Term | Acc]);
build_string_condition([{integer, _, Value} | Tail],
                       Acc) ->
    Term = lists:concat([",", integer_to_list(Value)]),
    build_string_condition(Tail, [Term | Acc]);
build_string_condition([{var, _, Value} | Tail], Acc) ->
    Term = lists:concat([",", atom_to_list(Value)]),
    build_string_condition(Tail, [Term | Acc]);
build_string_condition([{cons, _, {var, _, Value1},
                         {var, _, Value2}}
                        | Tail],
                       Acc) ->
    Term = lists:flatten([",[", atom_to_list(Value1), "|",
                          atom_to_list(Value2), "]"]),
    build_string_condition(Tail, [Term | Acc]).

%% @doc P and a list containing the conditions prior to Cond Of the
%% same production
make_struct(EngineState0 = #eresye{join=Join}, _Rule, [], _P, Cur_node, ng) ->
    %% create production node
    Key = {np_node, nil},
    {Node, {Join2, EngineState1}} =
        case eresye_tree_list:child(Key, Cur_node, Join) of
            false ->
                Value = [],
                {Node0, Join1} = eresye_tree_list:insert(Key, Value,
                                                         Cur_node, Join),
                {Node0, update_new_node(EngineState0, Node0, Cur_node,
                                        Join1)};
            Node0 -> {Node0, {Join, EngineState0}}
        end,
    {EngineState1#eresye{join=Join2}, Node};
make_struct(EngineState0 = #eresye{join=Join}, Rule, [], _P, Cur_node, nil) ->
    %% create production node
    Key = {p_node, Rule},
    {Join2, EngineState1} = case eresye_tree_list:child(Key, Cur_node, Join) of
                                false ->
                                    Value = [],
                                    {Node, Join1} = eresye_tree_list:insert(Key, Value,
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
    EngineState1#eresye{join=Join2};
make_struct(EngineState0 = #eresye{join=Join}, Rule, [], P, Cur_node, {Nod, NConds}) ->
    Id = eresye_tree_list:get_id(Nod),
    {Cur_node1, Join1, EngineState1} = make_join_node(EngineState0, Join,
                                                      {old, {n_node, Id}}, NConds, P,
                                                      Cur_node),
    Nod1 = eresye_tree_list:refresh(Nod, Join1),
    Join2 = eresye_tree_list:set_child(Cur_node1, Nod1,
                                       Join1),
    Key = {p_node, Rule},
    {Join4, EngineState2} =
        case eresye_tree_list:child(Key, Cur_node1, Join2) of
            false ->
                Value = [],
                {Node, Join3} = eresye_tree_list:insert(Key, Value,
                                                        Cur_node1, Join2),
                Cur_node2 = eresye_tree_list:refresh(Cur_node1, Join3),
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
    EngineState2#eresye{join=Join4};
make_struct(EngineState0 = #eresye{join=Join}, Rule, [Cond | T], P, Cur_node, Nod) ->
    {Alfa1, Tab} = add_alfa(EngineState0, Cond),
    {Cur_node1, Join1, EngineState1} = make_join_node(EngineState0, Join, Tab,
                                                      Cond, P, Cur_node),
    P1 = P ++ [Cond],
    make_struct(EngineState1#eresye{alfa=Alfa1, join=Join1}, Rule, T,
                P1, Cur_node1, Nod).

add_alfa(#eresye{kb=Kb, alfa=Alfa}, Cond) ->
    case is_present(Cond, Alfa) of
        false ->
            Tab = ets:new(alfa, [bag]),
            Fun = lists:concat(["F=fun(", Cond, ")->true end."]),
            Alfa_fun = evaluate(Fun),
            Alfa1 = [{Cond, Tab, Alfa_fun} | Alfa],
            initialize_alfa(Cond, Tab, Kb),
            {Alfa1, {new, Tab}};
        {true, Tab} -> {Alfa, {old, Tab}}
    end.

%% @doc prepare a string representing Cond=Fact
%% (es. "{X, on, Y}={b1, on, b3}." )
prepare_string(Cond, Fact) ->
    Fact1 = tuple_to_list(Fact),
    Str = lists:concat([Cond, "={"]),
    Str1 = append_fact([], Fact1, siend),
    string:concat(Str, Str1).

append_fact(H, [Elem], _Flag) when is_list(Elem) ->
    lists:concat([H, "[", append_fact([], Elem, noend),
                  "]}."]);
append_fact(H, [Elem], _Flag) when is_tuple(Elem) ->
    Elem1 = tuple_to_list(Elem),
    lists:concat([H, "{",
                  append_fact([], Elem1, noend), "}}."]);
append_fact(H, [Elem], Flag) when is_integer(Elem) ->
    case Flag of
        noend -> lists:concat([H, Elem]);
        _Other -> lists:concat([H, Elem, "}."])
    end;
append_fact(H, [Elem], Flag) ->
    case Flag of
        noend -> lists:concat([H, "'", Elem, "'"]);
        _Other -> lists:concat([H, "'", Elem, "'}."])
    end;
append_fact(H, [Elem | Other_elem], Flag)
  when is_list(Elem) ->
    H1 = lists:concat([H, "[", append_fact([], Elem, noend),
                       "],"]),
    append_fact(H1, Other_elem, Flag);
append_fact(H, [Elem | Other_elem], Flag)
  when is_tuple(Elem) ->
    Elem1 = tuple_to_list(Elem),
    H1 = lists:concat([H, "{",
                       append_fact([], Elem1, noend), "},"]),
    append_fact(H1, Other_elem, Flag);
append_fact(H, [Elem | Other_elem], Flag)
  when is_integer(Elem) ->
    H1 = lists:concat([H, Elem, ","]),
    append_fact(H1, Other_elem, Flag);
append_fact(H, [Elem | Other_elem], Flag) ->
    H1 = lists:concat([H, "'", Elem, "',"]),
    append_fact(H1, Other_elem, Flag).

remove_prod(EngineState0 = #eresye{join=Join}, Fun) ->
    case eresye_tree_list:keysearch({p_node, Fun}, Join) of
        false -> EngineState0;
        Node ->
            Parent_node = eresye_tree_list:get_parent(Node, Join),
            Join1 = eresye_tree_list:remove_node(Node, Join),
            Parent_node1 = eresye_tree_list:refresh(Parent_node,
                                                    Join1),
            EngineState1 = remove_nodes(Parent_node1,
                                        EngineState0#eresye{join=Join1}),
            remove_prod(EngineState1, Fun)
    end.

remove_nodes(Node, EngineState0 = #eresye{join=Join, alfa=Alfa}) ->
    case eresye_tree_list:have_child(Node) of
        false ->
            case eresye_tree_list:is_root(Node) of
                false ->
                    Parent_node = eresye_tree_list:get_parent(Node, Join),
                    Join1 = eresye_tree_list:remove_node(Node, Join),
                    Parent_node1 = eresye_tree_list:refresh(Parent_node,
                                                            Join1),
                    {First, _} = eresye_tree_list:get_key(Node),
                    case First of
                        {n_node, IdNp_node} ->
                            ParentKey = eresye_tree_list:get_key(Parent_node1),
                            %% delete all nodes of the conditions negated
                            Np_node = eresye_tree_list:get_node(IdNp_node, Join1),
                            Join2 = eresye_tree_list:remove_child(Node, Np_node,
                                                                  Join1),
                            Np_node1 = eresye_tree_list:refresh(Np_node, Join2),
                            EngineState1 = remove_nodes(Np_node1, EngineState0#eresye{join=Join2}),
                            %% Recovery of the parent node of the node passed as an argument n_node
                            %% The parent can now have a different id, but has the same key
                            Join3 = EngineState1#eresye.join,
                            Parent_node2 = eresye_tree_list:keysearch(ParentKey,
                                                                      Join3),
                            remove_nodes(Parent_node2, EngineState1);
                        np_node ->
                            remove_nodes(Parent_node1, EngineState0#eresye{join=Join1});
                        Tab ->
                            Alfa1 = case eresye_tree_list:is_present(Tab, Join1) of
                                        false ->
                                            ets:delete(Tab),
                                            lists:keydelete(Tab, 2, Alfa);
                                        true ->
                                            Alfa
                                    end,
                            remove_nodes(Parent_node1, EngineState0#eresye{alfa=Alfa1, join=Join1})
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

same_cond(Cond1, Cond1) -> true;
same_cond(Cond1, Cond2) ->
    ?LOG("Same Cond = ~p, ~p~n", [Cond1, Cond2]),
    C2 = parse_cond(Cond2),
    S1 = prepare_string(Cond1, C2),
    case evaluate(S1) of
        C2 ->
            C1 = parse_cond(Cond1),
            S2 = prepare_string(Cond2, C1),
            case evaluate(S2) of
                C1 -> true;
                false -> false
            end;
        false -> false
    end.

parse_cond(L) ->
    L1 = string:sub_string(L, 2, length(L) - 1),
    A = to_elem(L1),
    ?LOG("to_elem ~p --> ~p~n", [L1, A]),
    list_to_tuple(A).

to_elem([]) -> [];
%% to tuple
to_elem(L) when hd(L) == ${ ->
    ElemStr = string:sub_string(L, 2),
    {List, OtherStr} = to_elem(ElemStr),
    T = list_to_tuple(List),
    Other = to_elem(OtherStr),
    E1 = [T] ++ Other,
    E1;
to_elem(L) when hd(L) == $[ ->
    ElemStr = string:sub_string(L, 2),
    {List, OtherStr} = to_elem(ElemStr),
    Other = to_elem(OtherStr),
    Li = [List] ++ Other,
    Li;
to_elem(L) when hd(L) == $, ->
    L1 = string:sub_string(L, 2), to_elem(L1);
to_elem(L) ->
    Index1 = string:chr(L, $,),
    Index2 = string:chr(L, $}),
    Index3 = string:chr(L, $]),
    Index4 = string:chr(L, $|),
    if (Index4 /= 0) and (Index3 /= 0) and (Index1 == 0) ->
            ElemStr = string:sub_string(L, 1, Index3),
            OtherStr = string:sub_string(L, Index3 + 1),
            {list_to_atom(lists:concat(["[", ElemStr])), OtherStr};
       true -> to_elem(Index1, Index2, Index3, L)
    end.

to_elem(I1, I2, I3, L)
  when I2 /= 0, (I2 < I1) or (I1 == 0),
       (I2 < I3) or (I3 == 0) ->
    %% the element of the last element of the tuple
    ElemStr = string:sub_string(L, 1, I2 - 1),
    OtherStr = string:sub_string(L, I2 + 1),
    Elem = get_atom(ElemStr),
    {[Elem], OtherStr};
to_elem(I1, I2, I3, L)
  when I3 /= 0, (I3 < I1) or (I1 == 0),
       (I3 < I2) or (I2 == 0) ->
    %% the element of the last erement of the list
    ElemStr = string:sub_string(L, 1, I3 - 1),
    OtherStr = string:sub_string(L, I3 + 1),
    Elem = get_atom(ElemStr),
    {[Elem], OtherStr};
to_elem(I1, _I2, _I3, L) ->
    {ElemStr, OtherStr} =
        case I1 == 0 of
            false ->
                {string:sub_string(L, 1, I1 - 1),
                 string:sub_string(L, I1 + 1)};
            true ->
                %% The last element
                {L, []}
        end,
    Elem = [get_atom(ElemStr)],
    case to_elem(OtherStr) of
        {Other, Str} -> E = Elem ++ Other, {E, Str};
        Other -> A = Elem ++ Other, A
    end.
%% @doc Takes as input a string of L-type = "{elem1, elem1, elemM}
%% ..."  Returns the corresponding tuple ({elem1, elem2, elemN ...})
%% Transforming the variables into atoms (X-> 'X')
get_atom(X)
  when hd(X) == $' ->    %'
    L = length(X),
    case lists:nth(L, X) of
        39 ->
            Sub = string:sub_string(X, 2, L - 1), list_to_atom(Sub);
        _Other ->
            erlang:throw({eresye, {missing, element, X}})
    end;
get_atom(X) -> X1 = string:strip(X), list_to_atom(X1).

evaluate(String) ->
    ?LOG("FUN = ~p~n", [String]),
    {ok, Tokens, _} = erl_scan:string(String),
    {ok, Expr} = erl_parse:parse_exprs(Tokens),
    case catch erl_eval:exprs(Expr, erl_eval:new_bindings())
    of
        {'EXIT', _} -> false;
        {value, Value, _Bindings} -> Value;
        _ -> false
    end.

prepare_fun(_Cond, []) -> "nil.";
prepare_fun(Cond, [Cond1 | T1]) when hd(Cond) == ${ ->
    Str = lists:concat(["F=fun(", Cond, ",[", Cond1,
                        string_tail(T1, []), "])->true end."]),
    Str;
prepare_fun([Cond | T], [Cond1 | T1]) ->
    Str = lists:concat(["F=fun([", Cond, string_tail(T, []),
                        "],[", Cond1, string_tail(T1, []), "])->true end."]),
    Str.

string_tail([], Str) -> Str;
string_tail([C | OtherC], Str) ->
    Str1 = lists:concat([Str, ",", C]),
    string_tail(OtherC, Str1).

%% @doc join_node entering any new updates in the beta-token memory
update_new_node(EngineState0, Node, Parent_node, Join) ->
    case eresye_tree_list:is_root(Parent_node) of
        false ->
            Children = eresye_tree_list:children(Parent_node, Join),
            case Children -- [Node] of
                [] ->
                    case eresye_tree_list:get_key(Parent_node) of
                        {{n_node, _IdNp_node}, _Join_fun} ->
                            Beta = eresye_tree_list:get_beta(Parent_node),
                            update_from_n_node(EngineState0, Parent_node, Beta, Join);
                        {Tab, _} ->
                            Fact_list = ets:tab2list(Tab),
                            update_new_node(EngineState0, Node, Parent_node, Join, Fact_list)
                    end;
                [Child | _Other_child] ->
                    Beta = eresye_tree_list:get_beta(Child),
                    Join1 = eresye_tree_list:update_beta(Beta, Node, Join),
                    EngineState1 =
                        case eresye_tree_list:get_key(Node) of
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
    Node1 = eresye_tree_list:refresh(Node, Join1),
    update_new_node(EngineState1, Node1, Parent_node, Join1, Other_fact).

update_from_n_node(EngineState0, Parent_node, [], Join) ->
    Join1 = eresye_tree_list:update_node(Parent_node, Join),
    {Join1, EngineState0};
update_from_n_node(EngineState0, Parent_node, [Tok | OtherTok], Join) ->
    {Join1, EngineState1} = left_act(EngineState0, {Tok, plus}, [Parent_node],
                                     Join),
    update_from_n_node(Parent_node, OtherTok, Join1,
                       EngineState1).

signal(EngineState0 = #eresye{pending_actions=PAList}, Token, Sign, {Fun, Salience}) ->
    NewPA =
        case Sign of
            plus ->
                fun(EngineState1) ->
                        eresye_agenda:add_activation(EngineState1, Fun,
                                                     Token, Salience)
                end;
            minus ->
                ActivationId = eresye_agenda:get_activation(EngineState0,
                                                            {Fun, Token}),
                fun(EngineState1) ->
                        eresye_agenda:delete_activation(EngineState1, ActivationId)
                end
        end,
    EngineState0#eresye{pending_actions=PAList ++ [NewPA]}.

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
pass_fact(EngineState0 = #eresye{join=Join}, Tab, {Fact, Sign}) ->
    Succ_node_list = eresye_tree_list:lookup_all(Tab, Join),
    {Join1, EngineState1} = propagate(EngineState0, Succ_node_list,
                                      {Fact, Sign}, Join),
    EngineState1#eresye{join=Join1}.

propagate(EngineState0, [], {_Fact, _Sign}, Join) ->
    {Join, EngineState0};
propagate(EngineState0, [Join_node | T], {Fact, Sign}, Join) ->
    Join_node1 = eresye_tree_list:refresh(Join_node, Join),
    Tok_list = right_act({Fact, Sign}, Join_node1),
    {Join1, EngineState1} =
        case eresye_tree_list:get_key(Join_node1) of
            {{n_node, _}, _} ->
                propagate_nnode(EngineState0, Join_node1, Tok_list,
                                Sign, Join);
            {_Tab, _Fun} ->
                Children_list = eresye_tree_list:children(Join_node1,
                                                          Join),
                pass_tok(EngineState0, Tok_list, Children_list,
                         Join)
        end,
    propagate(EngineState1, T, {Fact, Sign}, Join1).

propagate_nnode(EngineState0, _Join_node, [], _, Join) ->
    {Join, EngineState0};
propagate_nnode(EngineState0, Join_node, Tok_list, Sign, Join) ->
    Children_list = eresye_tree_list:children(Join_node,
                                              Join),
    Toks = [make_toks(Tok, Sign) || Tok <- Tok_list],
    case Sign of
        plus -> pass_tok(EngineState0, Toks, Children_list, Join);
        minus ->
            {{n_node, IdNp_node}, Join_fun} =
                eresye_tree_list:get_key(Join_node),
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
    Beta = eresye_tree_list:get_beta(Join_node),
    Beta1 =
        case Sign of
            plus -> Beta ++ [Token];
            minus -> Beta -- [Token]
        end,
    Join1 = eresye_tree_list:update_beta(Beta1, Join_node,
                                         Join),
    case eresye_tree_list:get_key(Join_node) of
        {p_node, Rule} ->
            left_act(signal(EngineState0, Token, Sign, Rule),
                     {Token, Sign}, T, Join1);
        {{n_node, IdNp_node}, Join_fun} ->
            left_act_nnode(EngineState0, {Token, Sign}, IdNp_node, Join_fun,
                           [Join_node | T], Join1);
        {np_node, nil} ->
            Children_list = eresye_tree_list:children(Join_node,
                                                      Join1),
            {Join2, EngineState1} = propagate(EngineState0, Children_list,
                                              {Token, Sign}, Join1),
            left_act(EngineState1, {Token, Sign}, T, Join2);
        {Tab, Join_fun} ->
            Alfa_mem = ets:tab2list(Tab),
            Tok_list = join_left({Token, Sign}, Alfa_mem, Join_fun),
            Children_list = eresye_tree_list:children(Join_node,
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
    Np_node = eresye_tree_list:get_node(IdNp_node, Join),
    BetaN = eresye_tree_list:get_beta(Np_node),
    Tok_list = join_left({Token, Sign}, BetaN, Join_fun),
    case Tok_list of
        [] ->
            Children_list = eresye_tree_list:children(Join_node,
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
    test_nnode(OtherTok, IdNpNode, Join_fun, Join_node,
               Join1, EngineState1).


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
    Node1 = eresye_tree_list:refresh(Node, Join),
    L1 = [Node1 | L],
    refresh(OtherNode, Join, L1).

refresh(N, Join) -> refresh(N, Join, []).

%% @doc WME is the match between the token and Tok and returns an empty list in case
%% Negative or a new token (Tok + WME) if so
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
    Join_fun = evaluate(prepare_fun(Cond, P)),
    new_join(EngineState0, J, Tab, Join_fun, Parent_node);
make_join_node(EngineState0, J, {old, Tab}, Cond, P, Parent_node) ->
    Result = eresye_tree_list:child(Tab, Parent_node, J),
    case Result of
        false ->
            Join_fun = evaluate(prepare_fun(Cond, P)),
            new_join(EngineState0, J, Tab, Join_fun, Parent_node);
        Node -> {Node, J, EngineState0}
    end.

new_join(EngineState0, J, Tab, Join_fun, Parent_node) ->
    Key = {Tab, Join_fun},
    Value = [],
    {Node, J1} = eresye_tree_list:insert(Key, Value,
                                         Parent_node, J),
    {J2, EngineState1} = update_new_node(EngineState0, Node, Parent_node, J1),
    Node1 = eresye_tree_list:refresh(Node, J2),
    {Node1, J2, EngineState1}.
