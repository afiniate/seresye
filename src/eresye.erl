%%%  ERESYE, an ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
-module(eresye).

-behaviour(gen_server).

-ifdef(debug).
-define(LOG(F, X), io:format(F, X)).
-else.
-define(LOG(F, X), true).
-endif.

%%====================================================================
%% Include files
%%====================================================================

%%====================================================================
%% External exports
%%====================================================================

-export([add_rule/2, add_rule/3, assert/2, get_kb/1,
         get_ontology/1, get_rete/1, get_rules_fired/1,
         query_kb/2, remove_rule/2, retract/2, start/1, start/2,
         start_link/1, stop/1, wait/2, wait_and_retract/2]).

%%====================================================================
%% Internal exports
%%====================================================================

-export([code_change/3, handle_call/3, handle_cast/2,
         handle_info/2, init/1, terminate/2]).

%%====================================================================
%% External functions
%%====================================================================
start(EngineName) ->
    gen_server:start({local, EngineName}, ?MODULE,
                     [EngineName, nil], []).

start(EngineName, Ontology) ->
    gen_server:start({local, EngineName}, ?MODULE,
                     [EngineName, Ontology], []).

start_link(EngineName) ->
    gen_server:start_link({local, EngineName}, ?MODULE,
                          [EngineName, nil], []).

stop(EngineName) -> gen_server:call(EngineName, {stop}).

assert(EngineName, Fact) when is_list(Fact) ->
    [assert(EngineName, F) || F <- Fact], ok;
assert(EngineName, Fact) when is_tuple(Fact) ->
    gen_server:call(EngineName, {assert, Fact}).

retract(EngineName, Fact) when is_list(Fact) ->
    [retract(EngineName, F) || F <- Fact];
retract(EngineName, Fact) when is_tuple(Fact) ->
    gen_server:call(EngineName, {retract, Fact}).

add_rule(Name, Fun) -> add_rule(Name, Fun, 0).

add_rule(Name, {Module, Fun, ClauseID}, Salience) ->
    add_rule(Name, {Module, Fun}, ClauseID, Salience);
add_rule(Name, {Module, Fun}, Salience) ->
    add_rule(Name, {Module, Fun}, 0, Salience).

add_rule(Name, Fun, ClauseID, Salience) ->
    Ontology = get_ontology(Name),
    case get_conds(Fun, Ontology, ClauseID) of
        error -> error;
        CondsList ->
            lists:foreach(fun (X) ->
                                  case X of
                                      {error, Msg} ->
                                          erlang:throw({eresye, {error_adding_rule,
                                                                 [Fun, Msg]}});
                                      {PConds, NConds} ->
                                          ?LOG(">> PConds=~p~n", [PConds]),
                                          ?LOG(">> NConds=~p~n", [NConds]),
                                          gen_server:call(Name,
                                                          {add_rule,
                                                           {Fun, Salience},
                                                           {PConds, NConds}})
                                  end
                          end,
                          CondsList),
            ok
    end.

remove_rule(Name, Rule) ->
    gen_server:call(Name, {remove_rule, Rule}).

wait(Name, Pattern) ->
    wait_retract(Name, Pattern, false).

wait_and_retract(Name, Pattern) ->
    wait_retract(Name, Pattern, true).

wait_retract(Name, Pattern, NeedRetract)
  when is_tuple(Pattern) ->
    PList = tuple_to_list(Pattern),
    SList = [term_to_list(X) || X <- PList],
    FunList = [if is_function(X) -> X;
                  true -> fun (_) -> true end
               end
               || X <- PList],
    [_ | DList] = lists:foldl(fun (X, Sum) ->
                                      lists:concat([Sum, ",", X])
                              end,
                              [], SList),
    PidHash = erlang:phash2(self()),
    ClientCondition =
        lists:flatten(io_lib:format("{client, ~p, Pid, FunList}",
                                    [PidHash])),
    FinalPattern = [ClientCondition,
                    lists:concat(["{", DList, "}"])],
    RetractString = if NeedRetract ->
                            "eresye:retract (Engine, Pattern__),";
                       true -> ""
                    end,
    SFun =
        lists:flatten(io_lib:format("fun (Engine, {client, ~p, Pid, FunList} "
                                    "= X, Pattern__) -> FunPatPairs = lists:zip "
                                    "(FunList, tuple_to_list (Pattern__)), "
                                    "FunEval = [F(X) || {F, X} <- FunPatPairs], "
                                    "A = lists:foldr (fun (X,Y) -> X and "
                                    "Y end, true, FunEval), if A -> eresye:retract "
                                    "(Engine, X), ~s Pid ! Pattern__;    "
                                    "true -> nil end end.",
                                    [PidHash, RetractString])),
    Fun = evaluate(SFun),
    gen_server:call(Name,
                    {add_rule, {Fun, 0}, {FinalPattern, []}}),
    eresye:assert(Name, {client, PidHash, self(), FunList}),
    Pat = receive Pat0 -> Pat0 end,
    gen_server:call(Name, {remove_rule, Fun}),
    Pat.

term_to_list(X) when is_integer(X) ->
    integer_to_list(X);
term_to_list(X) when is_atom(X) -> atom_to_list(X);
term_to_list(X) when is_function(X) -> "_";
term_to_list(X) -> X.

get_ontology(Name) ->
    gen_server:call(Name, {get_ontology}).

get_rete(Name) -> gen_server:call(Name, {get_rete}).

get_rules_fired(Name) ->
    gen_server:call(Name, {get_rules_fired}).

get_kb(Name) ->
    [KB | _] = gen_server:call(Name, {get_rete}), KB.

query_kb(Name, Pattern) when is_tuple(Pattern) ->
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
    KB = [V1
          || V1 <- get_kb(Name), query_kb_1(V1, PatternSize)],
    lists:filter(MatchFunction, KB);
%%
query_kb(Name, F) when is_function(F) ->
    lists:filter(F, get_kb(Name)).

query_kb_1(X, PatternSize) -> size(X) == PatternSize.

%%====================================================================
%% Callback functions
%%====================================================================
init([EngineName, Ontology]) ->
    %% EngineState = [Kb, Alfa, Join, Agenda, State]
    EngineState = [[], [], eresye_tree_list:new(),
                   eresye_agenda:start(EngineName), Ontology],
    {ok, EngineState}.

handle_cast(_, State) -> {noreply, State}.

handle_info(_, State) -> {noreply, State}.

handle_call({stop}, _, State) ->
    {stop, normal, ok, State};
handle_call({assert, Fact}, _, State) ->
    NewState = assert_fact(State, Fact),
    {reply, ok, NewState};
handle_call({retract, Fact}, _, State) ->
    NewState = retract_fact(State, Fact),
    {reply, ok, NewState};
handle_call({add_rule, {Fun, Salience},
             {PConds, NConds}},
            _, State) ->
    Rule = {Fun, Salience},
    [_, _, Join | _] = State,
    Root = eresye_tree_list:get_root(Join),
    NewState =
        case NConds of
            [] ->
                make_struct(State, Rule, PConds, [], Root,
                            nil);
            _ ->
                {R1, Np_node} = make_struct(State, Rule, NConds, [],
                                            Root, ng),
                Root1 = eresye_tree_list:refresh(Root,
                                                 lists:nth(3, R1)),
                make_struct(R1, Rule, PConds, [], Root1,
                            {Np_node, NConds})
    end,
    {reply, ok, NewState};
handle_call({remove_rule, Fun}, _, EngineState) ->
    [Kb, Alfa, Join, Agenda, State] = EngineState,
    Agenda1 = eresye_agenda:deleteRule(Agenda, Fun),
    R1 = [Kb, Alfa, Join, Agenda1, State],
    R2 = remove_prod(R1, Fun),
    {reply, ok, R2};
handle_call({get_rules_fired}, _, State) ->
    [_, _, _, Agenda, _] = State,
    {reply, eresye_agenda:getRulesFired(Agenda), State};
handle_call({get_rete}, _, State) ->
    {reply, State, State};
handle_call({get_ontology}, _, State) ->
    [_, _, _, _, Ontology] = State,
    {reply, Ontology, State}.

%%====================================================================
%% Func: code_change/3
%%====================================================================
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%====================================================================
%% Func: terminate/2
%%====================================================================
terminate(_Reason, EngineState) ->
    [_, _, _, AgendaPid, _] = EngineState,
    gen_server:call(AgendaPid, {stop}),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

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

get_conds({Module, Func}, Ontology, ClauseID) ->
    File = lists:concat([Module, '.erl']),
    case epp:parse_file(File, ["."], []) of
        {error, OpenError} ->
            erlang:throw({eresye, {error_parsing_file,
                                   [{Module, Func}, OpenError]}}),
            error;
        {ok, Form} ->
            Records = get_records(Form, []),
            case search_fun(Form, Func, Records) of
                {error, Msg} ->
                    erlang:throw({eresye, {error_parsing_file,
                                           [{Module, Func}, Msg]}});
                {ok, CL} ->
                    ClauseList = if ClauseID > 0 ->
                                         [lists:nth(ClauseID, CL)];
                                    true -> CL
                                 end,
                    SolvedClauses = if Ontology == nil -> ClauseList;
                                       true ->
                                            eresye_ontology_resolver:resolve_ontology(ClauseList,
                                                                                      Ontology)
                                    end,
                    case read_clause(SolvedClauses, [], Records) of
                        {error, Msg2} ->
                            erlang:throw({eresye, {error_parsing_file,
                                                   clause_issues,
                                                   [{Module, Func}, Msg2]}}),
                            error;
                        CondsList -> CondsList
                    end
            end
    end.

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
make_struct(R, _Rule, [], _P, Cur_node, ng) ->
    [Kb, Alfa, Join, Agenda, State] = R,
    %% create production node
    Key = {np_node, nil},
    {Node, {Join2, Agenda1}} =
        case eresye_tree_list:child(Key, Cur_node, Join) of
            false ->
                Value = [],
                {Node0, Join1} = eresye_tree_list:insert(Key, Value,
                                                         Cur_node, Join),
                {Node0, update_new_node(Node0, Cur_node,
                                        Join1, Agenda)};
            Node0 -> {Node0, {Join, Agenda}}
        end,
    {[Kb, Alfa, Join2, Agenda1, State], Node};
make_struct(R, Rule, [], _P, Cur_node, nil) ->
    [Kb, Alfa, Join, Agenda, State] = R,
    %% create production node
    Key = {p_node, Rule},
    {Join2, Agenda1} = case eresye_tree_list:child(Key, Cur_node, Join) of
                           false ->
                               Value = [],
                               {Node, Join1} = eresye_tree_list:insert(Key, Value,
                                                                       Cur_node, Join),
                               update_new_node(Node, Cur_node,
                                               Join1, Agenda);
                           Node ->
                               {Fun, _Salience} = Rule,
                               Key1 = element(1, Node),
                               Sal = element(2, element(2, Key1)),
                               io:format(">> Rule (~w) already present ~n", [Fun]),
                               io:format(">> with salience = ~w~n", [Sal]),
                               io:format(">> To change salience use 'set_salience()'.~n"),
                               {Join, Agenda}
                       end,
    [Kb, Alfa, Join2, Agenda1, State];
make_struct(R, Rule, [], P, Cur_node, {Nod, NConds}) ->
    [Kb, Alfa, Join, Agenda, State] = R,
    Id = eresye_tree_list:get_id(Nod),
    {Cur_node1, Join1, Agenda1} = make_join_node(Join,
                                                 {old, {n_node, Id}}, NConds, P,
                                                 Cur_node, Agenda),
    Nod1 = eresye_tree_list:refresh(Nod, Join1),
    Join2 = eresye_tree_list:set_child(Cur_node1, Nod1,
                                       Join1),
    Key = {p_node, Rule},
    {Join4, Agenda2} =
        case eresye_tree_list:child(Key, Cur_node1, Join2) of
            false ->
                Value = [],
                {Node, Join3} = eresye_tree_list:insert(Key, Value,
                                                        Cur_node1, Join2),
                Cur_node2 = eresye_tree_list:refresh(Cur_node1, Join3),
                update_new_node(Node, Cur_node2,
                                Join3, Agenda1);
            Node ->
                {Fun, _Salience} = Rule,
                Key1 = element(1, Node),
                Sal = element(2, element(2, Key1)),
                io:format(">> Rule (~w) already present ~n", [Fun]),
                io:format(">> with salience = ~w~n", [Sal]),
                io:format(">> To change salience use 'set_salience()'.~n"),
                {Join2, Agenda1}
        end,
    [Kb, Alfa, Join4, Agenda2, State];
make_struct(R, Rule, [Cond | T], P, Cur_node, Nod) ->
    [Kb, _Alfa, Join, Agenda, State] = R,
    {Alfa1, Tab} = add_alfa(R, Cond),
    {Cur_node1, Join1, Agenda1} = make_join_node(Join, Tab,
                                                 Cond, P, Cur_node, Agenda),
    P1 = P ++ [Cond],
    make_struct([Kb, Alfa1, Join1, Agenda1, State], Rule, T,
                P1, Cur_node1, Nod).

add_alfa(R, Cond) ->
    [Kb, Alfa, _Join, _, _] = R,
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

remove_prod(R, Fun) ->
    [Kb, Alfa, Join, Agenda, State] = R,
    case eresye_tree_list:keysearch({p_node, Fun}, Join) of
        false -> R;
        Node ->
            Parent_node = eresye_tree_list:get_parent(Node, Join),
            Join1 = eresye_tree_list:remove_node(Node, Join),
            Parent_node1 = eresye_tree_list:refresh(Parent_node,
                                                    Join1),
            R1 = remove_nodes(Parent_node1,
                              [Kb, Alfa, Join1, Agenda, State]),
            remove_prod(R1, Fun)
    end.

remove_nodes(Node, R) ->
    [Kb, Alfa, Join, Agenda, State] = R,
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
                            R1 = [Kb, Alfa, Join2, Agenda, State],
                            Np_node1 = eresye_tree_list:refresh(Np_node, Join2),
                            R2 = remove_nodes(Np_node1, R1),
                            %% Recovery of the parent node of the node passed as an argument n_node
                            %% The parent can now have a different id, but has the same key
                            Join3 = lists:nth(3, R2),
                            Parent_node2 = eresye_tree_list:keysearch(ParentKey,
                                                                      Join3),
                            remove_nodes(Parent_node2, R2);
                        np_node ->
                            R1 = [Kb, Alfa, Join1, Agenda, State],
                            remove_nodes(Parent_node1, R1);
                        Tab ->
                            Alfa1 = case eresye_tree_list:is_present(Tab, Join1) of
                                        false ->
                                            ets:delete(Tab),
                                            lists:keydelete(Tab, 2, Alfa);
                                        true ->
                                            Alfa
                                    end,
                            R1 = [Kb, Alfa1, Join1, Agenda, State],
                            remove_nodes(Parent_node1, R1)
                    end;
                true -> R
            end;
        true -> R
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
update_new_node(Node, Parent_node, Join, Agenda) ->
    case eresye_tree_list:is_root(Parent_node) of
        false ->
            Children = eresye_tree_list:children(Parent_node, Join),
            case Children -- [Node] of
                [] ->
                    case eresye_tree_list:get_key(Parent_node) of
                        {{n_node, _IdNp_node}, _Join_fun} ->
                            Beta = eresye_tree_list:get_beta(Parent_node),
                            update_from_n_node(Parent_node, Beta, Join, Agenda);
                        {Tab, _} ->
                            Fact_list = ets:tab2list(Tab),
                            update_new_node(Node, Parent_node, Join, Fact_list,
                                            Agenda)
                    end;
                [Child | _Other_child] ->
                    Beta = eresye_tree_list:get_beta(Child),
                    Join1 = eresye_tree_list:update_beta(Beta, Node, Join),
                    Agenda1 =
                        case eresye_tree_list:get_key(Node) of
                            {p_node, Rule} ->
                                update_agenda(Agenda, Beta, Rule);
                            _Key ->
                                Agenda
                        end,
                    {Join1, Agenda1}
            end;
        true -> {Join, Agenda}
    end.

update_agenda(Agenda, [], _Rule) -> Agenda;
update_agenda(Agenda, [Tok | OtherTok], Rule) ->
    Agenda1 = signal(Tok, plus, Rule, Agenda),
    update_agenda(Agenda1, OtherTok, Rule).

update_new_node(_Node, _Parent_node, Join, [], Agenda) ->
    {Join, Agenda};
update_new_node(Node, Parent_node, Join,
                [Fact | Other_fact], Agenda) ->
    Tok_list = right_act({Fact, plus}, Parent_node),
    {Join1, Agenda1} = pass_tok(Tok_list, [Node], Join,
                                Agenda),
    Node1 = eresye_tree_list:refresh(Node, Join1),
    update_new_node(Node1, Parent_node, Join1, Other_fact,
                    Agenda1).

update_from_n_node(Parent_node, [], Join, Agenda) ->
    Join1 = eresye_tree_list:update_node(Parent_node, Join),
    {Join1, Agenda};
update_from_n_node(Parent_node, [Tok | OtherTok], Join,
                   Agenda) ->
    {Join1, Agenda1} = left_act({Tok, plus}, [Parent_node],
                                Join, Agenda),
    update_from_n_node(Parent_node, OtherTok, Join1,
                       Agenda1).

signal(Token, Sign, {Fun, Salience}, Agenda) ->
    case Sign of
        plus ->
            eresye_agenda:addActivation(Agenda, Fun,
                                        [self(), Token], Salience);
        minus ->
            ActivationId = eresye_agenda:getActivation(Agenda,
                                                       {Fun, [self(), Token]}),
            eresye_agenda:deleteActivation(Agenda, ActivationId)
    end.

%%====================================================================
%% Fact Assertion/Retraction Functions
%%====================================================================

%% @doc Insert a fact in the KB.
%% It also checks if the fact verifies any condition,
%% if this is the case the fact is also inserted in the alpha-memory
assert_fact(R, Fact) ->
    [Kb, Alfa, Join, Agenda, State] = R,
    case lists:member(Fact, Kb) of
        false ->
            Kb1 = [Fact | Kb],
            check_cond([Kb1, Alfa, Join, Agenda, State], Alfa,
                       {Fact, plus});
        true -> R
    end.
%% @doc removes a 'fact' in the Knowledge Base and if something occurs
%% Condition is also deleted from the corresponding alpha-memory
retract_fact(R, Fact) ->
    [Kb, Alfa, Join, Agenda, State] = R,
    case lists:member(Fact, Kb) of
        true ->
            Kb1 = Kb -- [Fact],
            check_cond([Kb1, Alfa, Join, Agenda, State], Alfa,
                       {Fact, minus});
        false -> R
    end.

check_cond(R, [], {_Fact, _Sign}) -> R;
check_cond(R, [{_C1, Tab, Alfa_fun} | T],
           {Fact, Sign}) ->
    case catch Alfa_fun(Fact) of
        true ->
            case Sign of
                plus -> ets:insert(Tab, Fact);
                minus -> ets:delete_object(Tab, Fact)
            end,
            R1 = pass_fact(R, Tab, {Fact, Sign}),
            check_cond(R1, T, {Fact, Sign});
        {'EXIT', {function_clause, _}} ->
            check_cond(R, T, {Fact, Sign});
        _Other -> check_cond(R, T, {Fact, Sign})
    end.

%% @doc propagates the 'done' to all the nodes that follow the alpha-memory
%% With an index tab
pass_fact(R, Tab, {Fact, Sign}) ->
    [Kb, Alfa, Join, Agenda, State] = R,
    Succ_node_list = eresye_tree_list:lookup_all(Tab, Join),
    {Join1, Agenda1} = propagate(Succ_node_list,
                                 {Fact, Sign}, Join, Agenda),
    [Kb, Alfa, Join1, Agenda1, State].

propagate([], {_Fact, _Sign}, Join, Agenda) ->
    {Join, Agenda};
propagate([Join_node | T], {Fact, Sign}, Join,
          Agenda) ->
    Join_node1 = eresye_tree_list:refresh(Join_node, Join),
    Tok_list = right_act({Fact, Sign}, Join_node1),
    {Join1, Agenda1} =
        case eresye_tree_list:get_key(Join_node1) of
            {{n_node, _}, _} ->
                propagate_nnode(Join_node1, Tok_list,
                                Sign, Join, Agenda);
            {_Tab, _Fun} ->
                Children_list = eresye_tree_list:children(Join_node1,
                                                          Join),
                pass_tok(Tok_list, Children_list,
                         Join, Agenda)
        end,
    propagate(T, {Fact, Sign}, Join1, Agenda1).

propagate_nnode(_Join_node, [], _, Join, Agenda) ->
    {Join, Agenda};
propagate_nnode(Join_node, Tok_list, Sign, Join,
                Agenda) ->
    Children_list = eresye_tree_list:children(Join_node,
                                              Join),
    Toks = [propagate_nnode_1(V1, Sign) || V1 <- Tok_list],
    case Sign of
        plus -> pass_tok(Toks, Children_list, Join, Agenda);
        minus ->
            {{n_node, IdNp_node}, Join_fun} =
                eresye_tree_list:get_key(Join_node),
            test_nnode(Toks, IdNp_node, Join_fun, Join_node, Join,
                       Agenda)
    end.

propagate_nnode_1(Tk, Sign) ->
    L = element(1, Tk),
    T1 = lists:sublist(L, length(L) - 1),
    case Sign of
        plus -> {T1, minus};
        minus -> {T1, plus}
    end.
%% @doc Token propagates all the nodes in Children_list
pass_tok([], _Children_list, Join, Agenda) ->
    {Join, Agenda};
pass_tok([Tok | T], Children_list, Join, Agenda) ->
    {Join1, Agenda1} = left_act(Tok, Children_list, Join,
                                Agenda),
    Children_list1 = refresh(Children_list, Join1),
    pass_tok(T, Children_list1, Join1, Agenda1).

%% @doc Insert the token in Tok Beta_memory of Join_node, Token Tok
%% compares the present with all the facts nell'alfa memory Associated
%% with Join_node and if you have a successful propagates Token to the
%% child nodes
left_act({_Token, _Sign}, [], Join, Agenda) ->
    {Join, Agenda};
left_act({Token, Sign}, [Join_node | T], Join,
         Agenda) ->
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
            Agenda1 = signal(Token, Sign, Rule, Agenda),
            left_act({Token, Sign}, T, Join1, Agenda1);
        {{n_node, IdNp_node}, Join_fun} ->
            left_act_nnode({Token, Sign}, IdNp_node, Join_fun,
                           [Join_node | T], Join1, Agenda);
        {np_node, nil} ->
            Children_list = eresye_tree_list:children(Join_node,
                                                      Join1),
            {Join2, Agenda1} = propagate(Children_list,
                                         {Token, Sign}, Join1, Agenda),
            left_act({Token, Sign}, T, Join2, Agenda1);
        {Tab, Join_fun} ->
            Alfa_mem = ets:tab2list(Tab),
            Tok_list = join_left({Token, Sign}, Alfa_mem, Join_fun),
            Children_list = eresye_tree_list:children(Join_node,
                                                      Join1),
            {Join2, Agenda1} = pass_tok(Tok_list, Children_list,
                                        Join1, Agenda),
            left_act({Token, Sign}, T, Join2, Agenda1)
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

left_act_nnode({Token, Sign}, IdNp_node, Join_fun,
               [Join_node | T], Join, Agenda) ->
    Np_node = eresye_tree_list:get_node(IdNp_node, Join),
    BetaN = eresye_tree_list:get_beta(Np_node),
    Tok_list = join_left({Token, Sign}, BetaN, Join_fun),
    case Tok_list of
        [] ->
            Children_list = eresye_tree_list:children(Join_node,
                                                      Join),
            {Join1, Agenda1} = pass_tok([{Token, Sign}],
                                        Children_list, Join, Agenda),
            left_act({Token, Sign}, T, Join1, Agenda1);
        Tok_list -> left_act({Token, Sign}, T, Join, Agenda)
    end.

test_nnode([], _, _, _, Join, Agenda) -> {Join, Agenda};
test_nnode([Tok | OtherTok], IdNpNode, Join_fun,
           Join_node, Join, Agenda) ->
    {Join1, Agenda1} = left_act_nnode(Tok, IdNpNode,
                                      Join_fun, [Join_node], Join, Agenda),
    test_nnode(OtherTok, IdNpNode, Join_fun, Join_node,
               Join1, Agenda1).


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
make_join_node(J, {new, Tab}, Cond, P, Parent_node,
               Agenda) ->
    Join_fun = evaluate(prepare_fun(Cond, P)),
    new_join(J, Tab, Join_fun, Parent_node, Agenda);
make_join_node(J, {old, Tab}, Cond, P, Parent_node,
               Agenda) ->
    Result = eresye_tree_list:child(Tab, Parent_node, J),
    case Result of
        false ->
            Join_fun = evaluate(prepare_fun(Cond, P)),
            new_join(J, Tab, Join_fun, Parent_node, Agenda);
        Node -> {Node, J, Agenda}
    end.

new_join(J, Tab, Join_fun, Parent_node, Agenda) ->
    Key = {Tab, Join_fun},
    Value = [],
    {Node, J1} = eresye_tree_list:insert(Key, Value,
                                         Parent_node, J),
    {J2, Agenda1} = update_new_node(Node, Parent_node, J1,
                                    Agenda),
    Node1 = eresye_tree_list:refresh(Node, J2),
    {Node1, J2, Agenda1}.
