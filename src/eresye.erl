%
% eresye.erl
%
% ----------------------------------------------------------------------
%
%  ERESYE, an ERlang Expert SYstem Engine
%  Copyright (C) 2005-07 Francesca Gangemi (francesca@erlang-consulting.com)
%  Copyright (C) 2005-07 Corrado Santoro (csanto@diit.unict.it)
%
%  This program is free software: you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation, either version 3 of the License, or
%  (at your option) any later version.
%
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%
%  You should have received a copy of the GNU General Public License
%  along with this program.  If not, see <http://www.gnu.org/licenses/>
%
%
-module (eresye).

-behaviour (gen_server).

%%====================================================================
%% Include files
%%====================================================================

%%====================================================================
%% External exports
%%====================================================================

-export ([start/1,
          start/2,
          start_link/1,
          stop/1,
          assert/2,
          retract/2,
          add_rule/2,
          add_rule/3,
          remove_rule/2,
          wait/2,
          wait_and_retract/2,
          get_rete/1,
          get_ontology/1,
          get_kb/1,
          get_rules_fired/1,
          query_kb/2]).

%%====================================================================
%% Internal exports
%%====================================================================

-export ([init/1,
          handle_call/3,
          terminate/2,
          code_change/3]).

%%====================================================================
%% External functions
%%====================================================================
%%====================================================================
%% Func: start/1
%% Arguments: Engine Name
%%====================================================================
start (EngineName) ->
  gen_server:start ({local, EngineName}, ?MODULE, [EngineName, nil], []).

%%====================================================================
%% Func: start/2
%% Arguments: Engine Name, Ontology
%%====================================================================
start (EngineName, Ontology) ->
  gen_server:start ({local, EngineName}, ?MODULE, [EngineName, Ontology], []).

%%====================================================================
%% Func: start_link/1
%% Arguments: Engine Name
%%====================================================================
start_link (EngineName) ->
  gen_server:start_link ({local, EngineName}, ?MODULE,
                         [EngineName, nil], []).


%%====================================================================
%% Func: stop/1
%% Arguments: Engine Name
%%====================================================================
stop (EngineName) ->
  gen_server:call (EngineName, {stop}).


%%====================================================================
%% Func: assert/2
%% Arguments: EngineName, FactToAssert or FactList
%%====================================================================
assert (EngineName, Fact) when is_list (Fact) ->
  [assert (EngineName, F) || F <- Fact],
  ok;
assert (EngineName, Fact) when is_tuple (Fact) ->
  gen_server:call (EngineName, {assert, Fact}).



%%====================================================================
%% Func: retract/2
%% Arguments: EngineName, FactToRetract or FactList
%%====================================================================
retract (EngineName, Fact) when is_list (Fact) ->
  [retract (EngineName, F) || F <- Fact];
retract (EngineName, Fact) when is_tuple (Fact) ->
  gen_server:call (EngineName, {retract, Fact}).



%%====================================================================
%% Func: add_rule/2
%% Arguments: EngineName, Rule
%%====================================================================
add_rule (Name, Fun) ->
  add_rule (Name, Fun, 0).


%%====================================================================
%% Func: add_rule/3
%% Arguments: EngineName, Rule, Salience
%%====================================================================
add_rule (Name, {Module, Fun, ClauseID}, Salience) ->
  add_rule (Name, {Module, Fun}, ClauseID, Salience);
add_rule (Name, {Module, Fun}, Salience) ->
  add_rule (Name, {Module, Fun}, 0, Salience).

add_rule (Name, Fun, ClauseID, Salience) ->
  Ontology = get_ontology (Name),
  case get_conds (Fun, Ontology, ClauseID) of
    error -> error;
    CondsList ->
      lists:foreach (
        fun (X)->
            case X of
              {error, Msg} ->
                io:format(">> Errore!!!~n~w:~s~n",[Fun, Msg]);
              {PConds, NConds} ->
                %%io:format(">> PConds=~p~n",[PConds]),
                %%io:format(">> NConds=~p~n",[NConds]),
                gen_server:call (Name, {add_rule,
                                        {Fun, Salience},
                                        {PConds, NConds}})
            end
        end, CondsList),
      ok
  end.


%%====================================================================
%% Func: remove_rule/2
%% Arguments: EngineName, Rule
%%====================================================================
remove_rule (Name, Rule) ->
  gen_server:call (Name, {remove_rule, Rule}).


%%====================================================================
%% Func: wait/2
%% Arguments: EngineName, PatternToMatch
%%====================================================================
wait (Name, Pattern) ->
  wait_retract (Name, Pattern, false).

%%====================================================================
%% Func: wait_and_retract/2
%% Arguments: EngineName, PatternToMatch
%%====================================================================
wait_and_retract (Name, Pattern) ->
  wait_retract (Name, Pattern, true).


wait_retract (Name, Pattern, NeedRetract) when is_tuple (Pattern) ->
  PList = tuple_to_list (Pattern),
  SList = [term_to_list (X) || X <- PList],
  FunList = [if
               is_function (X) -> X;
               true -> fun (_) -> true end
             end || X <- PList],
  %%io:format ("SList = ~p~n", [SList]),
  [_ | DList] = lists:foldl (fun (X, Sum) ->
                                 lists:concat ([Sum, ",", X])
                             end,
                             [], SList),
  PidHash = erlang:phash2 (self ()),
  ClientCondition = lists:flatten (
                      io_lib:format ("{client, ~p, Pid, FunList}", [PidHash])
                     ),
  FinalPattern = [ClientCondition,
                  lists:concat (["{", DList, "}"])],
  RetractString =
    if
      NeedRetract -> "eresye:retract (Engine, Pattern__),";
      true -> ""
    end,
  SFun = lists:flatten (
           io_lib:format (
             "fun (Engine, {client, ~p, Pid, FunList} = X, Pattern__) -> "
             "FunPatPairs = lists:zip (FunList, tuple_to_list (Pattern__)), "
             "FunEval = [F(X) || {F, X} <- FunPatPairs], "
             "A = lists:foldr (fun (X,Y) -> X and Y end, true, FunEval), "
             "if A -> eresye:retract (Engine, X), ~s Pid ! Pattern__; "
             "   true -> nil end "
             "end.", [PidHash, RetractString])),
  %%io:format ("Fun = ~p~n", [SFun]),
  %%io:format ("Pattern = ~p~n", [FinalPattern]),
  Fun = evaluate (SFun),
  gen_server:call (Name, {add_rule,
                          {Fun, 0},
                          {FinalPattern, []}}),
  %%io:format ("Rete is ~p~n", [eresye:get_rete (Name)]),
  eresye:assert (Name, {client, PidHash, self (), FunList}),
  receive
    Pat -> Pat
  end,
%%   if
%%     NeedRetract -> eresye:retract (Name, Pat);
%%     true -> ok
%%   end,
  gen_server:call (Name, {remove_rule, Fun}),
  Pat.


term_to_list (X) when is_integer (X) -> integer_to_list (X);
term_to_list (X) when is_atom (X) -> atom_to_list (X);
term_to_list (X) when is_function (X) -> "_";
term_to_list (X) -> X.


%%====================================================================
%% Func: get_ontology/1
%% Arguments: EngineName
%%====================================================================
get_ontology (Name) ->
  gen_server:call (Name, {get_ontology}).


%%====================================================================
%% Func: get_rete/1
%% Arguments: EngineName
%%====================================================================
get_rete (Name) ->
  gen_server:call (Name, {get_rete}).


%%====================================================================
%% Func: get_rules_fired/1
%% Arguments: EngineName
%%====================================================================
get_rules_fired (Name) ->
  gen_server:call (Name, {get_rules_fired}).


%%====================================================================
%% Func: get_kb/1
%% Arguments: EngineName
%%====================================================================
get_kb (Name) ->
  [KB | _] = gen_server:call (Name, {get_rete}),
  KB.


%%====================================================================
%% Func: query_kb/2
%% Arguments: EngineName, Pattern
%%====================================================================
query_kb (Name, Pattern) when is_tuple (Pattern) ->
  PList = tuple_to_list (Pattern),
  PatternSize = length (PList),
  FunList = [if
               is_function (X) -> X;
               X == '_' -> fun (_) -> true end;
               true -> fun (Z) -> Z == X end
             end || X <- PList],
  MatchFunction = fun (P) ->
                      FunPatPairs = lists:zip (FunList, tuple_to_list (P)),
                      FunEval = [F(X) || {F, X} <- FunPatPairs],
                      MatchResult = lists:foldr (fun (X, Y) ->
                                                     X and Y
                                                 end,
                                                 true,
                                                 FunEval),
                      MatchResult
                  end,
  KB = lists:filter (fun (X) ->
                         size (X) == PatternSize
                     end, get_kb (Name)),
  lists:filter (MatchFunction, KB);
%%
query_kb (Name, F) when is_function (F) ->
  lists:filter (F, get_kb (Name)).


%%====================================================================
%% Callback functions
%%====================================================================
%%====================================================================
%% Func: init/1
%%====================================================================
init ([EngineName, Ontology]) ->
  %% EngineState = [Kb, Alfa, Join, Agenda, State]
  EngineState = [ [],
                  [],
                  eresye_tree_list:new (),
                  eresye_agenda:start (EngineName),
                  Ontology ],
  {ok, EngineState}.


%%====================================================================
%% Func: handle_call/3
%%====================================================================
handle_call ({stop}, _, State) ->
  {stop, normal, ok, State};

handle_call ({assert, Fact}, _, State) ->
  NewState = assert_fact (State, Fact),
  {reply, ok, NewState};

handle_call ({retract, Fact}, _, State) ->
  NewState = retract_fact (State, Fact),
  {reply, ok, NewState};

handle_call ({add_rule, {Fun, Salience}, {PConds, NConds}}, _, State) ->
  %%io:format ("Addrule ~p: ~p~n", [Fun, {PConds, NConds}]),
  Rule = {Fun, Salience},
  [_, _, Join | _] = State,
  Root = eresye_tree_list:get_root (Join),
  case NConds of
    [] ->
      NewState = make_struct (State, Rule, PConds, [], Root, nil);
    _ ->
      {R1, Np_node} = make_struct (State, Rule, NConds, [], Root, ng),
      Root1 = eresye_tree_list:refresh (Root, lists:nth (3, R1)),
      NewState = make_struct (R1, Rule, PConds, [], Root1, {Np_node, NConds})
  end,
  {reply, ok, NewState};

handle_call ({remove_rule, Fun}, _, EngineState) ->
  [Kb, Alfa, Join, Agenda, State] = EngineState,
  Agenda1 = eresye_agenda:deleteRule (Agenda, Fun),
  R1 = [Kb, Alfa, Join, Agenda1, State],
  R2 = remove_prod (R1, Fun),
  {reply, ok, R2};

handle_call ({get_rules_fired}, _, State) ->
  [_, _, _, Agenda, _] = State,
  {reply, eresye_agenda:getRulesFired (Agenda), State};

handle_call ({get_rete}, _, State) ->
  {reply, State, State};

handle_call ({get_ontology}, _, State) ->
  [_, _, _, _, Ontology] = State,
  {reply, Ontology, State}.

%%====================================================================
%% Func: code_change/3
%%====================================================================
code_change (OldVsn, State, Extra) -> {ok, State}.

%%====================================================================
%% Func: terminate/2
%%====================================================================
terminate (Reason, EngineState) ->
  [ _, _, _, AgendaPid, _] = EngineState,
  gen_server:call (AgendaPid, {stop}),
  %%io:format ("Terminating...~p~n", [self ()]),
  ok.


%%====================================================================
%% Internal functions
%%====================================================================

% inizializza la nuova alfa-memory con i fatti presenti
% nella Knowledge Base che soddisfano la condizione Cond
initialize_alfa (_, _, []) ->
    nil;
initialize_alfa (Cond, Tab, [Fact | Other_fact]) ->
  %%io:format ("ALPHA ~p,~p~n", [Cond, Fact]),
  Fun = prepare_match_alpha_fun (Cond),
  %%io:format ("Fun ~p~n", [Fun]),
  case Fun (Fact) of
    Fact ->
      ets:insert (Tab, Fact),
      initialize_alfa (Cond, Tab, Other_fact);
    false ->
      initialize_alfa (Cond, Tab, Other_fact)
  end.


%% initialize_alfa (Cond, Tab, [Fact | Other_fact]) ->
%%   io:format ("ALPHA ~p,~p~n", [Cond, Fact]),
%%   Str = prepare_string (Cond, Fact),
%%   io:format ("STR ~p~n", [Str]),
%%   case evaluate (Str) of
%%     Fact ->
%%       ets:insert (Tab, Fact),
%%       initialize_alfa (Cond, Tab, Other_fact);
%%     false ->
%%       initialize_alfa (Cond, Tab, Other_fact)
%%   end.



prepare_match_alpha_fun (Cond) ->
  FunString = lists:flatten (io_lib:format ("fun (~s = X__x__X) -> X__x__X;"
                                            "    (_)  -> false end.",
                                            [Cond])),
  %%io:format ("Fun String ~p~n", [FunString]),
  evaluate (FunString).


get_conds ({Module, Func}, Ontology, ClauseID) ->
  File = lists:concat ([Module, '.erl']),
  case epp:parse_file (File, ["."], []) of
    {error, OpenError} ->
      io:format(">> Errore!!!~n~w:~w~n", [{Module, Func}, OpenError]),
      error;
    {ok, Form} ->
      Records = get_records (Form, []),
      %%io:format (">> Records ~p~n", [Records]),
      case search_fun (Form, Func, Records) of
        {error, Msg} ->
          io:format(">> Errore!!!~n~w:~s~n",[{Module,Func}, Msg]),
          error;
        {ok, CL} ->
          ClauseList =
            if
              ClauseID > 0 -> [lists:nth (ClauseID, CL)];
              true -> CL
            end,
          %%io:format ("Clauses ~p~n", [ClauseList]),
          SolvedClauses =
            if
              Ontology == nil -> ClauseList;
              true -> eresye_ontology_resolver:resolve_ontology (ClauseList,
                                                                 Ontology)
            end,
          %%io:format (">>> ~p~n", [SolvedClauses]),
          case read_clause(SolvedClauses, [], Records) of
            {error, Msg2} ->
              io:format(">> Errore!!!~n~w:~s~n",[{Module,Func}, Msg2]),
              error;
            CondsList -> CondsList
          end
      end
  end.


get_records ([], Acc) -> lists:reverse (Acc);
get_records ([{attribute, _, record, {RecordName, RecordFields}} | Tail],
             Acc) ->
  NewAcc =  [{RecordName, get_record_fields (RecordFields, [])} | Acc],
  get_records (Tail, NewAcc);
get_records ([_ | Tail], Acc) ->
  get_records (Tail, Acc).


get_record_fields ([], Acc) -> lists:reverse (Acc);
get_record_fields ([{record_field, _,
                     {atom, _, FieldName}, {atom, _, DefaultValue}} | Tail],
                   Acc) ->
  NewAcc = [{FieldName, DefaultValue} | Acc],
  get_record_fields (Tail, NewAcc);
get_record_fields ([{record_field, _, {atom, _, FieldName}} | Tail],
                   Acc) ->
  NewAcc = [{FieldName} | Acc],
  get_record_fields (Tail, NewAcc).


search_fun ([], _, RecordList) ->
  {error,"Funzione non trovata"};
search_fun ([{function, _, Func, _, ClauseList} | Other], Func, RecordList) ->
  {ok, ClauseList};
% read_clause(ClauseList, [], RecordList);
search_fun ([Tuple | Other], Func, RecordList) ->
  search_fun (Other, Func, RecordList).



read_clause([], CondsList, RecordList)->
  CondsList;
read_clause([Clause | OtherClause], CondsList, RecordList) ->
  {clause, _, ParamList, GuardList, _} = Clause,
  %%PosConds = read_param (ParamList),
  PosConds = read_parameters (ParamList, RecordList),
  %%io:format (">>> Positive conditions : ~p~n", [PosConds]),
  NegConds = read_guard (GuardList),
  CondsList1 = lists:append (CondsList,[{PosConds, NegConds}]),
  read_clause (OtherClause, CondsList1, RecordList).

read_guard ([]) ->
  [];
read_guard ([[{op,_,'not', Conds}|_] | OtherGuard]) ->
  Nc = get_neg_cond(Conds, []),
  lists:reverse(Nc);
read_guard (Guard) ->
  [].

get_neg_cond ({nil,_}, Nc) ->
  Nc;
get_neg_cond ({cons, _, {string, _, C}, OtherC}, Nc) ->
  Nc1 = [C | Nc],
  get_neg_cond(OtherC, Nc1);
get_neg_cond (X, Nc) ->
  io:format(">> Negated Conditions must be a list of String~n"),
  [].


read_parameters ([{var, _, _} | Tail], RecordList) ->
  P = extract_parameters (Tail, [], RecordList),
  %%io:format ("read_parameters = ~p~n", [P]),
  Conditions = [ build_string_condition (X, []) || X <- P],
  Conditions.




extract_parameters ([], Acc, RecordList) ->
  lists:reverse (Acc);

extract_parameters ([{match, _, {tuple, _, Condition}, {var, _, _}} | Tail],
                    Acc, RecordList) ->
  extract_parameters (Tail, [Condition | Acc], RecordList);

extract_parameters ([{match, _, {var, _, _}, {tuple, _, Condition}} | Tail],
                    Acc, RecordList) ->
  extract_parameters (Tail, [Condition | Acc], RecordList);

extract_parameters ([{match, _, {record, _, _, _} = R, {var, _, _}} | Tail],
                    Acc, RecordList) ->
  extract_parameters ([R | Tail], Acc, RecordList);

extract_parameters ([{match, _, {var, _, _}, {record, _, _, _} = R} | Tail],
                    Acc, RecordList) ->
  extract_parameters ([R | Tail], Acc, RecordList);

extract_parameters ([{record, _, RecordName, Condition} | Tail],
                    Acc, RecordList) ->
  %%io:format ("Record: ~p~nCondition: ~p~n", [RecordName, Condition]),
  RecordDefinition = get_record_def (RecordName, RecordList),
  %%io:format ("Record Definition: ~p~n", [RecordDefinition]),
  RecordDefaults = make_record_default (RecordDefinition, []),
  %%io:format ("Record Defaults: ~p~n", [RecordDefaults]),
  Pattern = [{atom, 0, RecordName} |
             make_record_pattern (Condition, RecordDefaults,
                                  RecordDefinition)],
  %%io:format ("Record Pattern: ~p~n", [Pattern]),
  extract_parameters (Tail, [Pattern | Acc], RecordList);

extract_parameters ([{tuple, _, Condition} | Tail], Acc, RecordList) ->
  extract_parameters (Tail, [Condition | Acc], RecordList).





get_record_def (_, []) -> nil;
get_record_def (Name, [{Name, Definition} | Rest]) -> Definition;
get_record_def (Name, [_ | Rest]) -> get_record_def (Name, Rest).


make_record_default ([], Acc) ->
  lists:reverse (Acc);
make_record_default ([{_} | Tail], Acc) ->
  make_record_default (Tail, [{var, 0, '_'} | Acc]);
make_record_default ([{_, Value} | Tail], Acc) ->
  make_record_default (Tail, [{atom, 0, Value} | Acc]).


make_record_pattern ([], Pattern, RecordDefinition) ->
  Pattern;
make_record_pattern ([{record_field, _, {atom, _, FieldName}, MatchValue}
                      | Tail], Pattern, RecordDefinition) ->
  FieldPosition = get_record_field_pos (FieldName, RecordDefinition, 1),
  NewPattern =
    lists:sublist (Pattern, FieldPosition - 1) ++
    [MatchValue] ++
    lists:sublist (Pattern, FieldPosition + 1,
                   length (Pattern) - FieldPosition),
  make_record_pattern (Tail, NewPattern, RecordDefinition).


get_record_field_pos (Name, [], _) -> exit ({bad_record_field, Name});
get_record_field_pos (Name, [{Name, _} | _], Index) -> Index;
get_record_field_pos (Name, [{Name} | _], Index) -> Index;
get_record_field_pos (Name, [_ | Tail], Index) ->
  get_record_field_pos (Name, Tail, Index + 1).

build_string_condition ([], Acc) ->
  lists:concat (["{", tl (lists:flatten (lists:reverse (Acc))), "}"]);
build_string_condition ([{atom, _, Value} | Tail], Acc) ->
  Term = lists:concat ([",'", atom_to_list (Value), "'"]),
  build_string_condition (Tail,
                          [ Term | Acc]);
build_string_condition ([{integer, _, Value} | Tail], Acc) ->
  Term = lists:concat ([",", integer_to_list (Value)]),
  build_string_condition (Tail,
                          [ Term | Acc]);
build_string_condition ([{var, _, Value} | Tail], Acc) ->
  Term = lists:concat ([",", atom_to_list (Value)]),
  build_string_condition (Tail,
                          [ Term | Acc]).


%% read_param ([{var, _, EngineName} | {nil,_}]) -> % Non ci sono condizioni
%%   [];
%% read_param ([{var, _, EngineName}, {cons, _, {tuple, _, ListVar}, Tail}]) ->
%%   % Le condizioni sono espresse in una lista di tuple
%%   OtherCond = toString([Tail], {tail, newCond}),
%%   C1 = lists:concat(['{', toString(ListVar, inside),'}']),
%%   case OtherCond of
%%     [] ->
%%       [C1];
%%     OtherCond ->
%%       lists:append([C1], OtherCond)
%%   end;
%% read_param ([Param|OtherParam]) ->
%%   {error, "I parametri della regola devono essere una variabile e una lista di tuple (EngineName, [{Cond1},...])"}.

%% toString ([], Mode) -> [];
%% toString ([{nil,_} | Other], {tail, Mode}) -> toString(Other, Mode);
%% toString ([{nil,_} | Other], Mode) -> toString(Other, Mode);
%% toString ([{var,_, NameVar} | Other], Mode) ->
%%   Str =  toString(Other, Mode),
%%   case Str of
%%     [] ->
%%       NameVar;
%%     Str ->
%%       lists:concat([NameVar, ',', Str])
%%   end;
%% toString ([{atom, _, Atom} | Other], Mode  ) ->
%%   Str = toString(Other, Mode),
%%   case Str of
%%     [] ->
%%       lists:concat(['\'', Atom, '\'']);
%%     Str ->
%%       lists:concat(['\'', Atom, '\'', ',', Str])
%%   end;
%% toString ([{tuple,_, ListVar} | Other], inside) ->
%%   % la tupla e' un elemento di una condizione (inside)
%%   Str = toString(Other, inside),
%%   case Str of
%%     [] ->
%%       lists:concat(['{', toString(ListVar, inside), '}']);
%%     Str ->
%%       lists:concat(['{', toString(ListVar, inside), '}',',', Str])
%%   end;
%% toString ([{tuple,_, ListVar} | Other], newCond) ->
%%   % la tupla e' una nuova condizione (newCond)
%%   Str = toString(Other, newCond),
%%   Cond =  lists:concat(['{', toString(ListVar, inside), '}']),
%%   case Str of
%%     [] ->
%%       [Cond];
%%     Str ->
%%       lists:append([Cond], Str)
%%   end;
%% toString ([{cons, _, Head, Tail} | Other], {tail, Mode}) ->
%%   Str =  toString([Tail], {tail, Mode}),
%%   case Str of
%%     [] ->
%%       S =  toString([Head], Mode);
%%     Str ->
%%       S = lists:concat([toString([Head], Mode), Str])
%%   end,
%%   Str1 = toString(Other, Mode),
%%   case Str1 of
%%     [] ->
%%       S;
%%     Str1 ->
%%       lists:concat([S, ',', Str1])
%%   end;
%% toString ([{cons, _, Head, Tail} | Other], Mode) ->
%%   Str =  toString([Tail], {tail, Mode}),
%%   case Str of
%%     [] ->
%%       S = lists:concat(['[', toString([Head], Mode), ']']);
%%     Str ->
%%       S = lists:concat(['[', toString([Head], Mode),',', Str,']'])
%%   end,
%%   Str1 = toString(Other, Mode),
%%   case Str1 of
%%     [] ->
%%       S;
%%     Str1 ->
%%       lists:concat([S, ',', Str1])
%%   end.



% P e' una lista contenente le condizioni precedenti a Cond
% della stessa produzione
make_struct (R, Rule, [], P, Cur_node, ng) ->
  [Kb, Alfa, Join, Agenda, State] = R,
  %crea production node
  Key = {np_node, nil},
  case eresye_tree_list:child(Key, Cur_node, Join) of
    false ->
      Value = [],
      {Node, Join1} = eresye_tree_list:insert(Key, Value, Cur_node, Join),
      % io:format("Np_node=~w~n",[Node]),
      {Join2, Agenda1} = update_new_node (Node, Cur_node, Join1, Agenda);
    Node ->
      Join2 = Join,
      Agenda1 = Agenda
  end,
  {[Kb, Alfa, Join2, Agenda1, State], Node};
make_struct (R, Rule, [], P, Cur_node, nil) ->
  [Kb, Alfa, Join, Agenda, State] = R,
  %crea production node
  Key = {p_node, Rule},
  case eresye_tree_list:child(Key, Cur_node, Join) of
    false ->
      Value = [],
      {Node, Join1} = eresye_tree_list:insert(Key, Value, Cur_node, Join),
      {Join2, Agenda1} = update_new_node (Node, Cur_node, Join1, Agenda);
    Node ->
      {Fun, Salience} = Rule,
      Key1 =element(1, Node),
      Sal = element(2, element(2, Key1)),
      io:format(">> Rule (~w) already present ~n",[Fun]),
      io:format(">> with salience = ~w~n",[Sal]),
      io:format(">> To change salience use 'set_salience()'.~n"),
      Join2 = Join,
      Agenda1 = Agenda
  end,
  [Kb, Alfa, Join2, Agenda1, State];
make_struct (R, Rule, [], P, Cur_node, {Nod, NConds}) ->
  [Kb, Alfa, Join, Agenda, State] = R,
  Id = eresye_tree_list:get_id(Nod),
  {Cur_node1, Join1, Agenda1} = make_join_node (Join,
                                                {old,{n_node, Id}},
                                                NConds, P, Cur_node, Agenda),
  Nod1 = eresye_tree_list:refresh(Nod, Join1),
  Join2 = eresye_tree_list:set_child(Cur_node1, Nod1, Join1),
  %crea production node
  Key = {p_node, Rule},
  case eresye_tree_list:child(Key, Cur_node1, Join2) of
    false ->
      Value = [],
      {Node, Join3} = eresye_tree_list:insert(Key, Value, Cur_node1, Join2),
      % io:format("P_node=~w~n",[Node]),
      Cur_node2 = eresye_tree_list:refresh(Cur_node1, Join3),
      {Join4, Agenda2} = update_new_node (Node, Cur_node2, Join3, Agenda1);
    Node ->
      {Fun, Salience} = Rule,
      Key1 = element(1, Node),
      Sal = element(2, element(2, Key1)),
      io:format(">> Rule (~w) already present ~n",[Fun]),
      io:format(">> with salience = ~w~n",[Sal]),
      io:format(">> To change salience use 'set_salience()'.~n"),
      Join4 = Join2,
      Agenda2 = Agenda1
  end,
  [Kb, Alfa, Join4, Agenda2, State];
make_struct (R, Rule, [Cond|T], P, Cur_node, Nod) ->
  [Kb, Alfa, Join, Agenda, State] = R,
  {Alfa1, Tab} = add_alfa(R, Cond),
  {Cur_node1, Join1, Agenda1} = make_join_node (Join, Tab, Cond, P,
                                                Cur_node, Agenda),
  P1 = lists:append(P, [Cond]),
  make_struct([Kb, Alfa1, Join1, Agenda1, State], Rule, T, P1, Cur_node1, Nod).


add_alfa (R,  Cond)->
  [Kb, Alfa, Join, _, _] = R,
  case is_present(Cond, Alfa) of
    false ->
      Tab = ets:new(alfa, [bag]),
      %%io:format ("Cond ~p~n", [Cond]),
      Fun = lists:concat(["F=fun(", Cond, ")->true end."]),
      Alfa_fun = evaluate(Fun),
      Alfa1 = [{Cond, Tab, Alfa_fun} | Alfa],
      initialize_alfa(Cond, Tab, Kb),
      {Alfa1, {new, Tab}};
    {true, Tab} ->
      {Alfa, {old, Tab}}
  end.


% prepara una stringa del tipo Cond=Fact
% (es. "{X, on, Y}={b1, on, b3}." )
prepare_string (Cond, Fact) ->
  Fact1 = tuple_to_list(Fact),
  Str = lists:concat([Cond, "={"]),
  %%io:format ("F1 = ~p~n", [Fact1]),
  Str1 = append_fact ([], Fact1, siend),
  Str2 = string:concat(Str, Str1).



append_fact (H, [Elem], Flag) when is_list(Elem) ->
  H1 = lists:concat([H,"[", append_fact([], Elem, noend), "]}."]);
append_fact (H, [Elem], Flag) when is_tuple(Elem) ->
  Elem1 = tuple_to_list(Elem),
  H1 = lists:concat([H,"{", append_fact([], Elem1, noend), "}}."]);
append_fact (H, [Elem], Flag) when is_integer (Elem) ->
  case Flag of
    noend ->
      lists:concat([H, Elem]);
    Other ->
      lists:concat([H, Elem, "}."])
  end;
append_fact (H, [Elem], Flag) ->
  case Flag of
    noend ->
      lists:concat([H, "'", Elem, "'"]);
    Other ->
      lists:concat([H, "'", Elem, "'}."])
  end;
append_fact (H, [Elem | Other_elem], Flag) when is_list(Elem) ->
  H1 = lists:concat([H,"[", append_fact([], Elem, noend), "],"]),
  append_fact (H1, Other_elem, Flag);
append_fact (H, [Elem | Other_elem], Flag) when is_tuple(Elem) ->
  Elem1 = tuple_to_list(Elem),
  H1 = lists:concat([H,"{", append_fact([], Elem1, noend), "},"]),
  append_fact (H1, Other_elem, Flag);
append_fact (H, [Elem | Other_elem], Flag) when is_integer(Elem) ->
  H1 = lists:concat([H, Elem, ","]),
  append_fact (H1, Other_elem, Flag);
append_fact (H, [Elem | Other_elem], Flag) ->
  H1 = lists:concat([H,"'", Elem, "',"]),
  append_fact (H1, Other_elem, Flag).



remove_prod (R, Fun) ->
  [Kb, Alfa, Join, Agenda, State] = R,
  case eresye_tree_list:keysearch({p_node, Fun}, Join) of
    false ->
      R;
    Node ->
      Parent_node = eresye_tree_list:get_parent(Node, Join),
      Join1 = eresye_tree_list:remove_node(Node, Join),
      Parent_node1 = eresye_tree_list:refresh(Parent_node, Join1),
      R1 = remove_nodes (Parent_node1, [Kb, Alfa, Join1, Agenda, State]),
      remove_prod (R1, Fun)
  end.


remove_nodes (Node, R) ->
  [Kb, Alfa, Join, Agenda, State] = R,
  case eresye_tree_list:have_child(Node) of
    false ->
      case eresye_tree_list:is_root(Node) of
        false ->
          Parent_node = eresye_tree_list:get_parent(Node, Join),
          Join1 = eresye_tree_list:remove_node(Node, Join),
          Parent_node1 = eresye_tree_list:refresh(Parent_node, Join1),
          {First, _} = eresye_tree_list:get_key(Node),
          case First of
            {n_node, IdNp_node} ->
              ParentKey = eresye_tree_list:get_key(Parent_node1),
              % elimino tutti i nodi relativi alle condizioni negate
              Np_node = eresye_tree_list:get_node(IdNp_node, Join1),
              Join2 = eresye_tree_list:remove_child(Node, Np_node, Join1),
              R1 = [Kb, Alfa, Join2, Agenda, State],
              Np_node1 = eresye_tree_list:refresh(Np_node, Join2),
              R2 = remove_nodes(Np_node1, R1),
              % recupero il parent node del nodo n_node passato come argomento
              % il parent puo' avere adesso un altro id, ma ha la stessa key
              Join3 = lists:nth(3, R2),
              Parent_node2 = eresye_tree_list:keysearch(ParentKey, Join3),
              remove_nodes(Parent_node2, R2);
            np_node ->
              R1 = [Kb, Alfa, Join1, Agenda, State],
              remove_nodes(Parent_node1, R1);
            Tab ->
              case eresye_tree_list:is_present(Tab, Join1) of
                false ->
                  ets:delete(Tab),
                  Alfa1 = lists:keydelete(Tab, 2, Alfa);
                true ->
                  Alfa1 = Alfa
              end,
              R1 = [Kb, Alfa1, Join1, Agenda, State],
              remove_nodes(Parent_node1, R1)
          end;
        true ->
          R
      end;
    true ->
      R
  end.

% verifica se la condizione Cond e' gia' presente

is_present (Cond, []) ->
  false;
is_present (Cond, [{C1, Tab, Alfa_fun} | Other_cond]) ->
  case same_cond(Cond, C1) of
    true -> {true, Tab};
    false -> is_present(Cond, Other_cond)
  end.

	
same_cond (Cond1, Cond1) ->
  true;
same_cond (Cond1, Cond2) ->
  C2 = parse_cond(Cond2),
  S1 = prepare_string(Cond1, C2),
  % io:format("S1=~s~n",[S1]),
  case evaluate(S1) of
    C2 ->
      C1 = parse_cond(Cond1),
      S2 = prepare_string(Cond2, C1),
      % io:format("S2=~s~n",[S2]),
      case evaluate(S2) of
        C1 -> true;
        false -> false
      end;
    false -> false
  end.


parse_cond (L) ->
  L1 = string:sub_string(L, 2, length(L)-1),
  list_to_tuple(to_elem(L1)).


to_elem ([]) ->
  [];
% l'elemento e' una tupla
to_elem (L) when hd(L)==${ ->
  ElemStr = string:sub_string(L, 2),
  {List, OtherStr} = to_elem(ElemStr),
  T = list_to_tuple(List),
  Other = to_elem(OtherStr),
  E1=lists:append([T], Other),
  E1;
to_elem (L) when hd(L)==$[ ->
  ElemStr = string:sub_string(L, 2),
  {List, OtherStr} = to_elem(ElemStr),
  Other = to_elem(OtherStr),
  Li = lists:append([List], Other),
  Li;
to_elem (L) when hd(L)==$, ->
  L1 = string:sub_string(L, 2),
  to_elem(L1);
to_elem (L) ->
  Index1 = string:chr(L, $,),
  Index2 = string:chr(L, $}),
  Index3 = string:chr(L, $]),
  to_elem(Index1, Index2, Index3, L).
to_elem(I1, I2, I3, L)
  when I2/=0, ((I2<I1) or (I1==0)), ((I2<I3) or (I3==0)) ->
  % l'elemento e' l'ultimo elemento di una tupla
  ElemStr = string:sub_string(L, 1, I2-1),
  OtherStr = string:sub_string(L, I2+1),
  Elem = get_atom(ElemStr),
  {[Elem], OtherStr};
to_elem (I1, I2, I3, L)
  when I3/=0, ((I3<I1) or (I1==0)), ((I3<I2) or (I2==0)) ->
  % l'elemento e' l'ultimo elemento di una lista
  ElemStr = string:sub_string(L, 1, I3-1),
  OtherStr = string:sub_string(L, I3+1),
  Elem = get_atom(ElemStr),
  {[Elem], OtherStr};
to_elem (I1, I2, I3, L) ->
  case I1 == 0 of
    false ->
      ElemStr = string:sub_string(L, 1, I1-1),
      OtherStr = string:sub_string(L, I1+1);
    true ->
      % e' l'ultimo elemento
      ElemStr = L,
      OtherStr = []
  end,
  Elem = [get_atom(ElemStr)],
  case to_elem(OtherStr) of
    {Other, Str} ->
      E = lists:append(Elem, Other),
      {E, Str};
    Other ->
      A = lists:append(Elem, Other),
      A
  end.


% Prende in ingresso una stringa del tipo L="{elem1,elem1,...elemM}"
% restituisce la tupla corrispondente ({elem1, elem2,...elemN})
% trasformando le variabili in atomi (X->'X')

get_atom (X) when hd(X)==$' ->    %'
    L = length(X),
    case lists:nth(L, X) of
	39 ->
	    Sub = string:sub_string(X, 2, L-1),
	    list_to_atom(Sub);
	Other ->
	    io:format(">> Errore (manca l'apice):~s~n",[X])
    end;
get_atom (X) ->
    X1 = string:strip(X),
    list_to_atom(X1).




evaluate (String) ->
  %%io:format ("FUN = ~p~n", [String]),
  {ok, Tokens, _} = erl_scan:string (String),
  {ok, Expr} = erl_parse:parse_exprs (Tokens),
  case catch (erl_eval:exprs (Expr, erl_eval:new_bindings ())) of
    {'EXIT', _} -> false;
    {value, Value, Bindings} -> Value;
    _ -> false
  end.


prepare_fun (Cond, []) ->
    "nil.";
prepare_fun (Cond, [Cond1| T1]) when hd(Cond)==${ ->
    Str = lists:concat(["F=fun(", Cond, ",[", Cond1, string_tail(T1, []), "])->true end."]),
%    io:format("ST=~s~n",[Str]),
    Str;
prepare_fun ([Cond|T], [Cond1| T1]) ->
    Str = lists:concat(["F=fun([", Cond, string_tail(T,[]), "],[", Cond1, string_tail(T1, []), "])->true end."]),
 %   io:format("ST2=~s~n",[Str]),
    Str.

string_tail ([], Str) ->
    Str;
string_tail ([C| OtherC], Str) ->
    Str1 = lists:concat([Str, ",", C]),
    string_tail(OtherC, Str1).

% aggiorna il nuovo join_node inserendo eventuali token nella Beta-memory
update_new_node (Node, Parent_node, Join, Agenda) ->
  %  io:format("Update-Node=~w~n",[Node]),
  %  io:format("Parent=~w~n",[Parent_node]),
  case eresye_tree_list:is_root(Parent_node) of
    false ->
      Children = eresye_tree_list:children(Parent_node, Join),
      case Children -- [Node] of
        [] ->
          case eresye_tree_list:get_key(Parent_node) of
            {{n_node, IdNp_node}, Join_fun} ->
              Beta = eresye_tree_list:get_beta (Parent_node),
              update_from_n_node (Parent_node, Beta, Join, Agenda);
            {Tab, _} ->
              %%Tab = element(1, element(1, Parent_node)),
              Fact_list = ets:tab2list(Tab),
              update_new_node (Node, Parent_node, Join, Fact_list, Agenda)
          end;
        [Child | Other_child] ->
          Beta = eresye_tree_list:get_beta (Child),
          Join1 = eresye_tree_list:update_beta (Beta, Node, Join),
          case eresye_tree_list:get_key (Node) of
            {p_node, Rule} ->
              Agenda1 = update_agenda (Agenda, Beta, Rule);
            Key ->
              Agenda1 = Agenda
          end,
          {Join1, Agenda1}
      end;
    true ->
      {Join, Agenda}
  end.


update_agenda (Agenda, [], Rule) ->
  Agenda;
update_agenda (Agenda, [Tok| OtherTok], Rule) ->
  %%io:format ("UPDATE AGENDA ~p~n", [Rule]),
  Agenda1 = signal(Tok, plus, Rule, Agenda),
  update_agenda(Agenda1, OtherTok, Rule).


update_new_node (Node, Parent_node, Join, [], Agenda) ->
  {Join, Agenda};
update_new_node (Node, Parent_node, Join, [Fact | Other_fact], Agenda) ->
  Tok_list = right_act({Fact, plus}, Parent_node),
  {Join1, Agenda1} = pass_tok(Tok_list, [Node], Join, Agenda),
  Node1 = eresye_tree_list:refresh(Node, Join1),
  update_new_node (Node1, Parent_node, Join1, Other_fact, Agenda1).

update_from_n_node (Parent_node, [], Join, Agenda) ->
  Join1 = eresye_tree_list:update_node(Parent_node, Join),
  {Join1, Agenda};
update_from_n_node ( Parent_node, [Tok|OtherTok], Join, Agenda) ->
  {Join1, Agenda1} = left_act({Tok, plus}, [Parent_node], Join, Agenda),
  update_from_n_node(Parent_node, OtherTok, Join1, Agenda1).

%% execute_rule (Rule, Args, State) ->
%%     callfunction (Rule, Args),
%%     [Ser | _] = Args,
%%     State1 = end_processing(State),
%%     Data = {self(), state, State1},
%%     Ser ! Data,
%%     receive
%% 	{ack, X} -> X;
%% 	Other -> io:format (">> Invalid message = ~w\n", [Ser]),
%% 		 throw ({badtransaction, Other})
%%     after
%% 	5000 -> throw ({timeout, Data})
%%     end.




signal (Token, Sign, {Fun, Salience}, Agenda) ->
  case Sign of
    plus ->
      % spawn(ser, callfunction, [self(), Fun, Token]),
      %io:format(">> New match for production ~w~n", [Fun]),
      eresye_agenda:addActivation(Agenda, Fun,[self(), Token], Salience);
    minus ->
      %io:format(">> Lost match for production ~w~n", [Fun]),
      ActivationId = eresye_agenda:getActivation(Agenda,
                                                 {Fun, [self(), Token]}),
      eresye_agenda:deleteActivation(Agenda, ActivationId)
  end.


%% callfunction (Fun ,Term) ->
%%   %io:format("callfunction~n"),
%%   case catch (apply (Fun, Term)) of
%%     {'EXIT', {function_clause, K}} -> false;
%%     {'EXIT', OtherExc} -> throw (OtherExc);
%%     Other -> Other
%%   end.


%% end_processing (State) ->
%%   lists:delete(processing, State).


%%====================================================================
%% Fact Assertion/Retraction Functions
%%====================================================================
%% Insert a fact in the KB.
%% It also checks if the fact verifies any condition,
%% if this is the case the fact is also inserted in the alpha-memory
%%====================================================================
assert_fact (R, Fact) ->
  [Kb, Alfa, Join, Agenda, State] = R,
  case lists:member (Fact, Kb) of
    false ->
      Kb1 = [Fact | Kb],
      check_cond ([Kb1, Alfa, Join, Agenda, State], Alfa, {Fact, plus});
    true ->
      R
  end.



% rimuove un 'fatto' dalla Knowledge Base e se verifica qualche
% condizione  viene eliminato anche dalla corrispondente alfa-memory
retract_fact (R, Fact) ->
  [Kb, Alfa, Join, Agenda, State] = R,
  case lists:member (Fact, Kb) of
    true ->
      Kb1 = Kb -- [Fact],
      check_cond ([Kb1, Alfa, Join, Agenda, State], Alfa, {Fact, minus});
    false ->
      R
  end.



check_cond (R, [], {Fact, Sign}) -> R;
check_cond (R, [{C1, Tab, Alfa_fun} | T], {Fact, Sign} ) ->
  case catch Alfa_fun(Fact) of
    true ->
      case Sign of
        plus ->
          ets:insert (Tab, Fact);
        minus ->
          ets:delete_object (Tab, Fact)
      end,
      R1 = pass_fact (R, Tab, {Fact, Sign}),
      check_cond (R1, T, {Fact, Sign});
    {'EXIT',{function_clause,_}} ->
      check_cond(R, T, {Fact, Sign});
    Other ->
      check_cond(R, T, {Fact, Sign})
  end.

% propaga il 'fatto' a tutti i nodi che seguono l'alfa-memory
% con indice Tab
pass_fact (R, Tab, {Fact, Sign}) ->
  [Kb, Alfa, Join, Agenda, State] = R,
  Succ_node_list = eresye_tree_list:lookup_all (Tab, Join),
  {Join1, Agenda1} = propagate (Succ_node_list, {Fact, Sign}, Join, Agenda),
  [Kb, Alfa, Join1, Agenda1, State].

propagate ([], {Fact, Sign}, Join, Agenda) ->
  {Join, Agenda};
propagate ([Join_node | T], {Fact, Sign}, Join, Agenda) ->
  Join_node1 = eresye_tree_list:refresh(Join_node, Join),
  Tok_list = right_act({Fact, Sign}, Join_node1),
  case eresye_tree_list:get_key(Join_node1) of
    {{n_node, _}, _} ->
      {Join1, Agenda1} = propagate_nnode (Join_node1, Tok_list,
                                          Sign, Join, Agenda);
    {Tab, Fun} ->
      Children_list = eresye_tree_list:children(Join_node1, Join),
      {Join1, Agenda1} = pass_tok(Tok_list, Children_list, Join, Agenda)
  end,
  propagate(T, {Fact, Sign}, Join1, Agenda1).

propagate_nnode (Join_node, [], _, Join, Agenda) ->
  {Join, Agenda};
propagate_nnode (Join_node, Tok_list, Sign, Join, Agenda)->
  % io:format("Tok_list=~w~n",[Tok_list]),
  Children_list = eresye_tree_list:children(Join_node, Join),
  Toks = lists:map(fun(Tk)->
                       L = element(1, Tk),
                       T1 = lists:sublist(L, length(L)-1 ),
                       case Sign of
                         plus ->
                           {T1, minus };
                         minus ->
                           {T1, plus}
                       end
                   end, Tok_list),
  % io:format("Toks=~w~n",[Toks]),
  case Sign of
    plus ->
      pass_tok(Toks, Children_list, Join, Agenda);
    minus ->
      {{n_node, IdNp_node}, Join_fun} = eresye_tree_list:get_key(Join_node),
      test_nnode (Toks, IdNp_node, Join_fun, Join_node, Join, Agenda)
  end.


% propaga ogni token ai nodi presenti in Children_list
pass_tok ([], Children_list, Join, Agenda) ->
  {Join, Agenda};
pass_tok ([Tok | T], Children_list, Join, Agenda) ->
  {Join1, Agenda1} = left_act (Tok, Children_list, Join, Agenda),
  Children_list1 = refresh (Children_list, Join1),
  pass_tok (T, Children_list1, Join1, Agenda1).



% inserisce il token Tok nella Beta_memory del Join_node,
% confronta il token Tok con tutti i fatti presenti nell'alfa memory
% associata al Join_node e se si ha un esito positivo propaga il
% token ai nodi figli

left_act ({Token, Sign}, [], Join, Agenda) ->
  {Join, Agenda};

left_act ({Token, Sign}, [Join_node | T], Join, Agenda) ->
  Beta = eresye_tree_list:get_beta (Join_node),
  case Sign of
    plus ->
      Beta1 = Beta ++ [Token];
    minus ->
      Beta1 = Beta -- [Token]
  end,
  Join1 = eresye_tree_list:update_beta (Beta1, Join_node, Join),
  case eresye_tree_list:get_key(Join_node) of
    {p_node, Rule} ->
      %%io:format ("LEFT ACT ~p~n", [Rule]),
      Agenda1 = signal(Token, Sign, Rule, Agenda),
      left_act({Token, Sign}, T, Join1, Agenda1);
    {{n_node, IdNp_node}, Join_fun} ->
      left_act_nnode({Token, Sign}, IdNp_node, Join_fun,
                     [Join_node | T], Join1, Agenda);
    {np_node, nil} ->
      Children_list = eresye_tree_list:children(Join_node, Join1),
      {Join2, Agenda1} = propagate (Children_list,
                                    {Token, Sign}, Join1, Agenda),
      left_act({Token, Sign}, T, Join2, Agenda1);
    {Tab, Join_fun} ->
      Alfa_mem = ets:tab2list(Tab),
      Tok_list = join_left ({Token, Sign}, Alfa_mem, Join_fun),
      Children_list = eresye_tree_list:children(Join_node, Join1),
      {Join2, Agenda1} = pass_tok (Tok_list, Children_list, Join1, Agenda),
      left_act ({Token,Sign}, T, Join2, Agenda1)
  end.


join_left ({Tok, Sign}, Mem, Join_fun) ->
  lists:foldl(fun(WmeOrTok, Tok_list) ->
                  Tok1 = match(WmeOrTok, Tok, Join_fun),
                  case Tok1 of
                    [] -> Tok_list;
                    Other ->
                      lists:append(Tok_list, [{Tok1, Sign}])
                  end
              end, [], Mem).


left_act_nnode ({Token, Sign}, IdNp_node, Join_fun, [Join_node | T], Join, Agenda )->
  Np_node = eresye_tree_list:get_node(IdNp_node, Join),
  BetaN = eresye_tree_list:get_beta(Np_node),
  Tok_list = join_left ({Token, Sign}, BetaN, Join_fun),
  case Tok_list of
    [] ->
      Children_list = eresye_tree_list:children(Join_node, Join),
      {Join1, Agenda1} = pass_tok ([{Token, Sign}], Children_list,
                                   Join, Agenda),
      left_act ({Token, Sign}, T, Join1, Agenda1);
    Tok_list ->
      left_act ({Token, Sign}, T, Join, Agenda)
  end.

test_nnode ([], _, _, _, Join, Agenda)->
  {Join, Agenda};
test_nnode ( [Tok| OtherTok], IdNpNode, Join_fun, Join_node, Join, Agenda) ->
  {Join1, Agenda1} = left_act_nnode (Tok, IdNpNode, Join_fun, [Join_node], Join, Agenda),
  test_nnode(OtherTok, IdNpNode, Join_fun, Join_node, Join1, Agenda1).


% confronta il nuovo Wme con tutti i token presenti nella Beta-memory
% genitore e ritorna una lista di nuovi token ([Tok1+Wme, Tok2+Wme,...])
% o una lista vuota se il Wme non 'matcha' con i token della Beta-memory

right_act ({Fact, Sign}, Join_node) ->
    Beta = element(2, Join_node),
    Join_fun = element(2, element(1, Join_node)),
    Tok_list = join_right ({Fact, Sign}, Beta, Join_fun).


join_right ({Fact, Sign}, Beta, nil) ->
    append(Beta, {Fact, Sign});
join_right ({Fact, Sign}, [], Join_fun) ->
    [];
join_right ({Fact, Sign}, Beta, Join_fun) ->
    join_right ({Fact, Sign}, Beta, Join_fun, []).


join_right ({Fact, Sign}, [], Join_fun, PassedTok) ->
    PassedTok;
join_right ({Fact, Sign}, [Tok| T], Join_fun, PassedTok) ->
    Pass = match (Fact, Tok, Join_fun),
    case Pass of
	[] ->
	    join_right ({Fact, Sign}, T, Join_fun, PassedTok);
	New_tok ->
	    L = lists:append(PassedTok, [{Pass, Sign}]),
	    join_right ({Fact, Sign}, T, Join_fun, L)
    end.


refresh ([], Join, L) ->
  lists:reverse (L);
refresh ([Node | OtherNode], Join, L) ->
  Node1 = eresye_tree_list:refresh (Node, Join),
  L1 = [Node1 | L],
  refresh (OtherNode, Join, L1).
refresh (N, Join) ->
  refresh (N, Join, []).


% fa il match tra Wme e il token Tok e restituisce una lista vuota in caso
% negativo o un nuovo token (Tok+Wme) in caso positivo
match (Wme, Tok, Join_fun) ->
    case catch Join_fun(Wme, Tok) of
	true ->
	    lists:append(Tok, [Wme]);	
	{'EXIT',{function_clause,_}} ->
	    [];
	Other ->
	    []
    end.


append ([], {Fact, Sign}) ->
    [{[Fact], Sign}];
append (Beta, {Fact, Sign}) ->
    lists:foldl(fun(Tok, New_Beta)->
			Tok1 = lists:append(Tok, [Fact]),
			lists:append(New_Beta, {Tok1, Sign})
		end, [], Beta).



% condivide  o crea un join-node
make_join_node (J, {new, Tab}, Cond, P, Parent_node, Agenda) ->
    Join_fun = evaluate(prepare_fun(Cond, P)),
    new_join (J, Tab, Join_fun, Parent_node, Agenda);
make_join_node (J, {old, Tab}, Cond, P, Parent_node, Agenda) ->
    Result = eresye_tree_list:child(Tab, Parent_node, J),
    case Result of
	false ->
	    Join_fun = evaluate(prepare_fun(Cond, P)),
	    new_join(J, Tab, Join_fun, Parent_node, Agenda);
	 Node ->
	    {Node, J, Agenda}
    end.



new_join (J, Tab, Join_fun, Parent_node, Agenda) ->
    Key = {Tab, Join_fun},
    Value = [],                               %Beta_parent
    {Node, J1} = eresye_tree_list:insert(Key, Value, Parent_node, J),
    {J2, Agenda1} = update_new_node(Node, Parent_node, J1, Agenda),
    Node1 = eresye_tree_list:refresh(Node, J2),
    {Node1, J2, Agenda1}.


