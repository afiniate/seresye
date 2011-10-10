%%%  ERESYE, an ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
-module(eresye_agenda).

-behaviour(gen_server).

-export([addActivation/3, addActivation/4,
         breadthOrder/2, clearAgenda/1, deleteActivation/2,
         deleteRule/2, depthOrder/2, executor/1, fifoOrder/2,
         getActivation/2, getActivationFromName/2,
         getActivationSalience/2, getFirstActivation/1,
         getNumberOfActivations/1, getRulesFired/1,
         getStrategy/1, popRule/1, schedule/1,
         setActivationSalience/3, setRuleSalience/3,
         setStrategy/2, start/1, start_link/1]).

%%====================================================================
%% Internal exports
%%====================================================================

-export([code_change/3, handle_info/2, handle_call/3, handle_cast/2,
         init/1, terminate/2]).

%%====================================================================
%% External functions
%%====================================================================
%% @doc creates a new agenda
start(EngineName) ->
    {ok, Pid} = gen_server:start(?MODULE, EngineName, []),
    Pid.

start_link(EngineName) ->
    {ok, Pid} = gen_server:start_link(?MODULE, EngineName,
                                      []),
    Pid.

addActivation(Agenda, Rule, Args) ->
    addActivation(Agenda, Rule, Args, 0).

addActivation(Agenda, Rule, Args, Salience) ->
    gen_server:call(Agenda,
                    {add_activation, Rule, Args, Salience}),
    Agenda.

%% @doc Remove all activation from Agenda,
%% returns an empty agenda with same past strategy
clearAgenda(Agenda) ->
    gen_server:call(Agenda, {clear}), Agenda.

getStrategy(Agenda) ->
    gen_server:call(Agenda, {get_strategy}).

setStrategy(Agenda, NewStrategy) ->
    gen_server:call(Agenda, {set_strategy, NewStrategy}),
    Agenda.

getRulesFired(Agenda) ->
    gen_server:call(Agenda, {get_rules_fired}).

%% @doc Remove activation with id='Id' or
%% all activation whose id is in the list passed as argument
deleteActivation(Agenda, Ids) ->
    gen_server:call(Agenda, {delete_activation, Ids}),
    Agenda.

%% @doc Remove all activation associated with rule 'Rule' from the agenda,
%% returns the modified agenda
deleteRule(Agenda, Rule) ->
    gen_server:call(Agenda, {delete_rule, Rule}), Agenda.

%% @doc Returns the Id of activation associated to rule 'Rule' and
%% arguments 'Args', false if it not present
getActivation(Agenda, {Rule, Args}) ->
    gen_server:call(Agenda, {get_activation, {Rule, Args}}).

%% @doc Returns the Id of first activation associated to rule 'Rule',
%% false if 'Rule' is not present in the agenda
getActivationFromName(Agenda, Rule) ->
    gen_server:call(Agenda, {get_activation, Rule}).

%% @doc Return the Id associated to first activation in the agenda
%% false if agenda is empty
getFirstActivation(Agenda) ->
    gen_server:call(Agenda, {get_first_activation}).

%% @doc Return the salience value of activation with id='Id'
%% false if Id is not present in the agenda
getActivationSalience(Agenda, Id) ->
    gen_server:call(Agenda, {get_activation_salience, Id}).

%% @doc Sets the salience value of activation with id='Id',
%% returns the modified agenda
setActivationSalience(Agenda, Id, NewSalience) ->
    gen_server:call(Agenda,
                    {set_activation_salience, Id, NewSalience}),
    Agenda.

%% @doc Sets the salience value of all activations associated to rule 'Rule',
%% returns the modified agenda
setRuleSalience(Agenda, Rule, NewSalience) ->
    gen_server:call(Agenda,
                    {set_rule_salience, Rule, NewSalience}),
    Agenda.

schedule(Agenda) -> gen_server:cast(Agenda, {schedule}).

%%====================================================================
%% Callback functions
%%====================================================================
init(EngineName) ->
    Pid = spawn(?MODULE, executor, [EngineName]),
    put(rules_fired, 0),
    {ok, {depth, [], Pid, nil, 0}}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, State) ->
    {_, _, ExecutorPid, _, _} = State,
    ExecutorPid ! {stop},
    ok.

handle_call({stop}, _, State) ->
    {stop, normal, ok, State};
handle_call({get_rules_fired}, _, State) ->
    {reply, {ok, get(rules_fired)}, State};
handle_call({add_activation, Rule, Args, Salience},
            _From, State) ->
    {Strategy, RuleList, ExecutorPid, ExecState, Id} =
        State,
    NewRuleList = case Strategy of
                      depth -> depthAdd(RuleList, Rule, Args, Salience, Id);
                      breadth ->
                          breadthAdd(RuleList, Rule, Args, Salience, Id);
                      fifo -> fifoAdd(RuleList, Rule, Args, Salience, Id)
                  end,
    NewAgenda = {Strategy, NewRuleList, ExecutorPid,
                 ExecState, Id + 1},
    {reply, ok, after_activation_schedule__(NewAgenda)};
%% @doc Remove all activation from Agenda, returns an empty agenda
%% with same past strategy
handle_call({clear}, _From, Agenda) ->
    {_, _, ExecutorPid, ExecState, _} = Agenda,
    NewAgenda = {element(1, Agenda), [], ExecutorPid,
                 ExecState, 0},
    {reply, ok, NewAgenda};
handle_call({get_strategy}, _From, Agenda) ->
    {reply, element(1, Agenda), Agenda};
handle_call({set_strategy, NewStrategy}, _From,
            Agenda) ->
    {_OldStrategy, RuleList, ExecutorPid, ExecState,
     NextId} =
        Agenda,
    NewAgenda = case NewStrategy of
                    depth ->
                        RuleList1 = lists:sort({agenda, depthOrder}, RuleList),
                        {depth, RuleList1, ExecutorPid, ExecState, NextId};
                    breadth ->
                        RuleList1 = lists:sort({agenda, breadthOrder},
                                               RuleList),
                        {breadth, RuleList1, ExecutorPid, ExecState, NextId};
                    fifo ->
                        RuleList1 = lists:sort({agenda, fifoOrder}, RuleList),
                        {fifo, RuleList1, ExecutorPid, ExecState, NextId};
                    _ ->
                        %% Strategy not changed
                        Agenda
                end,
    {reply, ok, NewAgenda};
handle_call({delete_activation, Id}, _From, Agenda) ->
    NewAgenda = deleteActivation__(Agenda, Id),
    {reply, ok, NewAgenda};
handle_call({delete_rule, Rule}, _From, Agenda) ->
    {Strategy, RuleList, ExecutorPid, ExecState, NextId} =
        Agenda,
    ActList = proplists:lookup_all(Rule, RuleList),
    RuleList1 = lists:foldl(fun (X, R1) ->
                                    lists:delete(X, R1)
                            end,
                            RuleList, ActList),
    NewAgenda = {Strategy, RuleList1, ExecutorPid,
                 ExecState, NextId},
    {reply, ok, NewAgenda};
%% @doc Returns the Id of activation associated to rule 'Rule' and
%% arguments 'Args', false if it not present
handle_call({get_activation, {Rule, Args}}, _From,
            Agenda) ->
    {_, RuleList, _, _, _} = Agenda,
    ActList = proplists:lookup_all(Rule, RuleList),
    Reply = case lists:keysearch(Args, 2, ActList) of
                {value, {_, _, _, Id}} -> Id;
                false -> false
            end,
    {reply, Reply, Agenda};
%% @doc Returns the Id of first activation associated to rule 'Rule',
%% false if 'Rule' is not present in the agenda
handle_call({get_activation_from_name, Rule}, _From,
            Agenda) ->
    {_, RuleList, _, _, _} = Agenda,
    Reply = case lists:keysearch(Rule, 1, RuleList) of
                {value, {_, _, _, Id}} -> Id;
                false -> false
            end,
    {reply, Reply, Agenda};
%% @doc Return the Id associated to first activation in the agenda
%% false if agenda is empty
handle_call({get_first_activation}, _From,
            {_, [], _, _, _} = Agenda) ->
    {reply, false, Agenda};
handle_call({get_first_activation}, _From,
            {_, [First | _], _, _, _} = Agenda) ->
    {reply, element(4, First), Agenda};
%% @doc Return the salience value of activation with id='Id'
%% false if Id is not present in the agenda
handle_call({get_activation_salience, Id}, _From,
            Agenda) ->
    {_, RuleList, _, _, NextId} = Agenda,
    Reply = case NextId < Id of
                true -> false;
                false ->
                    case lists:keysearch(Id, 4, RuleList) of
                        {value, {_, _, Salience, _}} -> Salience;
                        false -> false
                    end
            end,
    {reply, Reply, Agenda};
%% @doc Sets the salience value of activation with id='Id',
%% returns the modified agenda
handle_call({set_activation_salience, Id, NewSalience},
            _From, Agenda) ->
    NewAgenda = setActivationSalience__(Agenda, Id,
                                        NewSalience),
    {reply, ok, NewAgenda};
%% @doc Sets the salience value of all activations associated to rule 'Rule',
%% returns the modified agenda
handle_call({set_rule_salience, Rule, NewSalience},
            _From, Agenda) ->
    {_, RuleList, _, _, _} = Agenda,
    ActList = proplists:lookup_all(Rule, RuleList),
    IdList = [handle_call_1(V1) || V1 <- ActList],
    NewAgenda = setActivationSalience__(Agenda, IdList,
                                        NewSalience),
    {reply, ok, NewAgenda}.

handle_call_1({_, _, _, Id}) -> Id.

handle_info(_, Agenda) ->
    {noreply, Agenda}.

handle_cast({schedule}, Agenda) ->
    {noreply, after_execution_schedule__(Agenda)}.

after_activation_schedule__(Agenda) ->
    {Strategy, RuleList, ExecutorPid, ExecState, NextId} =
        Agenda,
    {NewExecState, NewRuleList} = if ExecState == active ->
                                          {ExecState, RuleList};
                                     true ->
                                          {RuleToExecute, RL} =
                                              popRule(RuleList),
                                          if RuleToExecute == false -> {nil, RL};
                                             true ->
                                                  ExecutorPid !
                                                      {exec, self(), RuleToExecute},
                                                  put(rules_fired,
                                                      get(rules_fired) + 1),
                                                  {active, RL}
                                          end
                                  end,
    {Strategy, NewRuleList, ExecutorPid, NewExecState,
     NextId}.

after_execution_schedule__(Agenda) ->
    {Strategy, RuleList, ExecutorPid, _ExecState, NextId} =
        Agenda,
    {RuleToExecute, RL} = popRule(RuleList),
    {NewExecState, NewRuleList} = if RuleToExecute ==
                                     false ->
                                          {nil, RL};
                                     true ->
                                          ExecutorPid !
                                              {exec, self(), RuleToExecute},
                                          put(rules_fired, get(rules_fired) + 1),
                                          {active, RL}
                                  end,
    {Strategy, NewRuleList, ExecutorPid, NewExecState,
     NextId}.

%% @doc Remove activation with id='Id' or
%% all activation whose id is in the list passed as argument
deleteActivation__(Agenda, []) -> Agenda;
deleteActivation__({Strategy, RuleList, ExecutorPid,
                    ExecState, NextId},
                   Id)
  when not is_list(Id) ->
    {Strategy, lists:keydelete(Id, 4, RuleList),
     ExecutorPid, ExecState, NextId};
deleteActivation__(Agenda, [Id | OtherId]) ->
    A1 = deleteActivation__(Agenda, Id),
    deleteActivation__(A1, OtherId).

setActivationSalience__(Agenda, [], _NewSalience) ->
    Agenda;
setActivationSalience__(Agenda, Id, NewSalience)
  when is_number(NewSalience) and not is_list(Id) ->
    {Strategy, RuleList, ExecutorPid, ExecState, NextId} =
        Agenda,
    RuleList2 =
        case lists:keysearch(Id, 4, RuleList) of
            {value, {Rule, Args, _Salience, Id}} ->
                RuleList1 = lists:keyreplace(Id, 4, RuleList,
                                             {Rule, Args, NewSalience, Id}),
                order(RuleList1, Strategy);
            false ->
                RuleList
        end,
    {Strategy, RuleList2, ExecutorPid, ExecState, NextId};
setActivationSalience__(Agenda, Id, _NewSalience)
  when not is_list(Id) ->
    io:format("setActivatinSalience>Id must be a number~n"),
    Agenda;
setActivationSalience__(Agenda, [Id | OtherId],
                        NewSalience) ->
    A1 = setActivationSalience__(Agenda, Id, NewSalience),
    setActivationSalience__(A1, OtherId, NewSalience).

%% @doc Remove the first activation, returns the rule,
%% function arguments and the modified agenda
popRule([]) -> {false, []};
popRule([Activation | NewRuleList]) ->
    {Activation, NewRuleList}.

depthAdd(RuleList, Rule, Args, Salience, Id) ->
    {L1, L2} = lists:splitwith(fun ({_, _, S, _}) ->
                                       S > Salience
                               end,
                               RuleList),
    L1 ++ [{Rule, Args, Salience, Id} | L2].

breadthAdd(RuleList, Rule, Args, Salience, Id) ->
    {L1, L2} = lists:splitwith(fun ({_, _, S, _}) ->
                                       S >= Salience
                               end,
                               RuleList),
    L1 ++ [{Rule, Args, Salience, Id} | L2].

fifoAdd(RuleList, Rule, Args, Salience, Id) ->
    RuleList ++ [{Rule, Args, Salience, Id}].

getNumberOfActivations({_Strategy, RuleList, _NextId}) ->
    length(RuleList).

order(ActionList, Strategy) ->
    case Strategy of
        breadth ->
            lists:sort({agenda, breadthOrder}, ActionList);
        depth -> lists:sort({agenda, depthOrder}, ActionList);
        fifo -> lists:sort({agenda, fifoOrder}, ActionList);
        _Other -> ActionList
    end.

%% @doc define when an Act1 comes before Act2 in breadth strategy
breadthOrder(Act1, Act2)
  when element(3, Act1) > element(3, Act2) ->
    true;
breadthOrder(Act1, Act2)
  when element(3, Act1) < element(3, Act2) ->
    false;
breadthOrder(Act1, Act2)
  when element(4, Act1) > element(4, Act2) ->
    false;
breadthOrder(_Act1, _Act2) -> true.

%% @doc define when an Act1 comes before Act2 in depth strategy
depthOrder(Act1, Act2)
  when element(3, Act1) > element(3, Act2) ->
    true;
depthOrder(Act1, Act2)
  when element(3, Act1) < element(3, Act2) ->
    false;
depthOrder(Act1, Act2)
  when element(4, Act1) < element(4, Act2) ->
    false;
depthOrder(_Act1, _Act2) -> true.

%% @doc define when an Act1 comes before Act2 in fifo strategy
fifoOrder(Act1, Act2)
  when element(4, Act1) > element(4, Act2) ->
    false;
fifoOrder(_Act1, _Act2) -> true.

%%====================================================================
%% Executor functions
%%====================================================================
executor(EngineName) ->
    receive
        {exec, From, R} ->
            {Mod, Fun} = case R of
                             {{M, F}, _, _, _} -> {M, F};
                             _ -> {'_', '_'}
                         end,
            case catch execute_rule(EngineName, R) of
                {'EXIT', {function_clause, [{Mod, Fun, _} | _]}} -> ok;
                {'EXIT', Reason} ->
                    erlang:throw({eresye, {rule_execution,
                                           [EngineName, R, Reason]}});
                _ -> ok
            end,
            schedule(From),
            executor(EngineName);
        {stop} -> ok;
        _ -> executor(EngineName)
    end.

%% Fun = {Module, Function} or
%% Fun = fun (...)
execute_rule(EngineName, {Fun, [_, Args], _, _}) ->
    apply(Fun, [EngineName | Args]).
