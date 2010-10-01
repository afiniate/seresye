%
% eres_agenda.erl
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
-module (eresye_agenda).
-export ([addActivation/3,
          addActivation/4,
          start/1,
          start_link/1,
          deleteRule/2,
          popRule/1,
          deleteActivation/2,
          clearAgenda/1,
          getStrategy/1,
          setStrategy/2,
          getActivation/2,
          getActivationFromName/2,
          getFirstActivation/1,
          getActivationSalience/2,
          getNumberOfActivations/1,
          getRulesFired/1,
          setActivationSalience/3,
          setRuleSalience/3,
          schedule/1,
          breadthOrder/2,
          depthOrder/2,
          fifoOrder/2,
          executor/1]).

%%====================================================================
%% Internal exports
%%====================================================================

-export ([init/1,
          handle_call/3,
          handle_cast/2,
          terminate/2,
          code_change/3]).

-behaviour (gen_server).

%%====================================================================
%% External functions
%%====================================================================
%%====================================================================
%% Func: start/1
%% Arguments: creates a new agenda
%%====================================================================
start (EngineName) ->
  {ok, Pid} = gen_server:start (?MODULE, EngineName, []),
  Pid.

start_link (EngineName) ->
  {ok, Pid} = gen_server:start_link (?MODULE, EngineName, []),
  Pid.

addActivation (Agenda, Rule, Args) ->
  addActivation(Agenda, Rule, Args, 0).

addActivation (Agenda, Rule, Args, Salience) ->
  %%io:format("addActivation>Agenda=~w~nRule=~w~nArgs=~w~n",
  %%          [Agenda,Rule,Args]),
  gen_server:call (Agenda, {add_activation, Rule, Args, Salience}),
  Agenda.

% Remove all activation from Agenda,
% returns an empty agenda with same past strategy
clearAgenda (Agenda) ->
  gen_server:call (Agenda, {clear}),
  Agenda.

getStrategy (Agenda) ->
  gen_server:call (Agenda, {get_strategy}).

setStrategy (Agenda, NewStrategy) ->
  gen_server:call (Agenda, {set_strategy, NewStrategy}),
  Agenda.

getRulesFired (Agenda) ->
  gen_server:call (Agenda, {get_rules_fired}).


% Remove activation with id='Id' or
% all activation whose id is in the list passed as argument
deleteActivation (Agenda, Ids) ->
  %%io:format("deleteActivation>Agenda=~w~nRule=~w~n",
  %%          [Agenda, Ids]),
  gen_server:call (Agenda, {delete_activation, Ids}),
  Agenda.


% Remove all activation associated with rule 'Rule' from the agenda,
% returns the modified agenda
deleteRule (Agenda, Rule) ->
  gen_server:call (Agenda, {delete_rule, Rule}),
  Agenda.


% Returns the Id of activation associated to rule 'Rule' and
% arguments 'Args', false if it not present
getActivation (Agenda, {Rule, Args}) ->
  gen_server:call (Agenda, {get_activation, {Rule, Args}}).

% Returns the Id of first activation associated to rule 'Rule',
% false if 'Rule' is not present in the agenda
getActivationFromName (Agenda, Rule) ->
  gen_server:call (Agenda, {get_activation, Rule}).

% Return the Id associated to first activation in the agenda
% false if agenda is empty
getFirstActivation (Agenda) ->
  gen_server:call (Agenda, {get_first_activation}).

% Return the salience value of activation with id='Id'
% false if Id is not present in the agenda
getActivationSalience (Agenda, Id) ->
  gen_server:call (Agenda, {get_activation_salience, Id}).

% Sets the salience value of activation with id='Id',
% returns the modified agenda
setActivationSalience (Agenda, Id, NewSalience) ->
  gen_server:call (Agenda, {set_activation_salience, Id, NewSalience}),
  Agenda.

% Sets the salience value of all activations associated to rule 'Rule',
% returns the modified agenda
setRuleSalience (Agenda, Rule, NewSalience) ->
  gen_server:call (Agenda, {set_rule_salience, Rule, NewSalience}),
  Agenda.


schedule (Agenda) ->
  gen_server:cast (Agenda, {schedule}).


%%====================================================================
%% Callback functions
%%====================================================================
%%====================================================================
%% Func: init/1
%%====================================================================
init (EngineName) ->
  Pid = spawn (?MODULE, executor, [EngineName]),
  put (rules_fired, 0),
  {ok, {depth,[], Pid, nil, 0}}.


%%====================================================================
%% Func: code_change/3
%%====================================================================
code_change(OldVsn, State, Extra) -> {ok, State}.


%%====================================================================
%% Func: terminate/2
%%====================================================================
terminate(Reason, State) ->
  {_, _, ExecutorPid, _, _} = State,
  ExecutorPid ! {stop},
  %%io:format ("Terminating...~p~n", [self ()]),
  ok.


%%====================================================================
%% Func: handle_call/3
%%====================================================================
handle_call ({stop}, _, State) ->
  {stop, normal, ok, State};

handle_call ({get_rules_fired}, _, State) ->
  {reply, {ok, get(rules_fired)}, State};

handle_call ({add_activation, Rule, Args, Salience}, From, State) ->
  {Strategy, RuleList, ExecutorPid, ExecState, Id} = State,
  %%io:format("addActivation>Rule=~w~nArgs=~w~n",[Rule,Args]),
  NewRuleList =
    case Strategy of
      depth -> depthAdd(RuleList, Rule, Args, Salience, Id);
      breadth -> breadthAdd(RuleList, Rule, Args, Salience, Id);
      fifo -> fifoAdd(RuleList, Rule, Args, Salience, Id)
    end,
  NewAgenda = {Strategy, NewRuleList, ExecutorPid, ExecState, Id + 1},
  {reply, ok, after_activation_schedule__ (NewAgenda)};

% Remove all activation from Agenda,
% returns an empty agenda with same past strategy
handle_call ({clear}, From, Agenda) ->
  {_, _, ExecutorPid, ExecState, _} = Agenda,
  NewAgenda = {element(1, Agenda), [], ExecutorPid, ExecState, 0},
  {reply, ok, NewAgenda};

handle_call ({get_strategy}, From, Agenda) ->
  {reply, element(1, Agenda), Agenda};

handle_call ({set_strategy, NewStrategy}, From, Agenda) ->
  {OldStrategy, RuleList, ExecutorPid, ExecState, NextId} = Agenda,
  NewAgenda =
    case NewStrategy of
      depth ->
        RuleList1 = lists:sort({agenda,depthOrder}, RuleList),
        {depth, RuleList1, ExecutorPid, ExecState, NextId};
      breadth ->
        RuleList1 = lists:sort({agenda,breadthOrder}, RuleList),
        {breadth, RuleList1, ExecutorPid, ExecState, NextId};
      fifo ->
        RuleList1 = lists:sort({agenda,fifoOrder}, RuleList),
        {fifo, RuleList1, ExecutorPid, ExecState, NextId};
      _ ->
        %% Strategy not changed
        Agenda
    end,
  {reply, ok, NewAgenda};


handle_call ({delete_activation, Id}, From, Agenda) ->
  NewAgenda = deleteActivation__ (Agenda, Id),
  {reply, ok, NewAgenda};

handle_call ({delete_rule, Rule}, From, Agenda) ->
  {Strategy, RuleList, ExecutorPid, ExecState, NextId} = Agenda,
  ActList = proplists:lookup_all(Rule, RuleList),
  RuleList1 = lists:foldl (fun (X, R1)->
                               lists:delete(X, R1)
                           end, RuleList, ActList),
  NewAgenda = {Strategy, RuleList1, ExecutorPid, ExecState, NextId},
  {reply, ok, NewAgenda};

% Returns the Id of activation associated to rule 'Rule' and
% arguments 'Args', false if it not present
handle_call ({get_activation, {Rule, Args}}, From, Agenda) ->
  %Rule = {Module, Fun}, or
  %Rulle = fun(...)
  {_, RuleList, _, _, _} = Agenda,
  ActList = proplists:lookup_all(Rule, RuleList),
  %%io:format ("get =~p, ~p. Res = ~p~n", [Rule, RuleList, ActList]),
  Reply =
    case lists:keysearch(Args, 2, ActList) of
      {value, {_, _, _, Id}}->
        Id;
      false ->
        false
    end,
  {reply, Reply, Agenda};

% Returns the Id of first activation associated to rule 'Rule',
% false if 'Rule' is not present in the agenda
handle_call ({get_activation_from_name, Rule}, From, Agenda) ->
  {_, RuleList, _, _, _} = Agenda,
  Reply =
    case lists:keysearch(Rule, 1, RuleList) of
      {value, {_, _, _, Id}}->
        Id;
      false ->
        false
    end,
  %%io:format ("Rule = ~p~nRuleList = ~p~nRes = ~p~n", [Rule, RuleList, Reply]),
  {reply, Reply, Agenda};


% Return the Id associated to first activation in the agenda
% false if agenda is empty
handle_call ({get_first_activation}, From,
             {_, [], _, _, _} = Agenda) ->
  {reply, false, Agenda};
handle_call ({get_first_activation}, From,
             {_, [First | _], _, _, _} = Agenda) ->
  {reply, element (4, First), Agenda};

% Return the salience value of activation with id='Id'
% false if Id is not present in the agenda
handle_call ({get_activation_salience, Id}, From, Agenda) ->
  {_, RuleList, _, _, NextId} = Agenda,
  Reply =
    case NextId < Id of
      true ->
        false;
      false ->
        case lists:keysearch(Id, 4, RuleList) of
          {value, {_, _, Salience, _}} ->
            Salience;
          false ->
            false
        end
    end,
  {reply, Reply, Agenda};

% Sets the salience value of activation with id='Id',
% returns the modified agenda
handle_call ({set_activation_salience, Id, NewSalience}, From, Agenda) ->
  NewAgenda = setActivationSalience__ (Agenda, Id, NewSalience),
  {reply, ok, NewAgenda};

% Sets the salience value of all activations associated to rule 'Rule',
% returns the modified agenda
handle_call ({set_rule_salience, Rule, NewSalience}, From, Agenda) ->
  {_, RuleList, _, _, _} = Agenda,
  ActList = proplists:lookup_all(Rule, RuleList),
  IdList = lists:map(
             fun({_,_,_,Id})->
                 Id
             end, ActList),
  NewAgenda = setActivationSalience__ (Agenda, IdList, NewSalience),
  {reply, ok, NewAgenda}.



%%====================================================================
%% Func: handle_cast/3
%%====================================================================
handle_cast ({schedule}, Agenda) ->
  {noreply, after_execution_schedule__ (Agenda)}.


after_activation_schedule__ (Agenda) ->
  {Strategy, RuleList, ExecutorPid, ExecState, NextId} = Agenda,
  {NewExecState, NewRuleList} =
    if
      ExecState == active -> {ExecState, RuleList};
      true ->
        {RuleToExecute, RL} = popRule (RuleList),
        if
          RuleToExecute == false -> {nil, RL};
          true ->
            ExecutorPid ! {exec, self (), RuleToExecute},
            put (rules_fired, get (rules_fired) + 1),
            {active, RL}
        end
    end,
  {Strategy, NewRuleList, ExecutorPid, NewExecState, NextId}.


after_execution_schedule__ (Agenda) ->
  {Strategy, RuleList, ExecutorPid, ExecState, NextId} = Agenda,
  {RuleToExecute, RL} = popRule (RuleList),
  {NewExecState, NewRuleList} =
    if
      RuleToExecute == false -> {nil, RL};
      true ->
        ExecutorPid ! {exec, self (), RuleToExecute},
        put (rules_fired, get (rules_fired) + 1),
        {active, RL}
    end,
  {Strategy, NewRuleList, ExecutorPid, NewExecState, NextId}.


% Remove activation with id='Id' or
% all activation whose id is in the list passed as argument
deleteActivation__ (Agenda, []) ->
  Agenda;
deleteActivation__ ({Strategy, RuleList, ExecutorPid, ExecState, NextId},
                    Id) when not is_list(Id) ->
  {Strategy, lists:keydelete(Id, 4, RuleList), ExecutorPid, ExecState, NextId};
deleteActivation__ (Agenda, [Id|OtherId]) ->
  A1 = deleteActivation__ (Agenda, Id),
  deleteActivation__ (A1, OtherId).



setActivationSalience__ (Agenda, [], NewSalience) ->
  Agenda;
setActivationSalience__ (Agenda, Id, NewSalience)
  when is_number(NewSalience) and not is_list(Id)->
  {Strategy, RuleList, ExecutorPid, ExecState, NextId} = Agenda,
  case lists:keysearch(Id, 4, RuleList) of
    {value, {Rule, Args, Salience, Id}}->
      RuleList1 = lists:keyreplace(Id, 4, RuleList,
                                   {Rule, Args, NewSalience, Id}),
      RuleList2 = order(RuleList1, Strategy);
    false ->
      RuleList2 = RuleList
  end,
  {Strategy, RuleList2, ExecutorPid, ExecState, NextId};
setActivationSalience__ (Agenda, Id, NewSalience) when not is_list(Id) ->
  io:format("setActivatinSalience>Id must be a number~n"),
  Agenda;
setActivationSalience__ (Agenda, [Id | OtherId], NewSalience) ->
  A1 = setActivationSalience__ (Agenda, Id, NewSalience),
  setActivationSalience__ (A1, OtherId, NewSalience).


% Remove the first activation, returns the rule,
% function arguments and the modified agenda
popRule ([]) -> {false, []};
popRule ([Activation | NewRuleList] = RuleList) -> {Activation, NewRuleList}.




depthAdd (RuleList, Rule, Args, Salience, Id) ->
  {L1, L2} = lists:splitwith(fun({_, _, S, _}) ->
                                 S > Salience
                             end, RuleList),
  lists:append(L1, [{Rule, Args, Salience, Id}| L2]).



breadthAdd (RuleList, Rule, Args, Salience, Id) ->
  {L1, L2} = lists:splitwith(fun({_, _, S, _}) ->
                                 S >= Salience
                             end, RuleList),
  lists:append(L1, [{Rule, Args, Salience, Id}| L2]).

fifoAdd (RuleList, Rule, Args, Salience, Id) ->
  lists:append(RuleList, [{Rule, Args, Salience, Id}]).

getNumberOfActivations ({Strategy, RuleList, NextId}) ->
  length(RuleList).


order (ActionList, Strategy) ->
  case Strategy of
    breadth ->
      lists:sort({agenda, breadthOrder}, ActionList);
    depth ->
      lists:sort({agenda, depthOrder}, ActionList);
    fifo ->
      lists:sort({agenda, fifoOrder}, ActionList);
    Other ->
      ActionList
  end.

% define when an Act1 comes before Act2 in breadth strategy
breadthOrder (Act1, Act2) when element(3, Act1) > element(3, Act2) ->
  true;
breadthOrder (Act1, Act2) when element(3, Act1) < element(3, Act2) ->
  false;
breadthOrder (Act1, Act2) when element(4, Act1) > element(4, Act2) ->
  false;
breadthOrder (Act1, Act2)  ->
  true.

% define when an Act1 comes before Act2 in depth strategy
depthOrder (Act1, Act2) when element(3, Act1) > element(3, Act2) ->
  true;
depthOrder (Act1, Act2) when element(3, Act1) < element(3, Act2) ->
  false;
depthOrder (Act1, Act2) when element(4, Act1) < element(4, Act2) ->
  false;
depthOrder (Act1, Act2)  ->
  true.

% define when an Act1 comes before Act2 in fifo strategy
fifoOrder (Act1, Act2) when element(4, Act1) > element(4, Act2) ->
  false;
fifoOrder (Act1, Act2) ->
  true.


%%====================================================================
%% Executor functions
%%====================================================================
executor (EngineName) ->
  receive
    {exec, From, R} ->
      %%io:format ("Executing rule ~p~n", [R]),
      {Mod, Fun} =
        case R of
          {{M, F}, _, _, _} -> {M, F};
          _ -> {'_', '_'}
        end,
      case catch (execute_rule (EngineName, R)) of
        {'EXIT', {function_clause, [{Mod, Fun, _} | _]}} ->
            ok;
        {'EXIT', Reason} ->
          io:format("** Warning! ERES engine ~p rule execution error\n"
                    "** Rule is ~p~n"
                    "** Reason for Error == ~n** ~p~n",
                    [EngineName, R, Reason]);
        _ -> ok
      end,
      schedule (From),
      executor (EngineName);
    {stop} -> ok;
    _ -> executor (EngineName)
  end.


%% Fun = {Module, Function} or
%% Fun = fun (...)
execute_rule (EngineName ,{Fun, [_, Args], _, _}) ->
  apply (Fun, [EngineName | Args]).
