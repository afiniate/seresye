%%%  SERESYE, a Swarm oriented ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%%% All rights reserved.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
-module(seresye).

%%====================================================================
%% External exports
%%====================================================================

-export([start/0, start/1, start/2, stop/1, get_engine/1,
         add_rules/2, add_rule/2, add_rule/3, assert/2, get_kb/1,
         get_rules_fired/1, get_client_state/1,
         set_hooks/2, get_fired_rule/1,
         set_client_state/2, query_kb/2, serialize/1, 
         remove_rule/2, retract/2]).

%% gen_server callbacks
-export([start_link/0, start_link/1, start_link/2, init/1, handle_call/3,
         handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%====================================================================
%% External functions
%%====================================================================
start() ->
    seresye_sup:start_engine().

start(Name) ->
    seresye_sup:start_engine(Name).

start(Name, ClientState) ->
    seresye_sup:start_engine(Name, ClientState).

set_hooks(Name, Hooks) when is_list(Hooks) ->
    gen_server:cast(Name, {set_hooks, Hooks}).

set_client_state(Name, NewState) ->
    gen_server:cast(Name, {set_client_state, NewState}).

get_client_state(Name) ->
    gen_server:call(Name, get_client_state).

stop(EngineName) ->
    (catch gen_server:call(EngineName, stop)),
    ok.

get_engine(EngineName) ->
    gen_server:call(EngineName, get_engine).

%% @doc Insert a fact in the KB.
%% It also checks if the fact verifies any condition,
%% if this is the case the fact is also inserted in the alpha-memory
assert(Name, Facts) ->
    gen_server:call(Name, {assert, Facts}, infinity).

%% @doc removes a 'fact' in the Knowledge Base and if something occurs
%% Condition is also deleted from the corresponding alpha-memory
retract(Name, Facts) ->
    gen_server:call(Name, {retract, Facts}, infinity).

add_rules(Name, RuleList)
  when is_list(RuleList) orelse is_atom(RuleList) ->
    gen_server:call(Name, {add_rules, RuleList}).

add_rule(Name, Fun) ->
    gen_server:call(Name, {add_rule, Fun}).

add_rule(Name, Rule, Salience) ->
    gen_server:call(Name, {add_rule, Rule, Salience}).

remove_rule(Name, Rule) ->
    gen_server:call(Name, {remove_rule, Rule}).

get_rules_fired(Name) ->
    gen_server:call(Name, get_rules_fired).

get_fired_rule(Name) ->
    gen_server:call(Name, get_fired_rule).    

get_kb(Name) ->
    gen_server:call(Name, get_kb).

query_kb(Name, Pattern) ->
    gen_server:call(Name, {query_kb, Pattern}).

serialize(Name) ->
    gen_server:call(Name, serialize).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
start_link() ->
    gen_server:start_link(?MODULE, [], []).

start_link(Name) when is_atom(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []);

start_link(ClientState) when not is_atom(ClientState) ->
    gen_server:start_link(?MODULE, [ClientState], []).

start_link(ClientState, Name) when is_atom(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [ClientState], []).


init([]) ->
    {ok, seresye_engine:new()};
init([Engine]) when element(1, Engine) == seresye ->
    {ok, seresye_engine:restore(Engine)};
init([ClientState]) ->
    {ok, seresye_engine:new(ClientState)}.


handle_call(get_client_state, _From, State) ->
    Reply =
        try
            {ok, seresye_engine:get_client_state(State)}
        catch
            Type:Reason ->
                {error, {Type, Reason}}
        end,
    {reply, Reply, State};
handle_call(stop, _From, State) ->
    {stop, normal, State};
handle_call({assert, Facts}, _From, State0) ->
    {Reply, State1} =
        try
            {ok, seresye_engine:assert(State0, Facts)}
        catch
            Type:Reason ->
                {{error, {Type, Reason}}, State0}
        end,
    {reply, Reply, State1};
handle_call({retract, Facts}, _From, State0) ->
    {Reply, State1} =
        try
            {ok, seresye_engine:retract(State0, Facts)}
        catch
            Type:Reason ->
                {{error, {Type, Reason}}, State0}
        end,
    {reply, Reply, State1};
handle_call({add_rules, Rules}, _From, State0) ->
    {Reply, State1} =
        try
            {ok, seresye_engine:add_rules(State0, Rules)}
        catch
            Type:Reason ->
                {{error, {Type, Reason}}, State0}
        end,
    {reply, Reply, State1};
handle_call({add_rule, Rule}, _From, State0) ->
    {Reply, State1} =
        try
            {ok, seresye_engine:add_rule(State0, Rule)}
        catch
            Type:Reason ->
                {{error, {Type, Reason}}, State0}
        end,
    {reply, Reply, State1};
handle_call({add_rule, Rule, Salience}, _From, State0) ->
    {Reply, State1} =
        try
            {ok, seresye_engine:add_rule(State0, Rule, Salience)}
        catch
            Type:Reason ->
                {{error, {Type, Reason}}, State0}
        end,
    {reply, Reply, State1};
handle_call({remove_rule, Rule}, _From, State0) ->
    {Reply, State1} =
        try
            {ok, seresye_engine:remove_rule(State0, Rule)}
        catch
            Type:Reason ->
                {{error, {Type, Reason}}, State0}
        end,
    {reply, Reply, State1};
handle_call(get_rules_fired, _From, State0) ->
    Reply =
        try
            seresye_engine:get_rules_fired(State0)
        catch
            Type:Reason ->
                {error, {Type, Reason}}
        end,
    {reply, Reply, State0};
handle_call(get_fired_rule, _From, State0) ->
    Reply =
        try
            seresye_engine:get_fired_rule(State0)
        catch
            Type:Reason ->
                {error, {Type, Reason}}
        end,
    {reply, Reply, State0};
handle_call(get_engine, _From, State0) ->
    {reply, State0, State0};
handle_call(get_kb, _From, State0) ->
    Reply =
        try
            seresye_engine:get_kb(State0)
        catch
            Type:Reason ->
                {error, {Type, Reason}}
        end,
    {reply, Reply, State0};
handle_call({query_kb, Pattern}, _From, State0) ->
    Reply =
        try
            seresye_engine:query_kb(State0, Pattern)
        catch
            Type:Reason ->
                {error, {Type, Reason}}
        end,
    {reply, Reply, State0};

handle_call(serialize, _From, State) ->
    Reply = seresye_engine:serialize(State),
    {reply, Reply, State}.

handle_cast({set_hooks, Hooks}, State) ->
    {noreply, seresye_engine:set_hooks(State, Hooks)};

handle_cast({set_client_state, CS}, State) ->
    {noreply, seresye_engine:set_client_state(State, CS)}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
