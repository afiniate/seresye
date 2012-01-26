%%%  SERESYE, a Swarm oriented ERlang Expert SYstem Engine
%%%
%%% Copyright (c) 2005-2010, Francesca Gangemi, Corrado Santoro
%%% All rights reserved.
%%% Copyright (c) 2011, Afiniate, Inc.
%%%
%%% You may use this file under the terms of the BSD License. See the
%%% license distributed with this project or
%%% http://www.opensource.org/licenses/bsd-license.php
-module(seresye_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_engine/0, start_engine/1, start_engine/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_engine() ->
    supervisor:start_child(?SERVER, []).

start_engine(Name) ->
    supervisor:start_child(?SERVER, [Name]).

start_engine(Name, ClientState) ->
    supervisor:start_child(?SERVER, [Name, ClientState]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = transient,
    Shutdown = 2000,
    Type = worker,

    AChild = {seresye, {seresye, start_link, []},
              Restart, Shutdown, Type, [seresye]},

    {ok, {SupFlags, [AChild]}}.
