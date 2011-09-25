
-module(message_box2_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    _UserDb = {message_box2_user_db,                             %% id
               {message_box2_user_db, start_link, []},           %% child
               permanent,                                        %% restart
               2000,                                             %% shutdown
               worker,                                           %% type
               [message_box2_user_db]},                          %% modules

    RestartStrategy = one_for_one,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    MUserSup = {m_user_sup, {m_user_sup, start_link, []},
                permanent, 2000, supervisor, [m_user, m_user_sup]},

    WorkerSpawner = {message_box2_worker_spawner, 
                     {message_box2_worker_spawner, start_link, []},
                     permanent, 2000, worker, [message_box2_worker_spawner]},
    
    BootList = case message_box2_config:get(type) of
                   master -> 
                       [MUserSup, WorkerSpawner];
                   slave ->
                       [MUserSup, WorkerSpawner]
               end,        

    {ok, { SupFlags, BootList } }.

