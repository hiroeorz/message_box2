
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
    UserDb = {message_box2_user_db,                             %% id
              {message_box2_user_db, start_link, []},           %% child
              permanent,                                        %% restart
              2000,                                             %% shutdown
              worker,                                           %% type
              [message_box2_user_db]},                          %% modules

    RestartStrategy = one_for_one,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    BootList = case message_box2_config:get(type) of
                   master -> 
                       [UserDb];
                   slave ->
                       []
               end,        

    {ok, { SupFlags, BootList } }.

