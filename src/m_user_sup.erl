%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@mac-hiroe-orz-17.local>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 25 Sep 2011 by Hiroe Shin <shin@mac-hiroe-orz-17.local>
%%%-------------------------------------------------------------------
-module(m_user_sup).

-behaviour(supervisor).

%% Include
-include_lib("eunit/include/eunit.hrl").
-include("user.hrl").

%% API
-export([start_link/0, start_all_users/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 60000,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, []}}.

start_all_users() ->
    Fun = fun(User) ->
                  Restart = permanent,
                  Shutdown = 2000,
                  Type = worker,
                  
                  AChild = {User#user.id, {m_user, start_link, [User#user.id]},
                            Restart, Shutdown, Type, 
                            [m_user, follow_db, mentions_db, message_db, 
                             mmysql, util]},

                  supervisor:start_child(?SERVER, AChild)
          end,

    message_box2_user_db:map_do(Fun).

%%%===================================================================
%%% Internal functions
%%%===================================================================

