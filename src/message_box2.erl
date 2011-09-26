%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@mac-hiroe-orz-17.local>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 26 Sep 2011 by Hiroe Shin <shin@mac-hiroe-orz-17.local>
%%%-------------------------------------------------------------------
-module(message_box2).

%% Include
-include("message_box.hrl").
-include("message.hrl").
-include("user.hrl").

-export([start/0, stop/0,
         get_message/1, send_message/3, 
         get_home_timeline/2, get_mentions_timeline/2, get_sent_timeline/2,
         follow/3, unfollow/3, is_following/2]).

%%--------------------------------------------------------------------
%% @doc
%% Boot file_box system.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start() -> ok).

start() ->
    application:start(?MODULE),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Shut down file_box system.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop() -> ok).

stop() ->
    application:stop(?MODULE),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% Get Message from user process.
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_message(MessageId::integer()) -> #message{} ).

get_message(MessageId) when is_integer(MessageId) ->
    Fun = fun(Pid) -> message_box2_worker:get_message(Pid, MessageId) end,
    message_box2_worker_spawner:spawn_do(Fun).


%%--------------------------------------------------------------------
%% @doc
%% Send Message.
%%
%% @end
%%--------------------------------------------------------------------
-spec(send_message(UserId::integer(), Password::string(), TextBin::binary()) ->
             {ok, MessageId::integer()}).

send_message(UserId, Password, TextBin) when is_integer(UserId) and
                                             is_list(Password) and
                                             is_binary(TextBin) ->
    
    Fun = fun(Pid) -> 
                  message_box2_worker:send_message(Pid, UserId,
                                                   Password, 
                                                   TextBin)
          end,

    message_box2_worker_spawner:spawn_do(Fun).

%%--------------------------------------------------------------------
%% @doc
%% Get home timeline list.
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_home_timeline(UserId::integer(), Count::integer()) -> 
             {ok, [#message{}]} | {error, user_not_found}).

get_home_timeline(UserId, Count) when is_integer(UserId) and
                                      is_integer(Count) ->

    Fun = fun(Pid) -> 
                  message_box2_worker:get_home_timeline(Pid, UserId, Count) 
          end,
    message_box2_worker_spawner:spawn_do(Fun).

%%--------------------------------------------------------------------
%% @doc
%% Get mentions timeline list.
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_mentions_timeline(UserId::integer(), Count::integer()) -> 
             {ok, [#message{}]} | {error, user_not_found}).

get_mentions_timeline(UserId, Count) when is_integer(UserId) and
                                          is_integer(Count) ->
    Fun = fun(Pid) -> 
                  message_box2_worker:get_mentions_timeline(Pid, UserId, 
                                                            Count) 
          end,
    message_box2_worker_spawner:spawn_do(Fun).

%%--------------------------------------------------------------------
%% @doc
%% Get sent timeline list.
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_sent_timeline(UserId::integer(), Count::integer()) -> 
             {ok, [#message{}]} | {error, user_not_found}).

get_sent_timeline(UserId, Count) when is_integer(UserId) and
                                      is_integer(Count) ->
    Fun = fun(Pid) -> 
                  message_box2_worker:get_sent_timeline(Pid, UserId, Count) 
          end,
    message_box2_worker_spawner:spawn_do(Fun).

%%--------------------------------------------------------------------
%% @doc
%% Follow other user.
%%
%% @end
%%--------------------------------------------------------------------
-spec(follow(UserId::integer(), Password::string(), FollowUserId::integer()) -> 
             ok|{error, already_following}|{error, user_not_found}).

follow(UserId, Password, FollowUserId) when is_integer(UserId) and
                                            is_list(Password) and
                                            is_integer(FollowUserId) ->
    Fun = fun(Pid) -> 
                  message_box2_worker:follow(Pid, UserId, Password, 
                                             FollowUserId) 
          end,
    message_box2_worker_spawner:spawn_do(Fun).

%%--------------------------------------------------------------------
%% @doc
%% Remove other user.
%%
%% @end
%%--------------------------------------------------------------------
-spec(unfollow(UserId::integer(), Password::string(), 
               FollowUserId::integer()) -> 
             ok | {error, not_following} | {error, user_not_found}).

unfollow(UserId, Password, FollowUserId) when is_integer(UserId) and
                                              is_list(Password) and
                                              is_integer(FollowUserId) ->

    Fun = fun(Pid) -> 
                  message_box2_worker:unfollow(Pid, UserId, Password, 
                                               FollowUserId) 
          end,
    message_box2_worker_spawner:spawn_do(Fun).

%%--------------------------------------------------------------------
%% @doc
%% Check follow state.
%%
%% @end
%%--------------------------------------------------------------------
-spec(is_following(UserId::integer(), FollowUserId::integer()) -> 
             true | false | {error, not_found} | {error, user_not_found}).

is_following(UserId, FollowUserId) ->
    Fun = fun(Pid) -> 
                  message_box2_worker:is_following(Pid, UserId, FollowUserId) 
          end,
    message_box2_worker_spawner:spawn_do(Fun).
