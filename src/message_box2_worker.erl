%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@mac-hiroe-orz-17.local>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 25 Sep 2011 by Hiroe Shin <shin@mac-hiroe-orz-17.local>
%%%-------------------------------------------------------------------
-module(message_box2_worker).

-behaviour(gen_server).

%% Include
-include("message_box.hrl").
-include("message.hrl").
-include("user.hrl").

%% API
-export([start_link/0,
         create_user/4,
         send_message/4, get_message/2, 
         get_home_timeline/3, get_mentions_timeline/3, get_sent_timeline/3,
         follow/4, unfollow/4, is_following/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Get Message from user process.
%%
%% @end
%%--------------------------------------------------------------------

create_user(Pid, Name, Mail, Password) when is_pid(Pid) and
                                            is_list(Name) and is_list(Mail) and
                                            is_list(Password) ->
    gen_server:call(Pid, {create_user, Name, Mail, Password}).

%%--------------------------------------------------------------------
%% @doc
%% Get Message from user process.
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_message(Pid::pid(), MessageId::integer()) -> #message{} ).

get_message(Pid, MessageId) when is_pid(Pid) and is_integer(MessageId) ->
    gen_server:call(Pid, {get_message, MessageId}).

%%--------------------------------------------------------------------
%% @doc
%% Send Message.
%%
%% @end
%%--------------------------------------------------------------------
-spec(send_message(Pid::pid(), UserId::integer(), Password::string(), 
                   TextBin::binary()) -> {ok, MessageId::integer()}).

send_message(Pid, UserId, Password, TextBin) when is_pid(Pid) and
                                                  is_integer(UserId) and
                                                  is_list(Password) and
                                                  is_binary(TextBin) ->
    gen_server:call(Pid, {send_message, UserId, Password, TextBin}).    

%%--------------------------------------------------------------------
%% @doc
%% Get home timeline list.
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_home_timeline(Pid::pid(), UserId::integer(), Count::integer()) -> 
             {ok, [#message{}]} | {error, user_not_found}).

get_home_timeline(Pid, UserId, Count) when is_pid(Pid) and 
                                           is_integer(UserId) and
                                           is_integer(Count) ->
    gen_server:call(Pid, {get_home_timeline, UserId, Count}).

%%--------------------------------------------------------------------
%% @doc
%% Get mentions timeline list.
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_mentions_timeline(Pid::pid(), UserId::integer(), Count::integer()) -> 
             {ok, [#message{}]} | {error, user_not_found}).

get_mentions_timeline(Pid, UserId, Count) when is_pid(Pid) and 
                                               is_integer(UserId) and
                                               is_integer(Count) ->
    gen_server:call(Pid, {get_mentions_timeline, UserId, Count}).

%%--------------------------------------------------------------------
%% @doc
%% Get sent timeline list.
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_sent_timeline(Pid::pid(), UserId::integer(), Count::integer()) -> 
             {ok, [#message{}]} | {error, user_not_found}).

get_sent_timeline(Pid, UserId, Count) when is_pid(Pid) and 
                                           is_integer(UserId) and
                                           is_integer(Count) ->
    gen_server:call(Pid, {get_sent_timeline, UserId, Count}).

%%--------------------------------------------------------------------
%% @doc
%% Follow other user.
%%
%% @end
%%--------------------------------------------------------------------
-spec(follow(Pid::pid(), UserId::integer(), Password::string(), 
             FollowUserId::integer()) -> 
             ok|{error, already_following}|{error, user_not_found}).

follow(Pid, UserId, Password, FollowUserId) when is_pid(Pid) and
                                                 is_integer(UserId) and
                                                 is_list(Password) and
                                                 is_integer(FollowUserId) ->
    gen_server:call(Pid, {follow, UserId, Password, FollowUserId}).

%%--------------------------------------------------------------------
%% @doc
%% Remove other user.
%%
%% @end
%%--------------------------------------------------------------------
-spec(unfollow(Pid::pid(), UserId::integer(), Password::string(), 
               FollowUserId::integer()) -> 
             ok | {error, not_following} | {error, user_not_found}).

unfollow(Pid, UserId, Password, FollowUserId) when is_pid(Pid) and
                                                   is_integer(UserId) and
                                                   is_list(Password) and
                                                   is_integer(FollowUserId) ->
    gen_server:call(Pid, {unfollow, UserId, Password, FollowUserId}).

%%--------------------------------------------------------------------
%% @doc
%% Check follow state.
%%
%% @end
%%--------------------------------------------------------------------
-spec(is_following(Pid::pid(), UserId::integer(), FollowUserId::integer()) -> 
             true | false | {error, not_found} | {error, user_not_found}).

is_following(Pid, UserId, FollowUserId) ->
    gen_server:call(Pid, {is_following, UserId, FollowUserId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call({create_user, Name, Mail, Password}, _From, State) ->
    {ok, CreatedUser} = message_box2_user_db:add_user(Name, Mail, Password),
    {ok, _Child} = m_user_sup:start_user(CreatedUser),
    {ok, User} = message_box2_user_db:lookup_id(CreatedUser#user.id),
    {reply, {ok, User}, State};

handle_call({get_message, MessageId}, _From, State) ->
    Reply = 
        case util:get_user_from_message_id(MessageId) of
            {ok, User} ->
                m_user:get_message(User#user.pid, MessageId);
            {error, not_found} -> 
                {error, user_not_found}
        end,

    {reply, Reply, State};

handle_call({send_message, UserId, Password, TextBin}, _From, State) ->
    Reply = 
        case message_box2_user_db:lookup_id(UserId) of
            {ok, User} ->
                m_user:send_message(User#user.pid, Password, TextBin);
            {error, not_found} ->
                {error, user_not_found}
        end,
    {reply, Reply, State};

handle_call({get_home_timeline, UserId, Count}, _From, State) ->
    Reply = 
        case message_box2_user_db:lookup_id(UserId) of
            {ok, User} ->
                Timeline = m_user:get_home_timeline(User#user.pid, Count),
                {ok, Timeline};
            {error, not_found} ->
                {error, user_not_found}
        end,
    {reply, Reply, State};

handle_call({get_mentions_timeline, UserId, Count}, _From, State) ->
    Reply = 
        case message_box2_user_db:lookup_id(UserId) of
            {ok, User} ->
                Timeline = m_user:get_mentions_timeline(User#user.pid, Count),
                {ok, Timeline};
            {error, not_found} ->
                {error, user_not_found}
        end,
    {reply, Reply, State};

handle_call({get_sent_timeline, UserId, Count}, _From, State) ->
    Reply = 
        case message_box2_user_db:lookup_id(UserId) of
            {ok, User} ->
                Timeline = m_user:get_sent_timeline(User#user.pid, Count),
                {ok, Timeline};
            {error, not_found} ->
                {error, user_not_found}
        end,
    {reply, Reply, State};

handle_call({follow, UserId, Password, FollowUserId}, _From, State) ->
    Reply = 
        case message_box2_user_db:lookup_id(UserId) of
            {ok, User} ->
                m_user:follow(User#user.pid, Password, FollowUserId);
            {error, not_found} ->
                {error, user_not_found}
        end,
    {reply, Reply, State};

handle_call({unfollow, UserId, Password, UnFollowUserId}, _From, State) ->
    Reply = 
        case message_box2_user_db:lookup_id(UserId) of
            {ok, User} ->
                m_user:unfollow(User#user.pid, Password, UnFollowUserId);
            {error, not_found} ->
                {error, user_not_found}
        end,
    {reply, Reply, State};

handle_call({is_following, UserId, FollowUserId}, _From, State) ->
    Reply = 
        case message_box2_user_db:lookup_id(UserId) of
            {ok, User} ->
                m_user:is_following(User#user.pid, FollowUserId);
            {error, not_found} ->
                {error, user_not_found}
        end,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
