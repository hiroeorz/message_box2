%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <shin@u652212.xgsfmg23.imtp.tachikawa.mopera.net>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 11 Sep 2011 by Hiroe Shin <shin@u652212.xgsfmg23.imtp.tachikawa.mopera.net>
%%%-------------------------------------------------------------------
-module(m_user).

-behaviour(gen_server).
-include("message_box.hrl").
-include("message.hrl").
-include("user.hrl").

%% API
-export([start_link/1,
         get_message/2, send_message/3, 
         add_follower/2, delete_follower/2, is_following/2]).

%% Internal System
-export([save_to_home/3, save_to_mentions/2]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {user                   ::#user{},
                message_tid            ::tid(),
                home_tid               ::tid(),
                mentions_tid           ::tid(),
                one_time_password_list ::list(binary())}).

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
-spec(start_link(UserId::integer()) -> 
             {ok, Pid::pid()} | ignore | {error, Error::atom()}).

start_link(UserId) when is_integer(UserId) ->
    gen_server:start_link(?MODULE, [UserId], []).

%%--------------------------------------------------------------------
%% @doc
%% Get Message from user process.
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_message(UserId::integer(), MessageId::integer()) -> #message{} ).

get_message(Pid, MessageId) ->
    gen_server:call(Pid, {get_message, MessageId}).

%%--------------------------------------------------------------------
%% @doc
%% Send Message.
%%
%% @end
%%--------------------------------------------------------------------
-spec(send_message(Pid::pid(), Password::string(), TextBin::binary()) ->
             {ok, MessageId::integer()}).

send_message(Pid, Password, TextBin) ->
    gen_server:call(Pid, {send_message, Password, TextBin}).


%%--------------------------------------------------------------------
%% @doc
%% Save to users home table.
%% This function called from other m_user process.
%%
%% @end
%%--------------------------------------------------------------------
-spec(save_to_home(Pid::pid(), MessageId::integer(), 
                   IsReplyTo::{{true, #user{}}|{false, nil}} ) -> ok).

save_to_home(Pid, MessageId, IsReplyText) ->
    gen_server:call(Pid, {save_to_home, MessageId, IsReplyText}).

%%--------------------------------------------------------------------
%% @doc
%% Save to users mentions table.
%% This function called from other m_user process.
%%
%% @end
%%--------------------------------------------------------------------
-spec(save_to_mentions(Pid::pid(), MessageId::integer()) -> 
             {ok, MessageId::integer()}).

save_to_mentions(Pid, MessageId) ->
    gen_server:call(Pid, {save_to_mentions, MessageId}).


%%--------------------------------------------------------------------
%% @doc
%% Follow other user.
%%
%% @end
%%--------------------------------------------------------------------
-spec(add_follower(Pid::pid(), UserId::integer()) -> 
             ok|{error, already_following}|{error, not_found}).

add_follower(Pid, UserId) ->
    gen_server:call(Pid, {add_follower, UserId}).

%%--------------------------------------------------------------------
%% @doc
%% Remove other user.
%%
%% @end
%%--------------------------------------------------------------------
-spec(delete_follower(Pid::pid(), UserId::integer()) ->
             {ok, deleted}|{error, not_following}|{error, not_found}).

delete_follower(Pid, UserId) ->
    gen_server:call(Pid, {delete_follower, UserId}).

%%--------------------------------------------------------------------
%% @doc
%% Check follow state.
%%
%% @end
%%--------------------------------------------------------------------
is_following(Pid, UserId) ->
    gen_server:call(Pid, {is_following, UserId}).


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
init([UserId]) ->
    {ok, User} = message_box2_user_db:lookup_id(UserId),
    {ok, MessageTid} = message_db:init(UserId),
    {ok, HomeTid} = home_db:init(UserId),
    {ok, MentionsTid} = mentions_db:init(UserId),

    message_box2_user_db:save_pid(UserId, self()),
    
    {ok, #state{user=User,
                message_tid=MessageTid, home_tid=HomeTid, 
                mentions_tid=MentionsTid,
                one_time_password_list=[]}}.

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
handle_call({get_message, MessageId}, _From, State) ->
    Tid = State#state.message_tid,
    User = State#state.user,
    Reply = message_db:get_message(Tid, User, MessageId),
    {reply, Reply, State};

handle_call({send_message, Password, TextBin}, _From, State) ->
    User = State#state.user,
    OneTimePasswordList = State#state.one_time_password_list,
    Tid = State#state.message_tid,    
    HomeTid = State#state.home_tid,

    Reply = 
        case util:authenticate(User, Password, OneTimePasswordList) of
            {ok, authenticated} ->
                case message_db:save_message(Tid, User#user.id, TextBin) of
                    {ok, MessageId} ->
                        IsReplyTo = util:is_reply_text(TextBin),
                        send_to_followers(MessageId, User, HomeTid, IsReplyTo),
                            
                        ReplyToList = util:get_reply_list(TextBin),
                        send_to_replies(MessageId, ReplyToList),
                        {ok, MessageId};
                    Other -> Other
                end;
            Other -> Other
        end,

    {reply, Reply, State};

handle_call({save_to_home, MessageId, IsReplyText}, _From, State) ->
    Tid = State#state.home_tid,
    User = State#state.user,
    UserId = User#user.id,

    Reply = 
        case IsReplyText of
            {true, To} ->
                io:format("IsReplyText:~p", [IsReplyText]),
                FromUserId = util:get_user_id_from_message_id(MessageId),
                
                case check_reply_receiver(FromUserId, To#user.id, User) of
                    true  -> home_db:save_message_id(Tid, UserId, MessageId);
                    false -> ok
                end;               
            
            {false, nil} ->
                home_db:save_message_id(Tid, UserId, MessageId)
        end,
    
    {reply, Reply, State};

handle_call({save_to_mentions, MessageId}, _From, State) ->
    Tid = State#state.mentions_tid,
    User = State#state.user,
    Reply = mentions_db:save_message_id(Tid, User#user.id, MessageId),
    {reply, Reply, State};

handle_call({add_follower, UserId}, _From, State) ->
    User = State#state.user,

    Reply = 
        case message_box2_user_db:lookup_id(UserId) of
            {ok, _User} ->
                follow_db:save_follow_user(User, UserId);
            {error, not_found} ->
                {error, not_found}
        end,

    {reply, Reply, State};

handle_call({delete_follower, UserId}, _From, State) ->
    User = State#state.user,

    Reply = 
        case message_box2_user_db:lookup_id(UserId) of
            {ok, _User} ->
                follow_db:delete_follow_user(User, UserId);
            {error, not_found} ->
                {error, not_found}
        end,

    {reply, Reply, State};

handle_call({is_following, UserId}, _From, State) ->
    User = State#state.user,

    Reply = 
        case message_box2_user_db:lookup_id(UserId) of
            {ok, _User} ->        follow_db:is_following(User, UserId);
            {error, not_found} -> {error, not_found}
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

%%
%% @doc send message to followers and saved to there's home_db.
%%
send_to_followers(MessageId, User, HomeTid, IsReplyTo) ->
    Fun1 = fun(Follower) ->
                   FollowerId = Follower#follow.user_id,
                   {ok, FUser} = message_box2_user_db:lookup_id(FollowerId),
                   Pid = FUser#user.pid,
		   m_user:save_to_home(Pid, MessageId, IsReplyTo),

		   io:format("sent: ~p to ~p~n", 
			     [MessageId, Follower#follow.id])
	   end,
    Fun2 = fun(Follower) -> spawn(fun() -> Fun1(Follower) end) end,
    follow_db:map_do(follower, User, Fun2),
    home_db:save_message_id(HomeTid, User#user.id, MessageId).

%%
%% @doc send reply to destination user. 
%%    
send_to_replies(MessageId, ReplyToList) ->
    case ReplyToList of
	[] -> ok;
	[ReplyTo | Tail] ->
	    spawn(fun() ->
			  %%m_user:save_to_mentions(ReplyTo, MessageId),
			  io:format("reply ~p to ~p~n", [MessageId, ReplyTo]),
			  send_to_replies(MessageId, Tail)
		  end)
    end.

%%
%% @doc check reply receiver display home.
%%
-spec(check_reply_receiver(SenderId::integer(), ReceiverId::integer(), 
                           User::#user{}) -> true|false ).

check_reply_receiver(SenderId, ReceiverId, User) ->
    ThisUserId = User#user.id,

    case follow_db:is_following(User, SenderId) of
        true ->
            case follow_db:is_following(User, ReceiverId) of
                true -> true;
                _ ->
                    case ReceiverId  of
                        ThisUserId -> true;
                        _ -> false
                    end
            end;
        _  ->
            false
    end.
