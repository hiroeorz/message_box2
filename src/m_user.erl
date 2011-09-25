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
         follow/3, unfollow/3, is_following/2,
         get_sent_timeline/2, get_home_timeline/2, get_mentions_timeline/2]).

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
-spec(get_message(Pid::pid(), MessageId::integer()) -> #message{} ).

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
-spec(follow(Pid::pid(), Password::string(), UserId::integer()) -> 
             ok|{error, already_following}|{error, not_found}).

follow(Pid, Password, UserId) ->
    gen_server:call(Pid, {follow, Password, UserId}).

%%--------------------------------------------------------------------
%% @doc
%% Remove other user.
%%
%% @end
%%--------------------------------------------------------------------
-spec(unfollow(Pid::pid(), Password::string(), UserId::integer()) ->
             {ok, deleted}|{error, not_following}|{error, not_found}).

unfollow(Pid, Password, UserId) ->
    gen_server:call(Pid, {unfollow, Password, UserId}).

%%--------------------------------------------------------------------
%% @doc
%% Check follow state.
%%
%% @end
%%--------------------------------------------------------------------
-spec(is_following(Pid::pid(), UserId::integer()) -> true|false).

is_following(Pid, UserId) ->
    gen_server:call(Pid, {is_following, UserId}).


%%--------------------------------------------------------------------
%% @doc
%% Get sent timeline list.
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_sent_timeline(Pid::pid(), Count::integer()) -> [#message{}]).

get_sent_timeline(Pid, Count) ->
    gen_server:call(Pid, {get_sent_timeline, Count}).
    
%%--------------------------------------------------------------------
%% @doc
%% Get home timeline list.
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_home_timeline(Pid::pid(), Count::integer()) -> [#message{}]).

get_home_timeline(Pid, Count) ->
    gen_server:call(Pid, {get_home_timeline, Count}).

%%--------------------------------------------------------------------
%% @doc
%% Get home timeline list.
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_mentions_timeline(Pid::pid(), Count::integer()) -> [#message{}]).

get_mentions_timeline(Pid, Count) ->
    gen_server:call(Pid, {get_mentions_timeline, Count}).

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
handle_call({get_message, MessageId}, From, State) ->
    spawn_link(fun() ->
                       Tid = State#state.message_tid,
                       User = State#state.user,
                       Reply = message_db:get_message(Tid, User, MessageId),
                       gen_server:reply(From, Reply)
               end),

    {noreply, State};

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

handle_call({follow, Password, UserId}, _From, State) ->
    User = State#state.user,
    OneTimePasswordList = State#state.one_time_password_list,

    Reply =     
        case util:authenticate(User, Password, OneTimePasswordList) of
            {ok, authenticated} ->
                case message_box2_user_db:lookup_id(UserId) of
                    {ok, _User} ->
                        follow_db:save_follow_user(User, UserId);
                    {error, not_found} ->
                        {error, not_found}
                end;
            Other -> Other
        end,

    {reply, Reply, State};

handle_call({unfollow, Password, UserId}, _From, State) ->
    User = State#state.user,
    OneTimePasswordList = State#state.one_time_password_list,

    Reply = 
        case util:authenticate(User, Password, OneTimePasswordList) of
            {ok, authenticated} ->
                case message_box2_user_db:lookup_id(UserId) of
                    {ok, _User} ->
                        follow_db:delete_follow_user(User, UserId);
                    {error, not_found} ->
                        {error, not_found}
                end;
            Other -> Other
        end,
    
    {reply, Reply, State};

handle_call({is_following, UserId}, _From, State) ->
    User = State#state.user,

    Reply = 
        case message_box2_user_db:lookup_id(UserId) of
            {ok, _User} ->        follow_db:is_following(User, UserId);
            {error, not_found} -> {error, not_found}
        end,

    {reply, Reply, State};

handle_call({get_sent_timeline, Count}, _From, State) ->
    User = State#state.user,
    Tid = State#state.message_tid,
    Reply = message_db:get_sent_timeline(Tid, User, Count),
    {reply, Reply, State};

handle_call({get_home_timeline, Count}, From, State) ->
    spawn_link(fun() ->
                       User = State#state.user,
                       Tid = State#state.home_tid,
                       Reply = home_db:get_timeline(Tid, User#user.id, Count),
                       gen_server:reply(From, Reply)
               end),

    {noreply, State};

handle_call({get_mentions_timeline, Count}, From, State) ->
    spawn_link(fun() ->
                       User = State#state.user,
                       Tid = State#state.mentions_tid,
                       Reply = mentions_db:get_timeline(Tid, User#user.id, 
                                                        Count),
                       gen_server:reply(From, Reply)
               end),

    {noreply, State}.


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
-spec(send_to_replies(MessageId::integer(), ReplyToList::list(binary())) -> ok).

send_to_replies(MessageId, ReplyToList) ->
    case ReplyToList of
	[] -> ok;
	[ReplyTo | Tail] ->
	    spawn(fun() ->
                          {ok, ToUser} = 
                              message_box2_user_db:lookup_name(ReplyTo),

			  m_user:save_to_mentions(ToUser#user.pid, MessageId),
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
