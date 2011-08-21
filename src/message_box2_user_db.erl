%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <hiroe.orz@gmail.com>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 21 Aug 2011 by Hiroe Shin <hiroe.orz@gmail.com>
%%%-------------------------------------------------------------------
-module(message_box2_user_db).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% Include
-include("../include/user.hrl").

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([add_user/3, update_user/1, lookup_id/1, lookup_name/1, lookup_pid/1,
         map_do/1, save_pid/2, get_pid/1]).

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
    gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

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
    FileName = "/var/message_box2/user_db",
    create_tables(FileName),
    restore_table(),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%%
%% @doc add new user to system.
%%
%% @end
%%--------------------------------------------------------------------
-spec(add_user(Name::atom(), Mail::string(), Password::string()) -> 
             {ok, #user{}} | {error, already_exist}).

add_user(Name, Mail, Password) when is_atom(Name) and is_list(Mail) and 
                                    is_list(Password) ->
    gen_server:call({global, ?SERVER}, {add_user, Name, Mail, Password}).

%%--------------------------------------------------------------------
%%
%% @doc add new user to system.
%%
%% @end
%%--------------------------------------------------------------------
-spec(update_user(#user{}) -> {ok, #user{}} | {error, not_found}).

update_user(User) ->
    gen_server:call({global, ?SERVER}, {update_user, User}).

%%--------------------------------------------------------------------
%%
%% @doc lookup user from id.
%%
%% @end
%%--------------------------------------------------------------------
-spec(lookup_id(integer()) -> {ok, #user{}} | {error, not_found}).

lookup_id(Id)->
    gen_server:call({global, ?SERVER}, {lookup_id, Id}).

%%--------------------------------------------------------------------
%%
%% @doc lookup user from name(string).
%%
%% @end
%%--------------------------------------------------------------------
-spec(lookup_name(Name::atom() | string()) -> 
             {ok, #user{}} | {error, not_found}).

lookup_name(Name) when is_list(Name)->
    gen_server:call({global, ?SERVER}, {lookup_name, list_to_atom(Name)});

lookup_name(Name) when is_atom(Name) ->
    gen_server:call({global, ?SERVER}, {lookup_name, Name}).

%%--------------------------------------------------------------------
%%
%% @doc lookup user from pid.
%%
%% @end
%%--------------------------------------------------------------------
-spec(lookup_pid(Pid::pid()) -> {ok, #user{}} | {error, not_found}).

lookup_pid(Pid) ->
    gen_server:call({global, ?SERVER}, {lookup_pid, Pid}).

%%--------------------------------------------------------------------
%%
%% @doc exec fun to each element of user list.
%%
%% @end
%%--------------------------------------------------------------------
-spec(map_do(fun()) -> ok).

map_do(Fun) ->
    gen_server:cast({global, ?SERVER}, {map_do, Fun}).

%%--------------------------------------------------------------------
%%
%% @doc save user pid.
%%
%% @end
%%--------------------------------------------------------------------
-spec(save_pid(Id::integer(), Pid::pid()) -> ok | {error, not_found}).

save_pid(Id, Pid) ->
    gen_server:call({global, ?SERVER}, {save_pid, Id, Pid}).

%%--------------------------------------------------------------------
%%
%% @doc lookup user pid.
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_pid(UserName_OR_Id::integer() | atom() | string()) -> 
             {ok, #user{}} | {error, not_found}).

get_pid(UserName_OR_Id) when is_list(UserName_OR_Id)  ->
    get_pid(list_to_atom(UserName_OR_Id));

get_pid(UserName_OR_Id) ->
    gen_server:call({global, ?SERVER}, {get_pid, UserName_OR_Id}).    

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
handle_call({add_user, Name, Mail, Password}, _From, State) ->
    Reply = case get_user_by_name(Name) of
                {ok, _User} -> {error, already_exist};
                {error, not_found} ->
                    NextUserId = case ets:last(userRam) of
                                     '$end_of_table' -> 1;
                                     UserId -> UserId + 1
                                 end,
                    User = #user{id=NextUserId, name=Name, status=true,
                                 mail=Mail, password=undefined},
                    MD5Password = util:get_md5_password(User, Password),
                    NewUser = User#user{password=MD5Password},
                    
                    ets:insert(userRam, NewUser),
                    dets:insert(userDisk, NewUser),
                    {ok, NewUser}
            end,

    {reply, Reply, State};

handle_call({update_user, User}, _From, State) ->
    Reply = case get_user_by_id(User#user.id) of
                {ok, _} ->
                    ets:insert(userRam, User),
                    dets:insert(userDisk, User),
                    {ok, User};
                {error, not_found} -> {error, not_found}
            end,
    
    {reply, Reply, State};

handle_call({lookup_id, Id}, _From, State) ->
    Reply = get_user_by_id(Id),
    {reply, Reply, State};

handle_call({lookup_name, Name}, _From, State) ->
    Reply = get_user_by_name(Name),
    {reply, Reply, State};

handle_call({lookup_pid, Pid}, _From, State) ->
    Reply = get_user_by_pid(Pid),
    {reply, Reply, State};

handle_call({save_pid, Id, Pid}, _From, State) ->
    Reply =     case get_user_by_id(Id) of
                    {ok, User} ->
                        UpdatedUser = User#user{pid=Pid},
                        ets:insert(userRam, UpdatedUser),
                        dets:insert(userDisk, UpdatedUser),
                        ok;
                    {error, not_found} -> {error, not_found}
                end,

    {reply, Reply, State};

handle_call({get_pid, UserName}, _From, State) when is_atom(UserName) ->
    Reply = case get_user_by_name(UserName) of
                {ok, User} -> {ok, User#user.pid};
                Other -> Other
            end,
    
    {reply, Reply, State};

handle_call({get_pid, UserId}, _From, State) when is_integer(UserId) ->
    Reply = case get_user_by_id(UserId) of
                {ok, User} -> {ok, User#user.pid};
                Other -> Other
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
handle_cast({map_do, Fun}, State) ->
    case ets:first(userRam) of
	'$end_of_table' ->
	    ok;
	First ->
	    [User] = ets:lookup(userRam, First),
	    Fun(User),
	    map_do(Fun, First)
    end,

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
    close_tables(),
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

-spec(create_tables(FileName::string()) -> ok).

create_tables(FileName) ->
    ets:new(userRam, [named_table, ordered_set, {keypos, #user.id}]),
    dets:open_file(userDisk, [{file, FileName}, {keypos, #user.id}]),
    ok.
    
-spec(close_tables() -> true).

close_tables() ->
    ets:delete(userRam),
    dets:close(userDisk).

-spec(restore_table() -> ok).

restore_table() ->
    Insert = fun(#user{id=_UserID, name=_UserName, status=_Status,
		       mail=_Mail, password=_Password} = User)->
		     ets:insert(userRam, User),
		     continue
	     end,
    dets:traverse(userDisk, Insert),
    ok.

-spec(get_user_by_pid(Pid::pid()) -> #user{}).

get_user_by_pid(Pid) when is_pid(Pid) ->
    Pattern = #user{id='$1', name='_', status='_', pid=Pid, 
		    mail='_', password='_'},
    case ets:match(userRam, Pattern) of
	[]-> {error, not_found};
	[[UserId]] -> get_user_by_id(UserId)
    end.

-spec(get_user_by_name(Name::atom()) -> #user{}).

get_user_by_name(Name) when is_atom(Name) ->
    Pattern = #user{id='$1', name=Name, status='_', pid='_', 
		    mail='_', password='_'},
    case ets:match(userRam, Pattern) of
	[]-> {error, not_found};
	[[UserId]] -> get_user_by_id(UserId)
    end.

-spec(get_user_by_id(Id::integer()) -> #user{}).

get_user_by_id(Id) when is_integer(Id) ->
    case ets:lookup(userRam, Id) of
	[] -> {error, not_found};
	[User] -> {ok, User}
    end.

map_do(Fun, Entry) ->
    case ets:next(userRam, Entry) of
	'$end_of_table' ->
	    ok;
	Next ->
	    [User] = ets:lookup(userRam, Next),
	    Fun(User),
	    map_do(Fun, Next)
    end.
