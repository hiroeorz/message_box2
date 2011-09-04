%%%-------------------------------------------------------------------
%%% @author Hiroe Shin <hiroe.orz@gmail.com>
%%% @copyright (C) 2011, Hiroe Shin
%%% @doc
%%%
%%% @end
%%% Created : 21 Aug 2011 by Hiroe Shin <hiroe.orz@gmail.com>
%%%-------------------------------------------------------------------
-module(message_box2_user_db).

%% API
-export([add_user/3, update_user/1, lookup_id/1, lookup_name/1, lookup_pid/1,
         map_do/1, save_pid/2, get_pid/1]).

%% Include
-include("../include/user.hrl").


-define(SERVER, ?MODULE). 

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%%
%% @doc add new user to system.
%%
%% @end
%%--------------------------------------------------------------------
-spec(add_user(Name::string(), Mail::string(), Password::string()) -> 
             {ok, #user{}} | {error, already_exist}).

add_user(Name, Mail, Password) when is_list(Name) and is_list(Mail) and 
                                    is_list(Password) ->
    add_user(list_to_binary(Name), list_to_binary(Mail), Password);

add_user(Name, Mail, Password) when is_binary(Name) and is_binary(Mail) and 
                                    is_list(Password) ->
    case get_user_by_name(Name) of
        {ok, _User} -> {error, already_exist};
        {error, not_found} ->
            Last = mnesia:activity(async_dirty, 
                                   fun() -> mnesia:last(user) end),
            NextUserId = case Last of
                             '$end_of_table' -> 1;
                             UserId -> UserId + 1
                         end,

            User = #user{id=NextUserId, name=Name, status=true,
                         mail=Mail, password=undefined},
            MD5Password = util:get_md5_password(User, Password),
            NewUser = User#user{password=MD5Password},
                    
            mnesia:activity(transaction, 
                            fun() -> mnesia:write(NewUser) end),

            {ok, NewUser}
    end.

%%--------------------------------------------------------------------
%%
%% @doc add new user to system.
%%
%% @end
%%--------------------------------------------------------------------
-spec(update_user(#user{}) -> {ok, #user{}} | {error, not_found}).

update_user(User) ->
    case get_user_by_id(User#user.id) of
        {ok, _} ->
            mnesia:activity(transaction, 
                            fun() -> mnesia:write(User) end),
            {ok, User};
        {error, not_found} -> {error, not_found}
    end.

%%--------------------------------------------------------------------
%%
%% @doc lookup user from id.
%%
%% @end
%%--------------------------------------------------------------------
-spec(lookup_id(integer()) -> {ok, #user{}} | {error, not_found}).

lookup_id(Id)->
    get_user_by_id(Id).

%%--------------------------------------------------------------------
%%
%% @doc lookup user from name(string).
%%
%% @end
%%--------------------------------------------------------------------
-spec(lookup_name(Name::atom() | string()) -> 
             {ok, #user{}} | {error, not_found}).

lookup_name(Name) when is_list(Name)->
    lookup_name(list_to_binary(Name));

lookup_name(Name) when is_binary(Name) ->
    get_user_by_name(Name).

%%--------------------------------------------------------------------
%%
%% @doc lookup user from pid.
%%
%% @end
%%--------------------------------------------------------------------
-spec(lookup_pid(Pid::pid()) -> {ok, #user{}} | {error, not_found}).

lookup_pid(Pid) ->
    get_user_by_pid(Pid).

%%--------------------------------------------------------------------
%%
%% @doc exec fun to each element of user list.
%%
%% @end
%%--------------------------------------------------------------------
-spec(map_do(fun()) -> ok).

map_do(Fun) ->
    case mnesia:activity(async_dirty, fun() -> mnesia:first(user) end) of
	'$end_of_table' ->
	    ok;
	First ->
            [User] = mnesia:activity(async_dirty, 
                                     fun() -> mnesia:read(user, First) end),
	    Fun(User),
	    map_do(Fun, First)
    end.

%%--------------------------------------------------------------------
%%
%% @doc save user pid.
%%
%% @end
%%--------------------------------------------------------------------
-spec(save_pid(Id::integer(), Pid::pid()) -> ok | {error, not_found}).

save_pid(Id, Pid) ->
    case get_user_by_id(Id) of
        {ok, User} ->
            UpdatedUser = User#user{pid=Pid},
            mnesia:activity(transaction,
                            fun() -> mnesia:write(UpdatedUser) end),
            ok;
        {error, not_found} -> {error, not_found}
    end.

%%--------------------------------------------------------------------
%%
%% @doc lookup user pid.
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_pid(UserName_OR_Id::integer() | atom() | string()) -> 
             {ok, #user{}} | {error, not_found}).

get_pid(UserName_OR_Id) when is_list(UserName_OR_Id)  ->
    get_pid(list_to_binary(UserName_OR_Id));

get_pid(UserName_OR_Id) when is_binary(UserName_OR_Id)->
    case get_user_by_name(UserName_OR_Id) of
        {ok, User} -> {ok, User#user.pid};
        Other -> Other
    end;

get_pid(UserName_OR_Id) when is_integer(UserName_OR_Id)->
    case get_user_by_id(UserName_OR_Id) of
        {ok, User} -> {ok, User#user.pid};
        Other -> Other
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec(get_user_by_pid(Pid::pid()) -> #user{}).

get_user_by_pid(Pid) when is_pid(Pid) ->
    Pattern = #user{id='$1', name='_', status='_', pid=Pid, 
		    mail='_', password='_'},

    case mnesia:activity(async_dirty, 
                         fun() -> mnesia:match_object(Pattern) end) of
	[]-> {error, not_found};
	[User] -> {ok, User}
    end.

-spec(get_user_by_name(Name::binary()) -> #user{}).

get_user_by_name(Name) when is_binary(Name) ->
    Pattern = #user{id='$1', name=Name, status='_', pid='_', 
		    mail='_', password='_'},

    case mnesia:activity(async_dirty, 
                         fun() -> mnesia:match_object(Pattern) end) of
	[]-> {error, not_found};
	[User] -> {ok, User}
    end.

-spec(get_user_by_id(Id::integer()) -> #user{}).

get_user_by_id(Id) when is_integer(Id) ->
    case mnesia:activity(async_dirty, fun() -> mnesia:read(user, Id) end) of
	[] -> {error, not_found};
	[User] -> {ok, User}
    end.

map_do(Fun, Entry) ->
    case mnesia:activity(async_dirty, fun() -> mnesia:next(user, Entry) end) of
	'$end_of_table' ->
	    ok;
	Next ->
	    [User] = mnesia:activity(async_dirty, 
                                     fun() -> mnesia:read(user, Next) end),
	    Fun(User),
	    map_do(Fun, Next)
    end.
