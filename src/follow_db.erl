%% File : follow_db.erl
%% Description : user follow relationship database.

-module(follow_db).
-include_lib("eunit/include/eunit.hrl").
-include("../include/message_box.hrl").
-include("../include/message.hrl").
-include("../include/user.hrl").

-export([init/1]).
-export([close_tables/2, save_follow_user/3, delete_follow_user/3,
        get_follow_ids/1, map_do/2, is_follow/2]).

%%--------------------------------------------------------------------
%%
%% @doc load follow users from dets to ets.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(UserName::atom()) -> {ok, EtsPid::pid()}).

init(UserName) ->
    process_flag(trap_exit, true),

    DB_DIR = message_box2_config:get(database_dir),
    case file:make_dir(DB_DIR ++ atom_to_list(UserName)) of
        ok -> ok;
        {error, eexist} -> ok
    end,

    {ok, EtsPid} = create_tables(UserName),
    restore_table(EtsPid, UserName),
    {ok, EtsPid}.

    
%%--------------------------------------------------------------------
%%
%% @doc create table ets and dets.
%%
%% @end
%%--------------------------------------------------------------------
-spec(create_tables(Device::atom()) -> {ok, EtsPid::pid(), DetsPid::pid()}).

create_tables(UserName) ->  
    EtsPid = ets:new(follow, [ordered_set, {keypos, #follow.id}]),
    {DiscName, FileName} = dets_info(UserName),
    dets:open_file(DiscName, [{file, FileName}, {keypos, #follow.id}]),
    {ok, EtsPid}.

%%--------------------------------------------------------------------
%%
%% @doc load follow users from dets to ets.
%%
%% @end
%%--------------------------------------------------------------------
-spec(restore_table(EtsPid::pid(), UserName::atom()) -> ok).

restore_table(EtsPid, UserName) ->
    Insert = fun(#follow{id=_Id, datetime=_DateTime} = Follow)->
		     ets:insert(EtsPid, Follow),
		     continue
	     end,

    {Dets, _} = dets_info(UserName),
    dets:traverse(Dets, Insert),
    ok.

%%--------------------------------------------------------------------
%%
%% @doc close ets and dets database.
%%
%% @end
%%--------------------------------------------------------------------
-spec(close_tables(EtsPid::pid(), UserName::atom()) -> 
             ok | {error, Reason::term()}).

close_tables(EtsPid, UserName) ->
    {Dets, _} = dets_info(UserName),
    ets:delete(EtsPid),
    dets:close(Dets).

%%--------------------------------------------------------------------
%%
%% @doc save user to follow database.
%%
%% @end
%%--------------------------------------------------------------------
-spec(save_follow_user(EtsPid::pid(), User::#user{}, Id::integer()) ->
             ok | {error, already_following}).

save_follow_user(EtsPid, User, Id) ->
    Follow = #follow{id=Id, datetime={date(), time()}},

    case is_following(User, Id) of
	true -> {error, already_following};
	false ->
            {Dets, _} = dets_info(User#user.name),
	    ets:insert(EtsPid, Follow),
	    dets:insert(Dets, Follow),
	    ok
    end.

%%--------------------------------------------------------------------
%%
%% @doc delet user from follow database.
%%
%% @end
%%--------------------------------------------------------------------
-spec(delete_follow_user(EtsPid::pid(), User::#user{}, Id::integer()) -> 
             {ok, deleted} | {error, not_following}).

delete_follow_user(EtsPid, User, Id) ->
    case is_following(EtsPid, Id) of
	true ->
	    {Dets, _} = dets_info(User#user.name),
	    ets:delete(EtsPid, Id),
	    dets:delete(Dets, Id),
	    {ok, deleted};
	false -> {error, not_following}
    end.

%%--------------------------------------------------------------------
%%
%% @doc get all follow users id.
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_follow_ids(EtsPid::pid()) -> [#follow{}]).

get_follow_ids(EtsPid) ->
    case ets:first(EtsPid) of
	'$end_of_table' -> [];
	First -> collect_id(EtsPid, First, [First])
    end.

%%--------------------------------------------------------------------
%%
%% @doc do function to all users.
%%
%% @end
%%--------------------------------------------------------------------
-spec(map_do(EtsPid::pid(), Fun::fun()) -> ok).

map_do(EtsPid, Fun) ->
    case ets:first(EtsPid) of
	'$end_of_table' ->
	    ok;
	First ->
	    [Follow] = ets:lookup(EtsPid, First),
	    Fun(Follow),
	    map_do(EtsPid, Fun, First)
    end.

%%--------------------------------------------------------------------
%%
%% @doc check followin user or not.
%%
%% @end
%%--------------------------------------------------------------------
-spec(is_follow(EtsPid::pid(), UserId::integer()) -> true|false).

is_follow(EtsPid, UserId) ->
    case ets:lookup(EtsPid, UserId) of
        [_FollowingUser] -> true;
        [] -> false
    end.

%%--------------------------------------------------------------------
%% local functions
%%--------------------------------------------------------------------

collect_id(EtsPid, Before, Result) ->
    case ets:next(EtsPid, Before) of
	'$end_of_table' -> Result;
	FollowId -> collect_id(EtsPid, FollowId, [FollowId | Result])
    end.	    

-spec(is_following(EtsPid::pid(), Id::integer()) -> true | false).

is_following(EtsPid, Id) ->
    case ets:lookup(EtsPid, Id) of
	[_Follow] -> true;
	[] -> false
    end.    

dets_info(UserName)->
    DiscName = list_to_atom(atom_to_list(UserName) ++ "_FollowDisc"),
    DB_DIR = message_box2_config:get(database_dir),
    FileName = DB_DIR ++ atom_to_list(UserName) ++ "follow",
    {DiscName, FileName}.

map_do(Device, Fun, Entry) ->
    case ets:next(Device, Entry) of
	'$end_of_table' ->
	    ok;
	Next ->
	    [Follow] = ets:lookup(Device, Next),
	    Fun(Follow),
	    map_do(Device, Fun, Next)
    end.
