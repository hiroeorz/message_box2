%% File : follow_db.erl
%% Description : user follow relationship database.

-module(follower_db).
-include_lib("eunit/include/eunit.hrl").
-include("../include/message_box.hrl").
-include("../include/message.hrl").
-include("../include/user.hrl").

-export([init/1]).
-export([close_tables/2, save_follower_user/3, delete_follower_user/3,
        get_follower_ids/1, map_do/2, is_follower/2]).

%%--------------------------------------------------------------------
%%
%% @doc load follower users from dets to ets.
%%
%% @end
%%--------------------------------------------------------------------
-spec(init(UserName::atom()) -> {ok, Tid::tid()}).

init(UserName) ->
    process_flag(trap_exit, true),

    DB_DIR = message_box2_config:get(database_dir),
    case file:make_dir(DB_DIR ++ atom_to_list(UserName)) of
        ok -> ok;
        {error, eexist} -> ok
    end,

    {ok, Tid} = create_tables(UserName),
    restore_table(Tid, UserName),
    {ok, Tid}.

    
%%--------------------------------------------------------------------
%%
%% @doc create table ets and dets.
%%
%% @end
%%--------------------------------------------------------------------
-spec(create_tables(Device::atom()) -> {ok, Tid::tid(), DetsPid::pid()}).

create_tables(UserName) ->  
    Tid = ets:new(follower, [ordered_set, {keypos, #follower.id}]),
    {DiscName, FileName} = dets_info(UserName),
    dets:open_file(DiscName, [{file, FileName}, {keypos, #follower.id}]),
    {ok, Tid}.

%%--------------------------------------------------------------------
%%
%% @doc load follower users from dets to ets.
%%
%% @end
%%--------------------------------------------------------------------
-spec(restore_table(Tid::tid(), UserName::atom()) -> ok).

restore_table(Tid, UserName) ->
    Insert = fun(#follower{id=_Id, datetime=_DateTime} = Follower)->
		     ets:insert(Tid, Follower),
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
-spec(close_tables(Tid::tid(), UserName::atom()) -> 
             ok | {error, Reason::term()}).

close_tables(Tid, UserName) ->
    {Dets, _} = dets_info(UserName),
    ets:delete(Tid),
    dets:close(Dets).

%%--------------------------------------------------------------------
%%
%% @doc save user to follower database.
%%
%% @end
%%--------------------------------------------------------------------
-spec(save_follower_user(Tid::tid(), User::#user{}, Id::integer()) ->
             ok | {error, already_followering}).

save_follower_user(Tid, User, Id) ->
    Follower = #follower{id=Id, datetime={date(), time()}},

    case is_followering(User, Id) of
	true -> {error, already_followering};
	false ->
            {Dets, _} = dets_info(User#user.name),
	    ets:insert(Tid, Follower),
	    dets:insert(Dets, Follower),
	    ok
    end.

%%--------------------------------------------------------------------
%%
%% @doc delet user from follower database.
%%
%% @end
%%--------------------------------------------------------------------
-spec(delete_follower_user(Tid::tid(), User::#user{}, Id::integer()) -> 
             {ok, deleted} | {error, not_followering}).

delete_follower_user(Tid, User, Id) ->
    case is_followering(Tid, Id) of
	true ->
	    {Dets, _} = dets_info(User#user.name),
	    ets:delete(Tid, Id),
	    dets:delete(Dets, Id),
	    {ok, deleted};
	false -> {error, not_followering}
    end.

%%--------------------------------------------------------------------
%%
%% @doc get all follower users id.
%%
%% @end
%%--------------------------------------------------------------------
-spec(get_follower_ids(Tid::tid()) -> [#follower{}]).

get_follower_ids(Tid) ->
    case ets:first(Tid) of
	'$end_of_table' -> [];
	First -> collect_id(Tid, First, [First])
    end.

%%--------------------------------------------------------------------
%%
%% @doc do function to all users.
%%
%% @end
%%--------------------------------------------------------------------
-spec(map_do(Tid::tid(), Fun::fun()) -> ok).

map_do(Tid, Fun) ->
    case ets:first(Tid) of
	'$end_of_table' ->
	    ok;
	First ->
	    [Follower] = ets:lookup(Tid, First),
	    Fun(Follower),
	    map_do(Tid, Fun, First)
    end.

%%--------------------------------------------------------------------
%%
%% @doc check followerin user or not.
%%
%% @end
%%--------------------------------------------------------------------
-spec(is_follower(Tid::tid(), UserId::integer()) -> true|false).

is_follower(Tid, UserId) ->
    case ets:lookup(Tid, UserId) of
        [_FolloweringUser] -> true;
        [] -> false
    end.

%%--------------------------------------------------------------------
%% local functions
%%--------------------------------------------------------------------

-spec(collect_id(Tid::tid(), Before::integer(), Result::[integer()]) ->
             [integer()]).

collect_id(Tid, Before, Result) ->
    case ets:next(Tid, Before) of
	'$end_of_table' -> Result;
	FollowerId -> collect_id(Tid, FollowerId, [FollowerId | Result])
    end.	    

-spec(is_followering(Tid::tid(), Id::integer()) -> true | false).

is_followering(Tid, Id) ->
    case ets:lookup(Tid, Id) of
	[_Follower] -> true;
	[] -> false
    end.    

-spec(dets_info(UserName::atom()) -> {Dets::atom(), FileName::string()}).

dets_info(UserName)->
    Dets = list_to_atom(atom_to_list(UserName) ++ "_FollowerDisc"),
    DB_DIR = message_box2_config:get(database_dir),
    FileName = DB_DIR ++ atom_to_list(UserName) ++ "follower",
    {Dets, FileName}.

-spec(map_do(Tid::tid(), Fun::fun(), Entry::integer()) -> term()).

map_do(Tid, Fun, Entry) ->
    case ets:next(Tid, Entry) of
	'$end_of_table' ->
	    ok;
	Next ->
	    [Follower] = ets:lookup(Tid, Next),
	    Fun(Follower),
	    map_do(Tid, Fun, Next)
    end.
