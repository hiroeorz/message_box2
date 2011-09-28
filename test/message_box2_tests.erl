-module(message_box2_tests).
-include_lib("eunit/include/eunit.hrl").

%% Include
-include("message_box.hrl").
-include("message.hrl").
-include("user.hrl").

-define(TEST_MYSQL_POOL,       message_box_mysql).

-define(Setup, fun() -> 
                       message_box2_config:load(),
                       mnesia:create_schema([node()]),
                       mnesia:start(),
                       mnesia:create_table(user, [{ram_copies, [node()]}, 
                                     {type, set},
                                     {attributes, record_info(fields, user)}]),
                       
                       mnesia:create_table(follow, [{ram_copies, [node()]}, 
                                     {type, set},
                                     {attributes, record_info(fields, follow)},
                                     {index, [id]}]),
                       mmysql:init_for_test(),
                       message_box2_sup:start_link()
	       end).

-define(Clearnup, fun(_) ->
                          drop_users_tables(),
                          application:stop(emysql)
		  end).

all_test_() ->
    {inorder,
     {setup, ?Setup, ?Clearnup,
      [
       { "新規ユーザを登録する",
         fun() ->
                 ?assertMatch({ok, _User},
                              message_box2:create_user("shin", "shin@mail", 
                                                       "password1")),

                 ?assertMatch({ok, _User},
                              message_box2:create_user("hiroe", "hiroe@mail", 
                                                       "password2")),

                 ?assertMatch({ok, _User},
                              message_box2:create_user("matsu", "matsu@mail", 
                                                       "password3"))
         end
       },

       { "ユーザ1をチェックする",
         fun() ->
                 {ok, User} = message_box2_user_db:lookup_id(1),
                 ?assertEqual(1, User#user.id),
                 ?assertEqual(<<"shin">>, User#user.name),
                 ?assertEqual(<<"shin@mail">>, User#user.mail)
         end
       },

       { "ユーザ2をチェックする",
         fun() ->
                 {ok, User} = message_box2_user_db:lookup_id(2),
                 ?assertEqual(2, User#user.id),
                 ?assertEqual(<<"hiroe">>, User#user.name),
                 ?assertEqual(<<"hiroe@mail">>, User#user.mail)
         end
       },

       { "ユーザ3をチェックする",
         fun() ->
                 {ok, User} = message_box2_user_db:lookup_id(3),
                 ?assertEqual(3, User#user.id),
                 ?assertEqual(<<"matsu">>, User#user.name),
                 ?assertEqual(<<"matsu@mail">>, User#user.mail)
         end
       },

       { "メッセージを投稿する",
         fun() ->
                 {ok, _Message} = 
                     message_box2:send_message(1, "password1", 
                                               <<"hello everyone! :-)">>)
         end
       },

       { "送信済みメッセージを取得する",
         fun() ->
                 {ok, Messages} = message_box2:get_sent_timeline(1, 100),
                 ?assertEqual(true, is_list(Messages)),
                 ?assertEqual(1, length(Messages))
         end
       }

      ]
     }
    }.

%%%===================================================================
%%% Internal functions
%%%===================================================================

drop_users_tables() ->
    Fun = fun(User) ->
                  UserId = User#user.id,
                  mmysql:execute("drop table user_~w_home", [UserId]),
                  mmysql:execute("drop table user_~w_mentions", [UserId]),
                  mmysql:execute("drop table user_~w_messages", [UserId])
          end,
    message_box2_user_db:map_do(Fun).
