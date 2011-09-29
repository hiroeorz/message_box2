-module(message_box2_tests).
-include_lib("eunit/include/eunit.hrl").

%% Include
-include("message_box.hrl").
-include("message.hrl").
-include("user.hrl").

-export([create_many_users/1, base_setup/0]).

-define(Setup, fun() -> base_setup() end).
-define(Clearnup, fun(_) ->
                          drop_users_tables(),
                          application:stop(emysql)
		  end).

basic_test_() ->
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

       { "ユーザ1の存在をチェックする",
         fun() ->
                 {ok, User} = message_box2_user_db:lookup_id(1),
                 ?assertEqual(1, User#user.id),
                 ?assertEqual(<<"shin">>, User#user.name),
                 ?assertEqual(<<"shin@mail">>, User#user.mail)
         end
       },

       { "ユーザ2の存在をチェックする",
         fun() ->
                 {ok, User} = message_box2_user_db:lookup_id(2),
                 ?assertEqual(2, User#user.id),
                 ?assertEqual(<<"hiroe">>, User#user.name),
                 ?assertEqual(<<"hiroe@mail">>, User#user.mail)
         end
       },

       { "ユーザ3の存在をチェックする",
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
       },

       { "hiroeはまだshinをフォローしていない",
         fun() ->
                 false = message_box2:is_following(2, 1)
         end
       },

       { "hiroeがshinをフォローする",
         fun() ->
                 ok = message_box2:follow(2, "password2", 1)
         end
       },

       { "hiroeはshinをフォローしている",
         fun() ->
                 true = message_box2:is_following(2, 1)
         end
       },

       { "shinがメッセージを投稿するとshinとhiroeのホームに現れる",
         fun() ->
                 {ok, MessageId} = 
                     message_box2:send_message(1, "password1", 
                                               <<"good morning :-)">>),
                 util:sleep(500), %%フォロワーへの配送は非同期なので念のため。
                 {ok, MessageList1} = message_box2:get_home_timeline(1, 10),
                 ?assertEqual(2, length(MessageList1)),
                 [Message1 | _] = MessageList1,
                 ?assertEqual(MessageId, Message1#message.message_id),

                 {ok, MessageList2} = message_box2:get_home_timeline(2, 10),
                 ?assertEqual(1, length(MessageList2)),
                 [Message2 | _] = MessageList2,
                 ?assertEqual(MessageId, Message2#message.message_id)
         end
       },

       { "shinのmentionsは空",
         fun() ->
                 ?assertEqual({ok, []},
                              message_box2:get_mentions_timeline(1, 10)) 
         end
       },

       { "hiroeからshinにリプライを送る",
         fun() ->
                 {ok, _MessageId} =
                     message_box2:send_message(2, "password2",
                                               <<"@shin hello shin :-)">>)
         end
       },

       { "shinのmentionsにメッセージが入る",
         fun() ->
                 {ok, MentionList} = message_box2:get_mentions_timeline(1, 10),
                 ?assertEqual(1, length(MentionList)),
                 [MentionMessage] = MentionList,
                 ?assertEqual(<<"@shin hello shin :-)">>, 
                              MentionMessage#message.text)
         end
       },

       { "shinはhiroeをフォローしてないのでshinのhomeに受信したリプライは表示されない",
         fun() ->
                 SendMessage = message_box2:get_latest_message(2),
                 {ok, [Message]} = message_box2:get_home_timeline(1, 1),
                 ?assertNot(SendMessage#message.message_id ==
                                Message#message.message_id)
         end
       },

       { "shinからhiroeにリプライを送る",
         fun() ->
                 {ok, _MessageId} =
                     message_box2:send_message(1, "password1",
                                               <<"@hiroe hello hiroe :-)">>)
         end
       },

       { "hiroeのmentionsにメッセージが入る",
         fun() ->
                 {ok, MentionList} = message_box2:get_mentions_timeline(2, 10),
                 ?assertEqual(1, length(MentionList)),
                 [MentionMessage] = MentionList,
                 ?assertEqual(<<"@hiroe hello hiroe :-)">>, 
                              MentionMessage#message.text)
         end
       },

       { "hiroeはshinをフォローしているshinのhomeに受信したリプライが表示される",
         fun() ->
                 SendMessage = message_box2:get_latest_message(1),
                 {ok, [Message]} = message_box2:get_home_timeline(2, 1),
                 ?assertEqual(SendMessage#message.message_id,
                              Message#message.message_id)
         end
       },

       { "hiroeがshinをリムーブする",
         fun() ->
                 ?assertEqual(ok, message_box2:unfollow(2, "password2", 1)),
                 ?assertEqual(false, message_box2:is_following(2, 1))
         end
       }

      ]
     }
    }.

%%%===================================================================
%%% Internal functions
%%%===================================================================

base_setup() ->
    {ThisFile, _} = filename:find_src(?MODULE),
    TestDir = filename:dirname(ThisFile),
    ConfFilePath = filename:absname_join(TestDir, 
                                         "../test/message_box2_test.conf"),
    message_box2_config:load(ConfFilePath),

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
    message_box2_sup:start_link().

% future use.
create_many_users(0) -> ok;
create_many_users(Count) ->
    UserName = lists:flatten(io_lib:format("test_user_~w", [Count])),
    MailAddress = lists:flatten(io_lib:format("~s@mail", [UserName])),
    Password = lists:flatten(io_lib:format("password~w", [Count])),
    {ok, _} = message_box2:create_user(UserName, MailAddress, Password),
    util:sleep(500),
    create_many_users(Count - 1).

drop_users_tables() ->
    Fun = fun(User) ->
                  UserId = User#user.id,
                  mmysql:execute("drop table user_~w_home", [UserId]),
                  mmysql:execute("drop table user_~w_mentions", [UserId]),
                  mmysql:execute("drop table user_~w_messages", [UserId])
          end,
    message_box2_user_db:map_do(Fun).
