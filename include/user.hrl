%% File : usr.hrl
%% Description : Include file for user_db

-record(user, {id              ::integer(),
	       status = true   ::atom(),
	       pid             ::atom(),
	       name            ::term(),
	       mail            ::string(),
	       password        ::string()
	      }).           


-record(follower, {user_id     ::integer(),
                   id          ::integer(),
		   datetime    ::term()
                  }).

-record(follow, {user_id       ::integer(),
                 id            ::integer(),
		 datetime      ::term()
                }).

