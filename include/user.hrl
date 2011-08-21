%% File : usr.hrl
%% Description : Include file for user_db

-record(user, {id              ::integer(),
	       status = true   ::atom(),
	       pid             ::atom(),
	       name            ::term(),
	       mail            ::string(),
	       password        ::string()
	      }).           


-record(follower, {id          ::integer(),
		   datetime    ::term()
                  }).

-record(follow, {id            ::integer(),
		 datetime      ::term()
                }).

