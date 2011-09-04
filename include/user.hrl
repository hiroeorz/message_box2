%% File : usr.hrl
%% Description : Include file for user_db

-record(user, {id              ::integer(),
	       status = true   ::atom(),
	       pid             ::pid(),
	       name            ::binary(),
	       mail            ::binary(),
	       password        ::binary()
	      }).           


-record(follower, {user_id     ::integer(),
                   id          ::integer(),
		   datetime    ::term()
                  }).

-record(follow, {user_id       ::integer(),
                 id            ::integer(),
		 datetime      ::term()
                }).

