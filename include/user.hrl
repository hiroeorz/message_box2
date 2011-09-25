%% File : usr.hrl
%% Description : Include file for user_db

%% mysql
-record(user, {id              ::non_neg_integer(),
	       status = true   ::atom(),
	       pid             ::pid(),
	       name            ::binary(),
	       mail            ::binary(),
	       password        ::binary()
	      }).           


-record(follow, {user_id       ::non_neg_integer(),
                 id            ::non_neg_integer(),
		 datetime      ::calendar:t_now()
                }).

