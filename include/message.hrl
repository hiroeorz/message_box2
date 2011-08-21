%% File : message.hrl
%% Description : Include file for message_db

-define(DatetimeFormat, 
	"~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B").

-record(message, {id                   ::integer(),
		  message_id           ::integer(),
		  text                 ::binary(),
		  datetime             ::tuple(),
                  user = undefined     %% #user{}
                 }).      

-record(message_index, {id             ::integer(),
			message_id     ::integer()
                       }).
