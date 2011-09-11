%% File : message_box.hrl
%% Description : Include file for configurations of application. 

-type(tid() ::integer()).

-define(MESSAGE_ID_LENGTH, 18).
-define(USER_ID_LENGTH, 9).


-record(result_packet, {seq_num, field_list, rows, extra}). %% mysql result
-define(OK_PACKET, {ok_packet, _,_,_,_,_,_}).
