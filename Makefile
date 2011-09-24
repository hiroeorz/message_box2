ERL=erl
BEAMDIR=./deps/*/ebin ./ebin
REBAR=./rebar

APP_NAME=message_box2
HOST_NAME=127.0.0.1
DB_DIR=/var/message_box2/db

all: clean compile xref

update-deps:
	@$(REBAR) update-deps

get-deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

xref:
	@$(REBAR) xref

clean: 
	@ $(REBAR) clean

check:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit 

edoc:
	@$(REBAR) doc

boot:
	@ mkdir -p $(DB_DIR)
	@ $(ERL) -pa $(BEAMDIR) -sname $(APP_NAME) \
                 -mnesia dir '"$(DB_DIR)"' \
                 -boot start_sasl \
                 -s $(APP_NAME) start

boot_test:
	@ mkdir -p $(DB_DIR)
	@ $(ERL) -pa $(BEAMDIR) -sname $(APP_NAME) \
                 -boot start_sasl \
                 -s $(APP_NAME) start