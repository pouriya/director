BUILD_PATH := _build
LIB_PATH := $(BUILD_PATH)/default/lib

.PHONY: all compile compile-director foo bar clean distclean

all: compile shell

compile: compile-director
	@../../rebar3 compile

compile-director:
	@cd ../../ && ./rebar3 compile

foo: compile
	@erl -pa $(LIB_PATH)/sample_mnesia/ebin \
	         ../../$(LIB_PATH)/director/ebin \
	         -sname foo@localhost

bar: compile
	@erl -pa $(LIB_PATH)/sample_mnesia/ebin \
	         ../../$(LIB_PATH)/director/ebin \
	         -sname bar@localhost

clean:
	@../../rebar3 clean

distclean:
	@rm -rf $(BUILD_PATH)
