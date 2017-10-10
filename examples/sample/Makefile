BUILD_PATH := _build
LIB_PATH := $(BUILD_PATH)/default/lib

.PHONY: all deps compile compile-director shell clean distclean

all: compile shell

deps:
	@../../rebar3 get-deps

compile: compile-director
	@../../rebar3 compile

compile-director:
	@cd ../../ && ./rebar3 compile

shell: compile
	@erl -pa $(LIB_PATH)/sample/ebin \
	         ../../$(LIB_PATH)/director/ebin

clean:
	@../../rebar3 clean

distclean:
	@rm -rf $(BUILD_PATH)
