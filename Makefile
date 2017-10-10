BUILD_PATH := _build
LIB_PATH := $(BUILD_PATH)/default/lib

.PHONY: all deps compile compile-examples shell docs test clean distclean

all: compile

deps:
	@./rebar3 get-deps

compile: compile-examples
	@./rebar3 compile

compile-examples:
	@cd examples/sample && ../../rebar3 compile

shell: compile-examples
	@erl -pa $(LIB_PATH)/director/ebin \
	         examples/sample/$(LIB_PATH)/sample/ebin

docs:
	@./rebar3 edoc

test: compile-examples
	./rebar3 ct && ./rebar3 dialyzer

clean:
	@./rebar3 clean
	@cd examples/sample && ../../rebar3 clean

distclean:
	@rm -rf $(BUILD_PATH)
	@rm -rf examples/*/$(BUILD_PATH)
