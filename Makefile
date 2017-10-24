BUILD_PATH := _build
LIB_PATH := $(BUILD_PATH)/default/lib

.PHONY: all compile compile-examples shell docs test clean distclean push

all: compile

compile: compile-examples
	./rebar3 compile

compile-examples:
	@cd examples/sample && ../../rebar3 compile
	@cd examples/tables/ETS && ../../../rebar3 compile
	@cd examples/tables/Mnesia && ../../../rebar3 compile

shell: compile
	@erl -pa $(LIB_PATH)/director/ebin \
	         examples/sample/$(LIB_PATH)/sample/ebin \
	         examples/tables/ETS/$(LIB_PATH)/sample_ets/ebin \
	         examples/tables/Mnesia/$(LIB_PATH)/sample_mnesia/ebin

docs:
	@./rebar3 edoc

test: compile-examples
	./rebar3 ct && ./rebar3 dialyzer

clean:
	@./rebar3 clean
	@cd examples/sample && ../../rebar3 clean
	@cd examples/tables/ETS && ../../../rebar3 clean
	@cd examples/tables/Mnesia && ../../../rebar3 clean

distclean:
	@rm -rf $(BUILD_PATH)
	@rm -rf examples/sample/$(BUILD_PATH)
	@rm -rf examples/tables/ETS/$(BUILD_PATH)
	@rm -rf examples/tables/Mnesia/$(BUILD_PATH)
	
push: test
	git push origin master
