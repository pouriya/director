REBAR = $(CURDIR)/rebar3
BUILD_PATH := _build
LIB_PATH := $(BUILD_PATH)/default/lib

.PHONY: all compile compile-examples shell docs test clean distclean

all: compile

compile: compile-examples
	@ $(REBAR) compile

compile-examples:
	@ echo Todo: after adding examples, compile them here

shell: compile
	@ erl -pa $(LIB_PATH)/director/ebin

docs:
	@ $(REBAR) as edown edoc

test: compile-examples
	$(REBAR) ct && $(REBAR) dialyzer

clean:
	@ $(REBAR) clean

distclean: clean
	@ rm -rf $(BUILD_PATH)
