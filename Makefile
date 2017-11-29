BUILD_PATH := _build
LIB_PATH := $(BUILD_PATH)/default/lib

.PHONY: all compile compile-examples shell docs test clean distclean md2html push

all: compile

compile: compile-examples
	@./rebar3 compile

compile-examples:
	@cd examples/sample && ../../rebar3 compile
	@cd examples/ETS && ../../rebar3 compile
	@cd examples/Mnesia && ../../rebar3 compile
	@cd examples/supervision-flexibility && ../../rebar3 compile

shell: compile
	@erl -pa $(LIB_PATH)/director/ebin \
	         examples/sample/$(LIB_PATH)/sample/ebin \
	         examples/ETS/$(LIB_PATH)/sample_ets/ebin \
	         examples/Mnesia/$(LIB_PATH)/sample_mnesia/ebin \
	         examples/supervision-flexibility/$(LIB_PATH)/sample_sf/ebin

docs:
	@./rebar3 edoc

test: compile-examples
	./rebar3 ct && ./rebar3 dialyzer

clean:
	@./rebar3 clean
	@rm -rf ./README.html
	@rm -rf ./wiki/*.html
	@cd ./examples/sample && ../../rebar3 clean
	@cd ./examples/ETS && ../../rebar3 clean
	@cd ./examples/Mnesia && ../../rebar3 clean
	@cd ./examples/supervision-flexibility && ../../rebar3 clean
	@rm -rf ./examples/*/README.html

distclean: clean
	@rm -rf $(BUILD_PATH)
	@rm -rf examples/*/$(BUILD_PATH)

md2html:
	@grip ./README.md --export ./README.html --title "Director"
	
	@mkdir -p ./wiki/html
	@grip ./wiki/overview.md --export ./wiki/overview.html --title "Director wiki - Overview"
	@grip ./wiki/director-behavior.md --export ./wiki/director-behavior.html --title "Director wiki - Behavior"
	@grip ./wiki/build.md --export ./wiki/build.html --title "Director wiki - Build"	
	
	@grip ./examples/sample/README.md --export ./examples/sample/README.html --title "Director - Sample"
	
	@grip ./examples/ETS/README.md --export ./examples/ETS/README.html --title "Director - Sample ETS usage"
	
	@grip ./examples/Mnesia/README.md --export ./examples/Mnesia/README.html --title "Director - Sample Mnesia usage"
	
	@grip ./examples/supervision-flexibility/README.md --export ./examples/supervision-flexibility/README.html --title "Director - Sample supervision flexibility"

push: test
	git push origin master
