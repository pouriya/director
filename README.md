![directror travis test status](https://travis-ci.org/Pouriya-Jahanbakhsh/director.png?branch=master)

# Welcome

![directror](director.jpg)

**Director** is a production-ready **supervisor** and **manager** for Erlang/Elixir processes that focuses on speed, performance and flexibility. Don't worry about replacing **Director** with OTP/Supervisor Because a **Director** process is responsive for all API functions of OTP/Supervisor module and has its own useful API functions too. This is more flexible than OTP/supervisor. Since **Director** calls callback-function to dealing with process crash, By changing code you can change strategy! To seeing all advantages just read this readme file.

## What is a supervisor? (for newbies)
According to the Erlang's manual:  
>A supervisor is a process that supervises other processes called child processes. A child process can either be another supervisor or a worker process. Supervisors are used to build a hierarchical process structure called a supervision tree, a nice way to structure a fault-tolerant application.  
>In Erlang we tell supervisors to start other processes. Every child process has its own options called childspec. 

## Features  
* Useful API functions:  
	* `director:get_pid(DirectorRef, ChildId)` gives pid of child if child is alive.  
	* `director:get_pids(DirectorRef)` gives pids of alive children.  
	* `director:terminate_and_delete_child(DirectorRef, ChildId)` terminates and deletes a child in one request.  
	* `director:become_supervisor(DirectorRef, Pid, ChildSpec)` makes **Director** supervisor of an alive process.  
	* `director:get_restart_count(DirectorRef, ChildId)` gives restart count of child (useful for debug). 
	* All functions not listed here.  
* **Director** is an Erlang behaviour and every callback-module of this behaviour should has following callback-functions (See "How to use?" section for detailed explanation):  
	* `init/1`: For initialization. Here you can define children database type, debug mode and childspecs of some children that you want to start them in initialize state.  
	* `handle_start/4`: Will be called after starting each child process.  
	* `handle_exit/5`: Will be called after crashing a child process. This callback-function should tell **Director** how to deal with process crash. Restart child? Restart it after time interval? Delete child from children? Do nothing? Terminate yourself?  
	* `handle_terminate/5`: Will be called when **Director** terminates a child process.  
	* `terminate/2`: Will be called for termination of **Director** itself.  
* Use different databases for keeping children. By default **Director** uses an Erlang list. It has three other modes for keeping children In a map or ETS or Mnesia table.  
* A number of **Director** processes can use one ETS table on same node (sharing ETS table).
* A number of **Director** processes can use one Mnesia table on cluster of nodes (sharing Mnesia table).
* By sharing table **Director**s can start, restart, terminate, etc a number of children simultaneously.  
* By using ETS or Mnesia you can read children info directly from table using API functions of `director_table_ets` and `director_table_mnesia` modules instead of getting them from **Director** process. For example `director_table_ets:get_pids(Tab)` or `director_table_mnesia:count_children(Tab)`.  
* You can define your own database for keeping children by implementing `director_table` behaviour. Also some test cases are ready for testing your module.  
* Understandable debug output for every operation.  

All features not listed here. For more info see Guide and examples. For contributing see [`CONTRIBUTING.md`](CONTRIBUTING.md) file.


## How to use?
 Since **Director** is an Erlang behaviour; So before explaining its workflow, I'll explain that "What a behaviour is?" for newbies.  
> In Erlang, a behaviour is a design pattern implemented in a module/library. It provides functionality in a fashion similar to inheritance in object-oriented programming. A number of functions called callback-functions must be defined for each behavior to work in a module called callback-module.  

When you want to start a **Director** process, you should specify your callback-module which has **Director**'s callback-functions defined and exported. In run-time, **Director** process calls those callback-functions in different states.  I'll explain those callback-functions in below.  
### `init/1`
For starting a linked **Director** process, you should call one of the API functions:
```erlang
director:start_link(Module::module(), InitArg::any()).
director:start_link(Module::module(), InitArg::any(), Opts::director:start_options()).
director:start_link(RegisterName::director:register_name(), Module::module(), InitArg::any()).
director:start_link(RegisterName::director:register_name(), Module::module(), InitArg::any(), Opts::director:start_options()).
```
For starting an stand-alone **Director** process, you should call one of the API functions:
```erlang
director:start(Module::module(), InitArg::any()).
director:start(Module::module(), InitArg::any(), Opts::director:start_options()).
director:start(RegisterName::director:register_name(), Module::module(), InitArg::any()).
director:start(RegisterName::director:register_name(), Module::module(), InitArg::any(), Opts::director:start_options()).
```
After calling, **Director** process calls `Module:init(InitArg)`, possible return values of `init/1` are:
```erlang
-type init_return() :: 'ok' % will be {ok, undefined, [], director:default_childspec(), []}
                     | {'ok', director:state()}
                     | {'ok', director:state(), [director:childspec()]|[]}
                     | {'ok', director:state(), [director:childspec()]|[], director:default_childspec()}
                     | {'ok', director:state(), [director:childspec()]|[], director:start_options()}
                     | {'ok', director:state(), [director:childspec()]|[], director:default_childspec(), director:start_options()}
                     | 'ignore'
                     | {'stop', Reason::any()}.
```
#### Childspec
A childspec is an Erlang map containing some mandatory and optional keys that belongs to one child process. I will explain these keys below.  
* **id:** This key should be unique for child and can be any erlang term. We will understand usage of this key later. This key is mandatory.  
* **start:** This key should be an `mfa()` (module, function and its arguments) and is mandatory. **Director** calls `erlang:apply(Mod, Func, Args)` using value of this key for starting child. Possible values of this key are:  
	* `mfa()`.  
	* `{module(), Func::atom()}`. Will be `{module(), Func::atom(), []}`.  
	* `module()`. Will be `{module(), start_link, []}`.  
* **state:** This key is optional and if not defined, its default value will be atom `undefined`. This is state data of child process inside **Director** process. We will understand usage of this key later.  
* **type:** A child process can be a worker or another supervisor. This key is optional and default value is atom `worker` and possible values are:  
	* `supervisor`.  
	* `sup` (Short for `supervisor`). 
	* `worker`.  
	* `w` (Short for `worker`).  
	* `s` (Short for `supervisor`).  
* **terminate_timeout:** When one **Director** process is terminating or when you call `director:terminate_child/2` or `director:terminate_and_delete_child/2` for a child,  It terminates its alive children or that child using `erlang:exit(ChildPid, Reason)`. Possible values are atom `infinity` and `0` and positive integers. When value is `0`, **Director** calls `erlang:exit(ChildPid, kill)` otherwise when this value is `X`, it calls `erlang:exit(ChildPid, shutdown)` and waits `X` milli-second for termination. If child did not terminate after `X`, It calls `erlang:exit(ChildPid, kill)`. This key is optional and if key `type` defined to `worker` its default value will be `5000` otherwise `infinity`. 
* **delete:** When **Director** process is in termination state, It deletes all children from its database (from list, ETS, Mnesia, etc). When you have some **Director** processes and they are using one ETS, Mnesia, etc table for keeping their children (Using shared table), If one **Director** stops, You can restart its children from another **Director** which has access to that table for every child that has this key `=> false` in its childspec. Default is `true`.   
* **modules:** This key is optional and its default value will be 1st element of its start `mfa`. Possible values are:  
	* `[module()]`.
	* `dynamic` (Where name of callback module will determine in future. for example in `gen_event` process).  
* **append:** What is usage of `director:default_childspec()` in above? If you want to have a number of children with similar childspec options, you can define a default childspec in return value of `init/1` and all children with `append => true` in their childspec will combine with that default childspec. Default childspec is like normal childspec except that this has not `id` and `append` keys. Default default childspec is:
	```erlang
	#{terminate_timeout => 0, modules => [], type => worker, delete => true, state => undefined}
	```  

#### Start options
You can define start options in two places, in calling `director:start/2-4` or `director:start_link/2-4` and in return value of `init/1`. Start options is an Erlang proplist with following items:
* **db:** Should be another erlang proplist with following items:
	* **table:** Default is `list`. Can be one of `list`, `ets` or `mnesia`. If you defined new table `x`,name your module `director_table_x` and define value of `table` to `x`.  
	* **init_arg:** If you are using `list` for keeping children, Its value not matters. for `mnesia` and `ets` must be table name. For new database callback module if this value not defined, atom `undefined` will be given to callback-function `director_table_x:create/1` and if defined, argument `{value, Value}` will be given.  
	* **delete:** Should be one of `true` or `false`. If defined to `true`. In termination of **Director** itself, It deletes the table. Default is `true`.  
* **debug:** Standard OTP/sys debug options:  
	```erlang
	-type dbg_opts() :: [dbg_opt()] | [].
	-type  dbg_opt() :: {'trace', 'true'}
                      | {'log',
                         {N :: non_neg_integer(),
                          [{Event :: system_event(),
                            FuncState :: _,
                            FormFunc :: format_fun()}]}}
                      | {'statistics', {file:date_time(),
                                        {'reductions', non_neg_integer()},
                                        MessagesIn :: non_neg_integer(),
                                        MessagesOut :: non_neg_integer()}}
                      | {'log_to_file', file:io_device()}
                      | {Func :: dbg_fun(), FuncState :: term()}.
	```  
	Default is nothing or `[]`. For more info see OTP `sys` module.  
* **spawn_opt:**: List of spawn options. For more info see OTP type `proc_lib:spawn_option()`. You cannot use this option in return value of `init/1`.  
* **timeout:** How much time process needs for initialization? You cannot use this option in return value of `init/1`.  

#### `init/1` examples
```erlang
-module(director_test).
...
-export([init/1]). % Do not forget to export it
...
-record(state, {}). %% State record for Director itself
-record(chstate, {}). %% State record for Director children

init(MyInitArg) ->
	Child_1 = #{id => child_1
	           ,start => {child_1_module, start_link, [MyInitArg]}
	           ,state => #chstate{}
	           ,terminate_timeout => 1000},
	Child_2 = #{id => child_2
	           ,start => {child_2_module, start_link, [MyInitArg, arg_2, arg_3]}
	           ,state => #chstate{}},
	{ok, #state{}, [Child_1, Child2], [{db, [{table, ets}, {init_arg, my_table}]}]}.
	%% In above if you want some children with similar childspecs, you can use default childspec:
%	Children = [#{id => Id, append => true} || _ <- lists:seq(1, 100)],
%	DefChildSpec = #{start => {foo, start, [arg_1, arg_2]}},
%	{ok, #state{}, Children, DefChildSpec}.
	%% You will have 100 childspecs with ids 1-100 and start {foo, start, [arg_1, arg_2]}
	
	%% If you want simple_one_for_one strategy of OTP/Supervisor:
%	Children = [#{id => Id, append => true, start => {foo, start, [arg_3]}} || _ <- lists:seq(1, 100)],
%	DefChildSpec = #{start => {foo, start, [arg_1, arg_2]}},
%	{ok, #state{}, Children, DefChildSpec}.
	%% You will have 100 childspecs with ids 1-100 and start {foo, start, [arg_1, arg_2, arg_3]}
...
```

### `handle_start/4`
When **Director** starts a child process, It calls:  
```erlang
YourCallbackModule:handle_start(ChildId, ChildState, DirectorState, Metadata)
```
In above example when **Director** starts `Child_1`, It calls:  
```erlang
director_test:handle_start(child_1, #chstate{}, #state{}, #{restart_count := 0, pid := PidOfChild_1})
```
This callback-function should yield:
```erlang
-type handle_start_return() :: {'ok', NewChildState::director:child_state(), NewDirectorState::director:state(), director:callback_return_options()}
                             | {'stop', director:child_state(), Reason::any(), director:callback_return_options()}.
```
Example of `handle_start/4` which just tells **Director** to don't call `error_logger` about starting any child:
```erlang
handle_start(_, ChState, State, _) ->
	{ok, ChState, State, [{log, false}]}.
```


### `handle_exit/5`
When a child process crashes, Its **Director** will receive child's exit signal and calls:  
```erlang
YourCallbackModule:handle_exit(ChildId, ChildState, ReasonOfChildTermination, DirectorState, MetaData)
```
In above example when `Child_1` exits with reason `oops`, **Director** calls:  
```erlang
director_test:handle_exit(child_1, #chstate{}, oops, #state{}, #{restart_count := 1})
```
This callback-function should yield:
```erlang
-type handle_exit_return() :: {director:action(), director:child_state(), director:state(), director:callback_return_options()}.
-type  action() :: 'restart'
                 | {'restart', pos_integer()}
                 | 'delete'
                 | 'wait'
                 | 'stop'
                 | {'stop', Reason::any()}.
```
Example of `handle_exit/5` which tells **Director** to restart child after 1000 milli-seconds:
```erlang
handle_exit(_, ChState, _, State, _) ->
	{{restart, 1000}, ChState, State, []}.
```
If you define `delete` as action, Child will be removed from children table. If you define `wait` as action, **Director** does nothing and you have to call `director:restart_child(DirectorProc, ChildId)` for restarting child. If you define `stop`, **Director** will terminate itself with error reason of child crash.  
What if you define `restart` or `{restart, Int}` and child does not restart?  **Director** will restart child again, So calls `handle_exit/5` again which its metadata argument has `restart_count` key plus one. For example in following code **Director** will restart child id `foo` for 5 times, then restarts it after 1000 milli-seconds for 6th time, and finally terminates itself with reason `{max_restart, foo}` for 7th time:  
```erlang
handle_exit(foo, ChState, _Reason, State, #{restart_count := RC}) when RC < 6 ->
	{restart, ChState, State, []};
handle_exit(foo, ChState, _Reason, State, #{restart_count := 6}) ->
	{{restart, 1000}, ChState, State, []};
handle_exit(foo, ChState, _Reason, State, _) ->
	{{stop, {max_restart, foo}}, ChState, State, []}.
```

### `handle_terminate/5`
When you call `director:terminate_child/2-3` or `director:delete_and_terminate_child/2-3`, **Director** terminates child process and calls:  
```erlang
YourCallbackModule:handle_terminate(ChildId, ChildState, ReasonOfChildTermination::shutdown|kill|term(), DirectorState, MetaData)
```
Also it calls `handle_terminate/5` when it is in terminate state and is terminating its alive children.  
For above example when you call `director:terminate_child(DirectorProc, child_2)`, It calls:  
```erlang
director_test:handle_exit(child_2, #chstate{}, shutdown, #state{}, #{restart_count := 0})
```
This callback-function should yield:  
```erlang
-type handle_terminate_return() :: {'ok', director:child_state(), director:state(), director:callback_return_options()}.
```
For example following code tells **Director** don't call `error_logger` for termination of child id `bar` and call it for other children:  
```erlang
handle_terminate(bar, ChildState, _, DirectorState, _) ->
	{ok, ChildState, DirectorState, [{log, false}]};
handle_terminate(_, ChildState, _, DirectorState, _) ->
	{ok, ChildState, DirectorState, []}. % Default log value is true
```

### `terminate/2`
When director is terminating itself, after terminating its alive children, It calls:  
```erlang
YourCallbackModule:terminate(ReasonOfTermination, DirectorState)
```
For above example if **Director** is terminating with reason `normal`, it calls:  
```erlang
director_test:terminate(normal, #state{})
```
This callback-function should yield:  
```erlang
-type terminate_return() :: {'ok', callback_return_options()}
                          | {'new_error', NewReason::any(), callback_return_options()}
                          | any().
```
For example following code tells **Director** to change crash reason `oops` to `normal` and do not call `error_logger` about terminating yourself:
```erlang
terminate(oops, State) ->
	{new_error, normal, [{log, false}]}.
```
Anything other than {`ok`, Opts}` and `{new_error, _, Opts}` causes **Director** to call `error_logger` and exit with its crash reason.  

# Build
## Compile
```sh
~/director $ make
===> Verifying dependencies...
===> Compiling director
```

## Use as dependency
##### Rebar3
Put this in deps in rebar.config:
```erlang
{director, "18.4.19"}
```

##### Rebar
Put this in deps in rebar.config:
```erlang
{director, ".*", {git, "https://github.com/Pouriya-Jahanbakhsh/director.git", {tag, "18.4.19"}}}
```

##### Mix
Put this in deps in mix.exs:
```elixir
{:director, "~> 18.4.19"}
```

##### erlang.mk
```make
dep_director = hex 18.4.19
```

#### API documentation
```sh

 /projects/director $ make doc
===> Verifying dependencies...
===> Fetching edown ({pkg,"edown","0.8.1"})
===> Compiling edown
===> Compiling director
===> Running edoc for director

 /projects/director $ ls doc/ | grep .md
director.md
director_table_ets.md
director_table_mnesia.md
README.md
```

### Todo
* Add test for having something like OTP/Supervisor `simple_one_for_one` strategy.  
* Add complete examples.  
* Add documentation for writing new `director_table` behaviour based module.  


### License
**`BSD 3-Clause`**


### Author
**`pouriya.jahanbakhsh@gmail.com`**


### Hex version
[**`18.4.19`**](https://hex.pm/packages/director)
