### What is a `behavior`?
> In Erlang, a behavior is a design pattern implemented in a module/library. It provides functionality in a fashion similar to inheritance in object-oriented programming. A number of callback functions must be defined for each behavior to work in a module called callback-module.
> When you want to start a **Director** process, you should specify your module which has some callback functions for **Director** library that your process will call them.  

**Director** needs a callback module (like OTP/supervisor). In callback module you should define functions `init/1` and `terminate/2`.  
You can start linked or stand-alone **Director** process using:
```erlang
%% Linked
director:start_link(CallbackModule, InitArgument).
director:start_link(RegisterName, CallbackModule, InitArgument).
director:start_link(CallbackModule, InitArgument, StartOpts).
director:start_link(RegisterName, CallbackModule, InitArgument, StartOpts).

%% Stand-alone
director:start(CallbackModule, InitArgument).
director:start(RegisterName, CallbackModule, InitArgument).
director:start(CallbackModule, InitArgument, StartOpts).
director:start(RegisterName, CallbackModule, InitArgument, StartOpts).
```

**CallbackModule**: It's your callback module.  
**InitArgument**: It can be anything, Your **Director** process will pass this to `CallbackModule:init/1`.  
**RegisterName**: Name for registering director.  
```erlang
-type register_name() :: {'local', atom()}
                       | {'global', atom()}
                       | {'via', module(), term()}.
```
**StartOpts**:
```erlang
-type start_options() :: [start_option()] | [].
-type  start_option() :: {'debug', [sys:dbg_opt()]|[]}
                       | {'spawn_opt', proc_lib:spawn_option()}
                       | {'timeout', timeout()}
                       | {'log_validator', log_validator()}
                       | {'table_module', module()}
                       | {'table_init_argument', any()}
                       | {'delete_table', boolean()}.
-type  log_validator() :: fun((Name::any(), Type:: log_level(), Extra::term(), state()) ->
                              log_mode()).
-type   log_level() :: 'info' | 'error' | 'warning'.
-type   log_mode() :: 'short' | 'long' | 'none'.
```

Default start options:
```erlang
[{'debug', []}
,{'log_validator', fun director:log_validator/4}
,{'table_module', 'director_table_list'}
,{'table_init_argument', 'undefined'}
,{'delete_table', 'true'}]
```
Default value of options `'spawn_opt'` and `'timeout'` depends on OTP `gen` module.  
After describing `init/1` callback i will describe what log_validator is and what it does.  

### Callback function `init/1`
By calling `director:start_link/2-3-4` or `director:start/2-3-4`, new **Director** process starts and it calls `CallbackModule:init(InitArgument)` and waits for function return.  
`init/1` should return:
```erlang
-type init_return() :: {'ok', state(), [childspec()]|[]}
                     | {'ok', state(), [childspec()]|[], default_childspec()}
                     | {'ok', state(), [childspec()]|[], start_options()}
                     | {'ok', state(), [childspec()]|[], default_childspec(), start_options()}
                     | 'ignore'
                     | {'stop', Reason::any()}.
-type childspec() :: #{'id' => id()
                      ,'start' => start()
                      ,'plan' => plan()
                      ,'terminate_timeout' => terminate_timeout()
                      ,'type' => type()
                      ,'modules' => modules()
                      ,'append' => append()
                      ,'log_validator' => log_validator()
                      ,'delete' => delete()
                      ,'state' => state()}.
-type  id() :: term().
-type  start() :: module() % will be {module(), start_link, []}
                | {module(), function()} % will be {module(), function(), []}
                | mfa().
-type  plan() :: fun((Id::term(), Reason::term(), RestartCount::pos_integer(), state()) ->
                     {'restart', state()}                           |
                     {{'restart', Interval::pos_integer()}, state()}|
                     {'wait', state()}                              |
                     {'delete', state()}                            |
                     {'stop', state()}                              |
                     {{'stop', Reason::term()}, state()})           .
-type  terminate_timeout() :: 'infinity' | non_neg_integer().
-type  type() :: 'worker' | 'supervisor'.
-type  modules() :: [module()] | 'dynamic'.
-type  append() :: boolean().
-type  log_validator() :: fun((Name::any(), Type:: log_level(), Extra::term(), state()) ->
                              log_mode()).
-type   log_level() :: 'info' | 'error' | 'warning'.
-type   log_mode() :: 'short' | 'long' | 'none'.
-type  delete() :: boolean().
-type  state() :: any().

-type default_childspec() :: #{'start' => start()
                              ,'plan' => plan()
                              ,'terminate_timeout' => terminate_timeout()
                              ,'type' => type()
                              ,'modules' => modules()
                              ,'log_validator' => log_validator()
                              ,'delete' => delete()
                              ,'state' => state()}.
```

### What is childspec?
Every process which you want **director** supervise it, has its own options called childspec and its supervisor maybe restart it, delete it, etc if it crashed based these options.

#### Describing possible key-values of a childspec
**id**: This can be any Erlang term and should be unique for this child. You can use this for getting some information about this child from supervisor or tell supervisor to terminate or restart this child, etc.  

**start**: This tells **Director** use which module, function and arguments to start this child. This can be `{module(), Function::atom(), Argumenst::list()}`, `{module(), Function::atom()}` (`Arguments` sets to `[]`) and `module()` (`Function` sets to `start_link` and `Arguments` sets to `[]`). This has not default value.  

**plan**: How this child should be managed and supervised? This should be a fun with arity 4. When this child crashed, **Director** process will execute this fun with id of child as first argument, reason of crash as second argument, restart count (how much time this child was crashed and restarted) as third argument and child's `state` value as fourth argument.  
Return value of this fun should be:  
__`{'restart', NewState::any()}`__: **Director** will restart this child. If child doesn't start, Runs this fun again with restart count plus one and returned state.  
__`{{'restart', Interval::pos_integer()}, NewState::any()}`__: **Director** will restart this child after spending `Interval` milli-seconds. If child doesn't start, Runs this fun again with restart count plus one and returned state.  
__`{'wait', NewState::any()}`__: **Director** doesn't restart child. and you need to call `director:restart_child(DirectorProcess, IdOfThisChild)` to restart it.  
__`{'delete', NewState::any()}`__: **Director** deletes child from its children.  
__`{'stop', NewState::any()}`__: **Director** will terminate other children and terminates itself with crash reason of this child.  
__`{{'stop', Reason::any()}, NewState::any()}`__: **Director** will terminate other children and terminates itself with crash reason `Reason`.  
Default plan is `director:plan/4`.  

**type**: If child is another supervisor, set this to `'supervisor'` otherwise `'worker'`. Default is `'worker'`.  

**terminate_timeout**: How much time child needs for termination? Default value for child with type `'supervisor'` is `'infinity'` and for child with type `'worker'` is `5000` milli-seconds. Note thath `0` means killing child.  

**modules**: callback moules of child. If child can not tell callback modules yet, set this to `'dynamic'` (like a `'gen_event'` behavior).  Default is module part of `'start'` key in a list.  

**log_validator**:  When a child started or crashed, **Director** can generate valid log to `'error_logger'` process.  **Director** itself has a log validator and every child has a log validator too. Both do the same thing. log validator should be a fun with arity four. When a child is started **Director** runs this fun with id of child as first argument, `'info'` atom as second argument, `'start'` atom as third argument and state value as fourth argument. When child is crashed **Director** runs this fun with `'error'` as second argument and reason of error as third argument.  When director itself receives unexpected message runs its log validator with `'warning'` as second argument and `{'receive_unexpected_message', Msg::any()}` as third argument and when director itself receives unexpected call request runs this with `'warning'` as second argument and  `{'receive_unexpected_call', From::any(), Request::any()}` as third argument and when director is terminating calls this with `'error'` as second argument and reason of error as third argument. This fun should return one of `'short'`, `'long'` and `'none'`. `'short'` means call `'error_logger'` process with short descriptions, `'long'` means call `'error_logger'` process with full descriptions and `'none'` means don't call `'error_logger'`. (it's useful when you want to generate your own log inside this fun)  

**delete**: A number of **Director**s can use one shared backend table for keeping their children. When a **Director** is terminating, it terminates all linked children (children which it starts them), if those children have `delete=>false` in their childspecs, then director does not delete them from table before its termination and you can restart them in other **Director** which has access to shared table.  

**append**: When set this to `'true'` possible values of childspec will be added to values of `DefaultChildSpec`.  
Example:  
```erlang
%% I have this default childspec:
#{start => {foo, bar, [1, 2]}
 ,terminate_timeout => 1000
 ,modules => [baz]
 ,log_validator => LogValidator}

%% If i start a child using childspec:
#{id => 1
 ,start => {foo, bar, [3]}
 ,append => true}

%% Above childspec will convert to:
#{id => 1
 ,start => {foo, bar, [1, 2, 3]}
 ,terminate_timeout => 6000 % because default was 5000 which added to 1000
 ,modules => [baz, foo]
 ,log_validator => LogValidator
 ,append => true}
```
If you don't specify `'start'` in above childspec, `'start'` will be `{'foo', 'bar', [1, 2]}`.

### Callback function `terminate/2`
When **director** is terminating, calls `CallbackModule:terminate(Reason, State)`. `terminate/2` can return `{'error', NewReason::any()}` or other values.
