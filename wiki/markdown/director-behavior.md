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
-type  start_option() :: {'debug', debug_options()}
                       | {'spawn_opt', proc_lib:spawn_option()}
                       | {'timeout', timeout()} % It's for starting
                       | {'log_validator', log_validator()}
                       | {'table_module', module()}
                       | {'table_init_argument', any()}
                       | {'delete_table_before_terminate', boolean()}.
-type   debug_options() :: ['trace'|'log'|'statistics'|'debug'|{'log_to_file', file:filename()}] 
                         | [].
-type   log_validator() :: fun((Type:: log_type(), Extra::term()) -> log_mode()).
-type    log_type() :: 'info' | 'error' | 'warning'.
-type    log_mode() :: 'short' | 'long' | 'none'.
```
----------------------------------------------------------------------------------------------------
Default start options:
```erlang
[{'debug', []}
,{'log_validator', fun director:log_validator/2}
,{'table_module', 'director_table_list'}
,{'table_init_argument', 'undefined'}
,{'delete_table_before_terminate', 'true'}]
```
Default value of options `'spawn_opt'` and `'timeout'` depends on OTP `gen` module.  
After describing `init/1` possible return values i will describe what log_validator is and what it does.  

### Callback function `init/1`
By calling `director:start_link/2-3-4` or `director:start/2-3-4`, new **Director** process starts and it calls `CallbackModule:init(InitArgument)` and waits for function return.  
`init/1` should return:
```erlang
-type init_return() :: {'ok', state(), childspecs()}
                     | {'ok', state(), childspecs(), default_childspec()}
                     | {'ok', state(), childspecs(), start_options()}
                     | {'ok', state(), childspecs(), default_childspecs(), start_options()} 
                     | {'stop', term()}
                     | 'ignore'.
-type  childspecs() :: [] | [childspec()].
-type   childspec() :: #{'id' => id()
                        ,'start' => start()
                        ,'plan' => plan()
                        ,'terminate_timeout' => terminate_timeout()
                        ,'type' => type()
                        ,'modules' => modules()
                        ,'append' => append()
                        ,'log_validator' => log_validator()
                        ,'pass_if_started' => pass_if_started()}.
-type   id() :: term().
-type   start() :: module() | {module(), function()} | mfa().
-type   plan() :: fun((Id::term(), Reason::term(), RestartCount::pos_integer(), State::any()) ->
                      'restart'                  |
                      {'restart', pos_integer()} |
                      'wait'                     |
                      'stop'                     |
                      {'stop', term()}).
-type   terminate_timeout() :: 'infinity' | non_neg_integer().
-type   type() :: 'worker' | 'supervisor'.
-type   modules() :: [module()] | 'dynamic'.
-type   append() :: boolean().
-type   pass_if_started() :: boolean().

-type  default_childspec() :: #{'start' => start()
                               ,'plan' => plan()
                               ,'terminate_timeout' => terminate_timeout()
                               ,'type' => type()
                               ,'modules' => modules()
                               ,'log_validator' => log_validator()
                               ,'pass_if_started' => boolean()}.
```

### What is childspec?
Every process which you want **director** supervise it, has its own options called childspec and its supervisor maybe restart it, delete it, etc if it crashed based these options.

#### Describing possible key-values of a childspec
**id**: This can be any Erlang term and should be uniqe for this child. You can use this for getting some information about this child from supervisor or tell supervisor to terminate or restart this child, etc.  

**start**: This tells **Director** use which module, function and arguments to start this child. This can be `{module(), Function::atom(), Argumenst::list()}`, `{module(), Function::atom()}` (`Arguments` sets to `[]`) and `module()` (`Function` sets to `start_link` and `Arguments` sets to `[]`). This has not default value.  

**plan**: How this child should be managed and supervised?  This should be a fun with arity 4. When this child crashed, **Director** process will execute this fun with id of child as first argument, reason of crash as second argument, restart count (how much time this child was crashed and restarted) as third argument and your returned `State` value as fourth argument.  
Return value of this fun should be:  
__`{'restart', NewState::any()}`__: **Director** will restart this child. If child doesn't start, Runs this fun again with restart count plus one.  
__`{{'restart', Interval::pos_integer()}, NewState::any()}`__: **Director** will restart this child after spending `Interval` milli-seconds. If child doesn't start, Runs this fun again with restart count plus one.  
__`{'wait', NewState::any()}`__: **Director** doesn't restart child. and you need to call `director:restart_child(DirectorProcess, IdOfThisChild)` to restart it.  
__`{'delete', NewState::any()}`__: **Director** deletes child from its children.  
__`{'stop', NewState::any()}`__: **Director** will terminate other children and terminates itself with crash reason of this child.  
__`{{'stop', Reason::any()}, NewState::any()}`__: **Director** will terminate other children and terminates itself with crash reason `Reason`.  
Default plan is `director:plan/4`.  
One plan example:
```erlang
%% I want my child to restart 5 first times if crashed with any reason, restart other times after 
%% 1000 milli-seconds if crashed with reason 'normal', doesn't restart if crashed 100 times and i
%% want to terminate entire supervisor with crash reason of child if it crashed more than 5 times 
%% for abnormal reasons.
my_plan(_Id, _Reason, RestartCount, State) when RestartCount =< 5 ->
	{restart, State};
my_plan(_Id, normal, 100, State) ->
	{wait, State};
my_plan(_Id, normal, _RestartCount, State) ->
	{{restart, 1000}, State};
my_plan(_Id, _Reason, _RestartCount, State) ->
	{stop, State}.
```
Be careful when State affects to restarting child, You returned one State and you may change this in fun plan of all children of this **Director**. Then changing this in fun plan of child A may affect to restarting child B. It's useful when you want to restart after time interval when you need to generate new time interval based previous generated time intervals.  
In above example we did not use `id`. If you want to use one fun plan for some children with different behaviors, it's useful.  


**type**: If child is another supervisor, set this to `'supervisor'` otherwise `'worker'`. Default is `'worker'`.  

**terminate_timeout**: How much time child needs for termination? Default value for child with type `'supervisor'` is `'infinity'` and for child with type `'worker'` is `5000` milli-seconds.  

**modules**: callback moules of child. If child could not tell callbacks modules yet, set this to `'dynamic'` (like a `'gen_event'` behavior).  Default is module part of `'start'` key.  

**log_validator**:  When a child started or crashed, **Director** can generate valid log to `'error_logger'` process.  **Director** itself has a log validator and every child has a log validator too. Both do the same thing. log validator should be a fun with arity two. When a child is started **Director** runs this fun with `'info'` as first argument and `'start'` as second argument and when child is crashed **Director** runs this fun with `'error'` as first argument and reason of error as second argument.  When director itself receives unexpected message runs itself log validator with `'warning'` as first argument and `{'receive_unexpected_message', Msg::any()}` as second argument and when director itself receives unexpected call request runs this with `'warning'` as first argument and  `{'receive_unexpected_call', From::any(), Request::any()}` as second argument and when director is terminating calls this with `'error'` as first argument and reason of error as second argument. This fun should return one of `'short'`, `'long'` and `'none'`. `'short'` means call `'error_logger'` process with short description of log, `'long'` means call `'error_logger'` process with full description and `'none'` means don't call `'error_logger'`. (it's useful when you want to generate your own log inside this function)  

**pass_if_started**: **Director** can use shared backend table for keeping its children. When **Director** wants to start children for first time, if you specify a child id twice you will get error `{'error', {'duplicate_child_name', Id::any()}}`. But if **Director** uses a shared table and an id is exists, child doesn't start. So when you are using a shared table and you want to return some children to some **director**s, set this to `'true'`, then **Director** knows that this child for first time started by other **Director**. There is some example for understanding this. Default value is `'false'`.  

**append**: When set this to `'true'` possible values of childspec will be added to values of  
`DefaultChildSpec`.  
Example:  
```erlang
%% I have this default childspec:
#{start => {undefined, undefined, [Arg1, Arg2]}
 ,terminate_timeout => 100
 ,modules => [Mod1]
 ,log_validator => LogValidator}

%% If i start a child using childspec:
#{id => 1
 ,start => {foo, bar, [Arg0]}
 ,append => true}

%% Above childspec will convert to:
#{id => 1
 ,start => {foo, bar, [Arg0, Arg1, Arg2]}
 ,terminate_timeout => 6000 % because default was 5000 which added to 1000
 ,modules => [foo, Mod1]
 ,log_validator => LogValidator
 ,append => true}
```
If you don't specify `'start'` in above childspec, `'start'` will be `{'undefined', 'undefined', [Arg1, Arg2]}`.

### Callback function `terminate/2`
When **director** is terminating, calls `CallbackModule:terminate(Reason, State)`. `terminate/2` can return `{'error', NewReason::any()}` or other values.
