![directror travis test status](https://travis-ci.org/Pouriya-Jahanbakhsh/director.png?branch=master)

# Director
Flexible, fast and powerful supervisor library for Erlang/Elixir processes.  

## synopsis
##### What is process supervisor?
According to the [**erlang.org**](http://erlang.org/doc/man/supervisor.html):  
A supervisor is a process that supervises other processes called child processes.  
A child process can either be another supervisor or a worker process.  
Supervisors are used to build a hierarchical process structure called a supervision tree, a nice way to structure a fault-tolerant application.  


##### Can we use Director instead of standard OTP/supervisor?
Yes, you can also call OTP/supervisor API for your `director` process !

##### What is advantage of using Director instead of OTP/supervisor?  
For example:  
* You can have 5 restarts with 1000 mili-seconds interval between them for specific child(ren).  
* You can tell `director` to delete specific child(ren) from supervision tree after 10th restart.  
* You can tell `director` to stop and exit supervisor with crash reason of specific child(ren) process.  
* You can tell `director` to restart specific child if it crashed with reason `oops` and restart it after 5000 mili-second if it crashed with reason `damn` and finally delete from supervisor if it crashed with reason `bye`.  

You can make it like OTP/supervisor `simple_one_for_one` in efficient and flexible approach.
...

## Download
```sh
~ $ git clone https://github.com/Pouriya-Jahanbakhsh/director.git
```

## Compile
Note that **OTP>=19** required (if you want to upgrade it using `release_handler`).  
Go to `director` and use `rebar` or `rebar3`.
```sh
~ $ cd director
```
rebar
```sh
~/director $ rebar compile
==> director_test (compile)
Compiled src/director.erl
~/director $
```
rebar3
```sh
~/director $ rebar3 compile
===> Verifying dependencies...
===> Compiling director
~/director $
```


## How it works
**director** needs a callback module (like OTP/supervisor).  
In callback module you should export function `init/1`.  
What `init/1` should return? wait, i'll explain step by step.
```erlang
-module(foo).
-export([init/1]).

init(_InitArg) ->
    {ok, []}.
```
Save above code in `foo.erl` in **director** directory and go to the Erlang shell.  
Use `erl -pa ./ebin` if you used `rebar` to compile it and use `rebar3 shell` if you used `rebar3`.  
```erlang
Erlang/OTP 19 [erts-8.3] [source-d5c06c6] [64-bit] [smp:8:8] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V8.3  (abort with ^G)
1> c(foo).
{ok,foo}

2> Mod = foo.
foo

3> InitArg = undefined. %% i don't need it yet.
undefined

4> {ok, Pid} = director:start_link(Mod, InitArg).
{ok,<0.112.0>}

5> 
```
Now we have a supervisor without children.  
Good news is that **director** comes with full OTP/supervisor API and it has its advanced features and specific approachs too.
```erlang
5> director:which_children(Pid). %% You can use supervisor:which_children(Pid) too :)
[]

6> director:count_children(Pid). %% You can use supervisor:count_children(Pid) too :)
[{specs,0},{active,0},{supervisors,0},{workers,0}]

7> director:get_pids(Pid). %% You can NOT use supervisor:get_pids(Pid) because it hasn't :D
[]
```
OK, I'll make simple `gen_server` and give it to our **director**.
```erlang
-module(bar).
-behaviour(gen_server).
-export([start_link/0
        ,init/1
        ,terminate/2]). %% i am not going to use handle_call, handle_cast ,etc.


start_link() ->
    gen_server:start_link(?MODULE, undefined, []).

init(_GenServerInitArg) ->
    {ok, state}.

terminate(_Reason, _State) ->
    ok.
```
Save above code in `bar.erl` and got back to the Shell.
```erlang
8> c(bar).                                       
bar.erl:2: Warning: undefined callback function code_change/3 (behaviour 'gen_server')
bar.erl:2: Warning: undefined callback function handle_call/3 (behaviour 'gen_server')
bar.erl:2: Warning: undefined callback function handle_cast/2 (behaviour 'gen_server')
bar.erl:2: Warning: undefined callback function handle_info/2 (behaviour 'gen_server')
{ok,bar}

%% You should define unique id for your process.
9> Id = bar_id.
bar_id

%% You should tell director about start module and function for your process.
%% Should be tuple {Module, Function, Args}.
%% If your start function doesn't need arguments (like our example)
%% just use {Module, function}.
10> start = {bar, start_link}.
{bar,start_link}

%% What is your plan for your process?
%% Plan should be an empty list or list with n elemenst.
%% Every element can be one of
%% 'restart'
%% 'delete'
%% 'wait'
%% 'stop'
%% {'stop', Reason::term()}
%% {'restart', Time::pos_integer()}
%% for example my plan is:
%% [restart, {restart, 5000}, delete]
%% In first crash director will restart my process, 
%% after next crash director will restart it after 5000 mili-seconds
%% and after third crash director will not restart it and will delete it
11> Plan = [restart, {restart, 5000}, delete].
[restart,{restart,5000},delete]

%% What if i want to restart my process 500 times?
%% Do i need a list with 500 'restart's?
%% No, you just need a list with one element, I'll explain it later.

12> Childspec = #{id => Id
                 ,start => Start
                 ,plan => Plan}.
#{id => bar_id,
  plan => [restart,{restart,5000},delete],
  start => {bar,start_link}}

13> director:start_child(Pid, Childspec). %% You can use supervisor:start_child(Pid, ChildSpec) too :)
{ok,<0.160.0>}

14> 
```
Lets check it
```erlang
14> director:which_children(Pid).
[{bar_id,<0.160.0>,worker,[bar]}]

15> director:count_children(Pid). 
[{specs,1},{active,1},{supervisors,0},{workers,1}]

%% What was get_pids/1?
%% It will returns all RUNNING ids with their pids.
16> director:get_pids(Pid).
[{bar_id,<0.160.0>}]

%% We can get Pid for specific RUNNING id too
17> {ok, BarPid1} = director:get_pid(Pid, bar_id). 
{ok,<0.160.0>}


%% I want to kill that process
18> erlang:exit(BarPid1, kill).
true

%% Check all running pids again
19> director:get_pids(Pid).                       
[{bar_id,<0.174.0>}] %% pid was changed (restarted)

%% I want to kill that process again
%% and i will check children before spending time
20> {ok, BarPid2} = director:get_pid(Pid, bar_id), erlang:exit(BarPid2, kill).
true

21> director:get_pids(Pid).
[]

22> director:which_children(Pid).
[{bar_id,restarting,worker,[bar]}] %% restarting

23> director:get_pid(Pid, bare_id).
{error,not_found}

%% after 5000 ms
24> director:get_pids(Pid).      
[{bar_id,<0.181.0>}]

25> %% Yoooohoooooo
```

I mentioned **advanced features**, what are they?
Lets see other acceptable keys for `Childspec` map.
```erlang
-type childspec() :: #{'id' => id()      
                      ,'start' => start()
                      ,'plan' => plan()
                      ,'count' => count()
                      ,'terminate_timeout' => terminate_timeout()
                      ,'type' => type()
                      ,'modules' => modules()
                      ,'append' => append()}.

%% 'id' is mandatory and can be any Erlang term
-type  id() :: term().

%% Sometimes 'start' is optional ! just wait and read carefully
-type  start() :: {module(), function()} % default Args is []
                | mfa().

%% I explained 'restart' and {'restart', MiliSeconds}
%%
%% 'stop': director will crash with crash reason of child process.
%%
%% {'stop', Reason}: director exactly will crash with reason Reason.
%%
%% 'wait': director will not restart process, 
%%  but you can restart it using director:restart_child/2 and you can use supervisor:restart_child/2 too.
%%
%% fun/2: director will execute fun with 2 arguments.
%%  First argument is crash reason for process and second argument is restart count for process.
%%  Fun should return one of previous terms.
%% Default plan is:
%% [fun
%%      (normal, _RestartCount) ->
%%          delete;
%%      (shutdown, _RestartCount) ->
%%          delete;
%%      ({shutdown, _Reason}, _RestartCount) ->
%%          delete;
%%      (_Reason, _RestartCount) ->
%%          restart
%%  end]
-type  plan() :: [plan_element()] | [].
-type   plan_element() :: 'restart'
                        | {'restart', pos_integer()}
                        | 'wait'
                        | 'stop'
                        | 'delete'
                        | {'stop', Reason::term()}
                        | fun((Reason::term()
                              ,RestartCount::pos_integer()) ->
                                  'restart'
                                | {'restart', pos_integer()}
                                | 'wait'
                                | 'stop'
                                | 'delete'
                                | {'stop', Reason::term()}).

%% How much time you want to run plan?
%% Default value is 1.
%% What if i want to restart my process 500 times?
%%  Do i need a list with 500 'restart's?
%%  You just need plan ['restart'] and 'count' 500 :)
-type  count() :: 'infinity' | non_neg_integer().

%% How much time director should wait for process termination?
%% 0 means brutal kill and director will kill your process using erlang:exit(YourProcess, kill).
%% For workers default value is 1000 mili-seconds and for supervisors default value is 'infinity'.
-type  terminate_timeout() :: 'infinity' | non_neg_integer().

%% default is 'worker'
-type  type() :: 'worker' | 'supervisor'.

%% Default is first element of 'start' (process start module)
-type  modules() :: [module()] | 'dynamic'.

%% :)
%% Default value is 'false'
%% I'll explan it later.
-type  append() :: boolean().
```


Edit `foo` module:
```erlang
-module(foo).
-export([start_link/0
        ,init/1]).

start_link() ->
    director:start_link({local, foo_sup}, ?MODULE, undefined).

init(_InitArg) ->
    Childspec = #{id => bar_id
                 ,plan => [wait]
                 ,start => {bar,start_link}
                 ,count => 1
                 ,terminate_timeout => 2000},
    {ok, [Childspec]}.
```

Go to the Erlang shell again:
```erlang
1> c(foo).
{ok,foo}

2> foo:start_link().
{ok,<0.121.0>}

3> director:get_childspec(foo_sup, bar_id). 
{ok,#{append => false,count => 1,id => bar_id,
      modules => [bar],
      plan => [wait],
      start => {bar,start_link,[]},
      terminate_timeout => 2000,type => worker}}

4> {ok, Pid} = director:get_pid(foo_sup, bar_id), erlang:exit(Pid, kill).
true

5> director:which_children(foo_sup).
[{bar_id,undefined,worker,[bar]}] %% undefined

6> director:count_children(foo_sup).
[{specs,1},{active,0},{supervisors,0},{workers,1}]

7> director:get_plan(foo_sup, bar_id).
{ok,[wait]}

%% I can change process plan
%% I killed process one time.
%% If i kill it again, entire supervisor will crash with reason {reached_max_restart_plan... because 'count' is 1
%% But after changing plan, its counter will restart from 0.
8> director:change_plan(foo_sup, bar_id, [restart]).
ok

9> director:get_childspec(foo_sup, bar_id).
{ok,#{append => false,count => 1,id => bar_id,
      modules => [bar],
      plan => [restart], %% here
      start => {bar,start_link,[]},
      terminate_timeout => 2000,type => worker}}

10> director:get_pids(foo_sup).
[]

11> director:restart_child(foo_sup, bar_id).
{ok,<0.111.0>}

12> {ok, Pid2} = director:get_pid(foo_sup, bar_id), erlang:exit(Pid2, kill).
true

13> director:get_pid(foo_sup, bar_id).
{ok,<0.113.0>}

14> %% Hold on
```

##### Finally what the `append` key is?
actually always we have one `DefaultChildspec`.
```erlang
14> director:get_default_childspec(foo_sup).
#{count => 0,modules => [],plan => [],terminate_timeout => 0}

15>
```
`DefaultChildspec` is like normal childspecs except that it can't accept `id` and `append` keys.  
If i change `append` value to `true` in my `Childspec`:  
My `terminate_timeout` will be added to `terminate_timeout` of `DefaultChildspec`.  
My `count` will be added to `count` of `DefaultChildspec`.  
My `modules` will be added to `modules` of `DefaultChildspec`.  
My `plan` will be added to `plan` of `DefaultChildspec`.  
And if i have `start` key with value `{ModX, FuncX, ArgsX}` in `DefaultChildspec` and `start` key with value `{ModY, FunY, ArgsY}` in `Childspec`, final value will be `{ModY, FuncY, ArgsX ++ ArgsY}`.  
And finally if i have `start` key with value `{Mod, Func, Args}` in `DefaultChildspec`, `start` key in `Childspec` is optional for me.  
You can return your own `DefaultChildspec` as third element of tuple in `init/1`.  
Edit `foo.erl`:
```erlang
-module(foo).
-behaviour(director). %% Yes, this is a behaviour
-export([start_link/0
        ,init/1]).

start_link() ->
    director:start_link({local, foo_sup}, ?MODULE, null).

init(_InitArg) ->
    Childspec = #{id => bar_id
                 ,plan => [wait]
                 ,start => {bar,start_link}
                 ,count => 1
                 ,terminate_timeout => 2000},
    DefaultChildspec = #{start => {bar, start_link}
                        ,terminate_timeout => 1000
                        ,plan => [restart]
                        ,count => 5},
    {ok, [Childspec], DefaultChildspec}.
```
Restart the shell:
```erlang
1> c(foo).
{ok,foo}

2> foo:start_link().
{ok,<0.111.0>}

3> director:get_pids(foo_sup).
[{bar_id,<0.112.0>}]

4> director:get_default_childspec(foo_sup).
#{count => 5,
 plan => [restart],
 start => {bar,start_link,[]},
 terminate_timeout => 1000}

5> Childspec1 = #{id => 1, append => true},
%% Default 'plan' is [Fun], so 'plan' will be [restart] ++ [Fun] or [restart, Fun].
%% Default 'count' is 1, so 'count' will be 1 + 5 or 6.
%% Args in above Childspec is [], so Args will be [] ++ [] or [].
%% Default 'terminate_timeout' is 1000, so 'terminate_timeout' will be 1000 + 1000 or 2000.
%% Default 'modules' is [bar], so 'modules' will be [bar] ++ [] or [bar].
5> director:start_child(foo_sup, Childspec1).
{ok,<0.116.0>}

%% Test
6> director:get_childspec(foo_sup, 1).       
{ok,#{append => true,
      count => 6,
      id => 1,
      modules => [bar],
      plan => [restart,#Fun<director.default_plan_element_fun.2>],
      start => {bar,start_link,[]},
      terminate_timeout => 2000,
      type => worker}}

7> director:get_pids(foo_sup).
[{bar_id,<0.112.0>},{1,<0.116.0>}]

%% I want to have 9 more children like that
8> [director:start_child(foo_sup
                        ,#{id => Count, append => true})
   || Count <- lists:seq(2, 10)].
[{ok,<0.126.0>},
 {ok,<0.127.0>},
 {ok,<0.128.0>},
 {ok,<0.129.0>},
 {ok,<0.130.0>},
 {ok,<0.131.0>},
 {ok,<0.132.0>},
 {ok,<0.133.0>},
 {ok,<0.134.0>}]

10> director:count_children(foo_sup).
[{specs,11},{active,11},{supervisors,0},{workers,11}]

11>
```
You can change `defaultChildspec` dynamically using `change_default_childspec/2` !  
And you can change `Childspec` of children dynamically too and set their `append` to `true` !  
But with changing them in different parts of code, you will make [**spaghetti code**](https://en.wikipedia.org/wiki/Spaghetti_code)


### Can i debug director?
Yessssss, **diorector** has its own debug and accepts standard `sys:dbg_opt/0`.  
**director** sends valid logs to `sasl` and `error_logger` in different states too.
```erlang
1> Name = {local, dname},
   Mod = foo,
   InitArg = undefined,
   DbgOpts = [trace],
   Opts = [{debug, DbgOpts}].
[{debug,[trace]}]

2> director:start_link(Name, Mod, InitArg, Opts).
{ok,<0.106.0>}
3> 
3> director:count_children(dname).
*DBG* director "dname" got request "count_children" from "<0.102.0>" 
*DBG* director "dname" sent "[{specs,1},
                              {active,1},
                              {supervisors,0},
                              {workers,1}]" to "<0.102.0>"
[{specs,1},{active,1},{supervisors,0},{workers,1}]

4> director:change_plan(dname, bar_id, [{restart, 5000}]).
*DBG* director "dname" got request "{change_plan,bar_id,[{restart,5000}]}" from "<0.102.0>" 
*DBG* director "dname" sent "ok" to "<0.102.0>"
ok

5> {ok, Pid} = director:get_pid(dname, bar_id).
*DBG* director "dname" got request "{get_pid,bar_id}" from "<0.102.0>" 
*DBG* director "dname" sent "{ok,<0.107.0>}" to "<0.102.0>"
{ok,<0.107.0>}

%% Start SASL
6> application:start(sasl).
ok
... %% Log about starting SASL

7> erlang:exit(Pid, kill).
*DBG* director "dname" got exit signal for pid "<0.107.0>" with reason "killed"
*DBG* director "dname" is running plan "{restart, 5000}" for id "bar_id"
true

=SUPERVISOR REPORT==== 4-May-2017::12:37:41 ===
     Supervisor: dname
     Context:    child_terminated
     Reason:     killed
     Offender:   [{id,bar_id},
                  {pid,<0.107.0>},
                  {plan,[{restart,5000}]},
                  {count,1},
                  {count2,0},
                  {restart_count,0},
                  {mfargs,{bar,start_link,[]}},
                  {plan_element_index,1},
                  {plan_length,1},
                  {timer_reference,undefined},
                  {terminate_timeout,2000},
                  {extra,undefined},
                  {modules,[bar]},
                  {type,worker},
                  {append,false}]
8>

%% After 5000 mili-seconds 
*DBG* director "dname" got restart event for child-id "bar_id" with timer reference "#Ref<0.0.1.176>"

=PROGRESS REPORT==== 4-May-2017::12:37:46 ===
          supervisor: dname
             started: [{id,bar_id},
                       {pid,<0.122.0>},
                       {plan,[{restart,5000}]},
                       {count,1},
                       {count2,1},
                       {restart_count,1},
                       {mfargs,{bar,start_link,[]}},
                       {plan_element_index,1},
                       {plan_length,1},
                       {timer_reference,#Ref<0.0.1.176>},
                       {terminate_timeout,2000},
                       {extra,undefined},
                       {modules,[bar]},
                       {type,worker},
                       {append,false}]
8>
```


## Warnings
* If you have plans like:  

```erlang
Childspec1 = #{id => foo
              ,start => {bar, baz}
              ,plan => [restart,restart,delete,wait,wait, {restart, 4000}]
              ,count => infinity}.

Childspec2 = #{id => foo
              ,start => {bar, baz}
              ,plan => [restart,restart,stop,wait, {restart, 20000}, restart]
              ,count => infinity}.

Childspec3 = #{id => foo
              ,start => {bar, baz}
              ,plan => [restart,restart,stop,wait, {restart, 20000}, restart]
              ,count => 0}.

Childspec4 = #{id => foo
              ,start => {bar, baz}
              ,plan => []
              ,count => infinity}.
```
The rest of `delete` element in `Childspec1` and the rest of `stop` element in `Childspec2` will never evaluate!  
In `Childspec3` you want to run your plan 0 times!  
In `ChildSpec4` you have not any plan to run `infinity` times!

* When you upgrade a release using `release_handler`, `release_handler` calls `supervisor:get_callback_module/1` for fetching its callback module.  
In OTP<19 `get_callback_module/1` uses supervisor internal state record for giving its callback module. Our **director** does not know about supervisor internal state record, then `supervisor:get_callback_module/1` does not work with **director**s.  
Good news is that in OTP>=19 `supervisor:get_callback_module/1` works perfectly with **director**s :).
```erlang
1> foo:start_link().
{ok,<0.105.0>}

2> supervisor:get_callback_module(foo_sup).
foo

3>
```



### License
`BSD 3-Clause`

### Links
[**Github**](https://github.com/Pouriya-Jahanbakhsh/director)  
This documentation is availible in [http://docs.codefather.org/director](http://docs.codefather.org/director) too.
