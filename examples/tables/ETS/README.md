# Sample of using ETS table as backend database for keeping children
Advantages:
* Read child(ren) information directly from ETS table is more faster than getting from **Director** process (Useful for worker pools).  
* You can share an ETS table to some **Director**s and you can start children in all of them concurrently instead of starting children sequential in one **Director**.  

# Test
```sh
~/director/examples/tables/sample_ets $ make shell
===> Verifying dependencies...
===> Compiling director
===> Verifying dependencies...
===> Compiling sample_ets
```
```erlang
Erlang/OTP 19 [erts-8.3] [source-d5c06c6] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]
Eshell V8.3  (abort with ^G)

%% Starting application
1> sample_ets:start().
ok

%% Starting new worker with id 'foo' and callback module 'sample_ets_worker_1'
%% For more info see source code of 'sample_ets_worker_1'
2> sample_ets:start_worker(foo, sample_ets_worker_1).
{ok,<0.65.0>}
 
%=INFO REPORT==== 24-Oct-2017::15:07:01 ===
%Worker foo started

%=INFO REPORT==== 24-Oct-2017::15:07:01 ===
%I'm done.

%=INFO REPORT==== 24-Oct-2017::15:07:01 ===
%Worker foo stopped after doing job

%% Getting pids of all RUNNING workers
3> sample_ets:get_pids().
[]

%% Starting new worker with id 'foo' and callback module 'sample_ets_worker_2'
%% For more info see source code of 'sample_ets_worker_2'
4> sample_ets:start_worker(foo, sample_ets_worker_2).
{ok,<0.68.0>}

%=INFO REPORT==== 24-Oct-2017::15:07:51 ===
%Worker foo started

%=ERROR REPORT==== 24-Oct-2017::15:07:51 ===
%Worker foo crashed with reason  oops

%=INFO REPORT==== 24-Oct-2017::15:07:51 ===
%Worker foo started

%=ERROR REPORT==== 24-Oct-2017::15:07:51 ===
%Worker foo crashed with reason  oops

%=INFO REPORT==== 24-Oct-2017::15:07:51 ===
%Worker foo started

%=ERROR REPORT==== 24-Oct-2017::15:07:51 ===
%Worker foo crashed with reason  oops

%=INFO REPORT==== 24-Oct-2017::15:07:51 ===
%Worker foo started

%=INFO REPORT==== 24-Oct-2017::15:07:51 ===
%I'm done.

%=INFO REPORT==== 24-Oct-2017::15:07:51 ===
%Worker foo stopped after doing job


5> sample_ets:get_pids().                            
[]

%% Starting new worker with id 'foo' and callback module 'sample_ets_worker_3'
%% For more info see source code of 'sample_ets_worker_3'
6> sample_ets:start_worker(foo, sample_ets_worker_3).
{ok,<0.78.0>}

%=INFO REPORT==== 24-Oct-2017::15:09:15 ===
%Worker foo started

7> sample_ets:get_pids().                            
[{foo,<0.78.0>}]

8> sample_ets:get_pid(). 
<0.78.0>

9> sample_ets:get_pid(foo).
<0.78.0>

%% Killing worker
10> exit(sample_ets:get_pid(foo), shutdown).
true

%=ERROR REPORT==== 24-Oct-2017::15:09:56 ===
%Worker foo crashed with reason  shutdown

%=INFO REPORT==== 24-Oct-2017::15:09:59 ===
%Worker foo started

11> sample_ets:get_pid(foo).                
<0.84.0>

%% Sending a fun to worker for executing
12> sample_ets:get_pid(foo) ! (fun() -> error_logger:info_msg("I'm working.~n"), exit(normal) end).
#Fun<erl_eval.20.118419387>

%=INFO REPORT==== 24-Oct-2017::15:18:42 ===
%Doing job.

%=INFO REPORT==== 24-Oct-2017::15:18:42 ===
%I'm working.

%=INFO REPORT==== 24-Oct-2017::15:18:42 ===
%Worker foo stopped after doing job

13> sample_ets:get_pid(foo).                                                                       
not_found


%% Starting 100 workers with ids 1-100 and callback module 'sample_ets_worker_3'
%% For more info see source code of 'sample_ets_worker_3'
14> lists:foreach(fun(Id) -> sample_ets:start_worker(Id, sample_ets_worker_3) end, lists:seq(1, 100)).
ok

%=INFO REPORT==== 24-Oct-2017::15:19:30 ===
%Worker 1 started

%=INFO REPORT==== 24-Oct-2017::15:19:30 ===
%Worker 2 started

%=INFO REPORT==== 24-Oct-2017::15:19:30 ===
%Worker 3 started

% ...

%=INFO REPORT==== 24-Oct-2017::15:19:30 ===
%Worker 99 started

%=INFO REPORT==== 24-Oct-2017::15:19:30 ===
%Worker 100 started

%% Getting random pid of an RUNNING worker
15> sample_ets:get_pid().                                                                             
<0.88.0>

16> sample_ets:get_pid().
<0.105.0>

17> sample_ets:get_pid().
<0.164.0>

18> sample_ets:get_pid().
<0.105.0>

19> sample_ets:get_pid().
<0.122.0>

20> sample_ets:get_pid().
<0.138.0>
 
```

## How to share this backend table to other **Director**(s)?
Just tell new director name of table and table backend module. Here i used `sample_ets_sup` module for starting two **Director**s and in this module they will get table.
```erlang
21> {ok, S1} = director:start_link(sample_ets_sup, []).
{ok,<0.196.0>}

22> {ok, S2} = director:start_link(sample_ets_sup, []).
{ok,<0.198.0>}

23> director:count_children(S1).
[{specs,100},{active,100},{supervisors,0},{workers,100}]

24> director:get_pid(S1, 100).  
{ok,<0.187.0>}

25> sample_ets:get_pid(101).
not_found

26> director:start_child(S2, #{id => 101, append => true, start => {sample_ets_worker_3, start_link, []}}).  
{ok,<0.206.0>}

%=INFO REPORT==== 24-Oct-2017::15:40:05 ===
%Worker 101 started

27> sample_ets:get_pid(101).                                                                                 
<0.206.0>

28> director:get_pid(S1, 101).                                                                               
{ok,<0.206.0>}
```
