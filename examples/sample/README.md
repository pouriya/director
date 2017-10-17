# sample
In this example we have an OTP application which uses Director as root supervisor. Our application's root supervisor has one child which is `gen_server` process.

```sh
 ~/director/examples/sample $ make shell
===> Verifying dependencies...
===> Compiling director
===> Verifying dependencies...
===> Compiling sample
```
```erlang
Erlang/OTP 19 [erts-8.3] [source-d5c06c6] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V8.3  (abort with ^G)
%% Starting application and its supervision tree
1> application:start(sample).
ok

2> Sup = sample_sup.
sample_sup

3> director:which_children(Sup).
[{1,<0.64.0>,worker,[sample_child]}]

%% All OTP/supervisor API works for our Director too
4> director:which_children(Sup) =:= supervisor:which_children(Sup).
true

5> director:count_children(Sup). %% =:= supervisor:count_children(Sup)
[{specs,1},{active,1},{supervisors,0},{workers,1}]

%% Getting id and pid of all running children
%% Our Director has one child with id 1
6> director:get_pids(Sup).
{ok,[{1,<0.64.0>}]}

%% Getting pid of specific running child
7> director:get_pid(Sup, 1).
{ok,<0.64.0>}


8> director:terminate_child(Sup, 1). %% =:= supervisor:terminate_child(Sup, 1)
ok

9> director:get_pids(Sup).
{ok,[]}

10> director:get_pid(Sup, 1).
{error,undefined}

11> director:restart_child(Sup, 1). %% =:= supevisor:restart_child(Sup, 1)
{ok,<0.75.0>}

%% Getting restart count of specific child
12> director:get_restart_count(Sup, 1).
{ok,1}

%% Killing child (Child registerd for 'sample_child')
%% In this example i did not define childspec plan, Then Director used its default plan for restarting
%% Our child.
%% By using default plan, Director restarts children if it crashed with any reason for first 3 times.
%% In fouth crash Director restarts it after 1000 milli-seconds and in fifth crash Director terminates 
%% other children and terminates itself with crash reason of child.
13> exit(whereis(sample_child), kill). 
true

%% Diretor restarts it
14> director:get_restart_count(Sup, 1).
{ok,2}

15> director:get_pid(Sup, 1).          
{ok,<0.78.0>}

16> exit(whereis(sample_child), kill). 
true

17> director:get_pid(Sup, 1).          
{ok,<0.82.0>}

18> exit(whereis(sample_child), kill). 
true

%% Director will restart it after 1000 milli-seconds
19> director:get_pid(Sup, 1).          
{error,restarting}

20> director:get_pid(Sup, 1).
{ok,<0.87.0>}

%% Our root supervisor will terminate itself with crash reason of child
21> exit(whereis(sample_child), kill). 
true

=ERROR REPORT==== 17-Oct-2017::16:27:24 ===
** Director sample_sup terminating 
** Reason for termination == killed

=INFO REPORT==== 17-Oct-2017::16:27:24 ===
    application: sample
    exited: killed
    type: temporary
```
