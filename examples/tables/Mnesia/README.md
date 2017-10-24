## Sample of using Mnesia as backend database
In this example i will start two Erlang nodes `foo` and `bar`. every node has `sample_mnesia` application running. In node `foo` we have a worker named `foo_service` and in node `bar` we have not any worker. I will get worker `foo_service` info from node `bar` simply and finally i will stop node `foo` and worker `foo_service` will be restarted in node `bar`.  
Start application and worker in node `foo`:
```sh
~/director/examples/tables/Mnesia $ make foo
===> Verifying dependencies...
===> Compiling director
===> Verifying dependencies...
===> Compiling sample_mnesia
```
```erlang
Erlang/OTP 19 [erts-8.3] [source-d5c06c6] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]
Eshell V8.3  (abort with ^G)

%% Starting application
(foo@localhost)1> sample_mnesia:start().
ok

%% Start one worker which prints "Hello world!\n" every 3 seconds
(foo@localhost)2> sample_mnesia:start_service(foo_service, fun() -> io:format("Hello world!~n") end, 3000, infinity).
{ok,<0.89.0>}
(foo@localhost)3> Hello world!
Hello world!
Hello world!
Hello world!
Hello world!
Hello world!
Hello world!
Hello world!
```
Leave this shell alone and start node `bar`:
```sh
~/director/examples/tables/Mnesia $ make bar
===> Verifying dependencies...
===> Compiling director
===> Verifying dependencies...
===> Compiling sample_mnesia
```
```erlang
Erlang/OTP 19 [erts-8.3] [source-d5c06c6] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]
Eshell V8.3  (abort with ^G)

%% Connect nodes and get copy of table
(bar@localhost)1> F = 
(bar@localhost)1>     fun() ->
(bar@localhost)1>         Node = 'foo@localhost',
(bar@localhost)1>         pong = net_adm:ping(Node),
(bar@localhost)1>         application:start(mnesia),
(bar@localhost)1>         {ok, _} = mnesia:change_config(extra_db_nodes, [Node]),
(bar@localhost)1>         mnesia:add_table_copy(service, node(), ram_copies),
(bar@localhost)1>         ok
(bar@localhost)1>     end,F().
ok

%% Start application
(bar@localhost)2> sample_mnesia:start().
ok

%% Get all RUNNING services in cluster
(bar@localhost)3> sample_mnesia:services().
{ok,[{foo_service,<7220.89.0>}]}

%% Get pid of service 'foo_service'
(bar@localhost)4> sample_mnesia:service_pid(foo_service).
<7220.89.0>

%% Get node of service 'foo_service'
(bar@localhost)5> node(sample_mnesia:service_pid(foo_service)).
foo@localhost
```
Stop node `foo` using `Ctrl-C`
```erlang
Hello world!
Hello world!
Hello world!

BREAK: (a)bort (c)ontinue (p)roc info (i)nfo (l)oaded
       (v)ersion (k)ill (D)b-tables (d)istribution
a
```
Now see node `bar`:
```erlang
(bar@localhost)6> Hello world!
Hello world!

%% Get node of service 'foo_service' again 
(bar@localhost)6> node(sample_mnesia:service_pid(foo)).
bar@localhost

(bar@localhost)7> Hello world!
Hello world!
Hello world!
Hello world!
Hello world!
Hello world!
Hello world!
```
