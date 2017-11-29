# sample
In this example we have an OTP application which uses Director as root supervisor. This supervisor has two other supervisors as its children. I will start a child in one of them and transferit to other sup.

```sh
 ~/director/examples/supervision-flexibility $ make shell 
===> Verifying dependencies...
===> Compiling director
===> Verifying dependencies...
===> Compiling sample_sf
Erlang/OTP 19 [erts-8.3] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe] [kernel-poll:false]
Eshell V8.3  (abort with ^G)

1> application:start(sample_sf).
ok

2> {ok, Pid} = director:start_child(sample_sf_sup_1, #{id => 1, start => {sample_sf_worker, start_link, [1]}}).
{ok,<0.67.0>}

%% See source code
3> Pid ! print_parent.
<0.67.0>: parent == <0.64.0>
print_parent

4> director:get_pids(sample_sf_sup_1).
{ok,[{1,<0.67.0>}]}

5> director:get_pids(sample_sf_sup_2).
{ok,[]}

6> Pid ! {change_parent, self(), whereis(sample_sf_sup_2)}.
{change_parent,<0.58.0>,<0.65.0>}

7> flush().
Shell got ok
ok

8> Pid ! print_parent.                                     
<0.67.0>: parent == <0.65.0>
print_parent

9> director:get_pids(sample_sf_sup_1).                     
{ok,[]}

10> director:get_pids(sample_sf_sup_2).                     
{ok,[{1,<0.67.0>}]}
```
