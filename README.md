# Welcome
**Director** is a production-ready **supervisor** and **manager** for Erlang/Elixir processes with focus on speed, performance and flexibility.

## Synopsis
According to the Erlang's manual:  
>A supervisor is a process that supervises other processes called child processes. A child process can either be another supervisor or a worker process. Supervisors are used to build a hierarchical process structure called a supervision tree, a nice way to structure a fault-tolerant application.  
>In Erlang we tell supervisors to start other processes. Every process has its own options called childspec and its supervisor maybe restart it if it crashed based these options, etc.


## Features:  
* If a child process crashed, **Director** can do followings (Depends on childspec):
    * Restart child.
    * Restart child after time interval.
    * Delete child from its children.
    * Wait for calling `restart_child/2-3` for restarting.
    * Crash entire supervision tree.  
* **Director** has three database modules for keeping children:
	* `director_table_list`: An Erlang list. You have to get all children information directly from **Director** process.  
	* `director_table_ets`: Getting children info is very fast because you can get info directly from table instead of **Director** process and a number of **Director** processes can share a table for keeping their children which increases speed of starting and stoping them.  
	* `director_table_mnesia`: Many **Director** processes in one node or cluster of nodes can share a table for keeping their children. You can get children info directly from table in any node that has a copy of table. With true configuration if one node goes down, your children on that node will restart on other node.  
* In **Director**, your callback module which should give childspecs from its `init/1` function, should return an `State` value too. In termination **Director** calls `YourCallback:terminate(Reason, State)`.  
* A **Director** process can give response for all API functions of OTP/Supervisor module (for example `director:which_children(Pid) =:= supervisor:which_children(Pid)`). It has its own interesting API too.  
* **Director** does not write on top of generic behaviors like `gen_server`, etc. It was written as Erlang special process and it is blazingly fast.  
* **Director** has its own clean debug output for any working state (Standard OTP/sys debug).  
* In **Director** every child can has its own filter for validating its log for starting and crashing.  
* **Director** provides robust and flexible approach for starting and managing many children with one childspec. (Like `simple_one_for_one` strategy of OTP/Supervisor but better)  
* Backend table modules `director_table_ets` and `director_table_mnesia` have some useful API for getting children information directly from table.  
* Since **Director** is modular, You can write your own backend table for keeping children and give it to **Director** for using by implementing `director_table` behavior. Some test cases are ready for testing your table in `/test` directory too.  

All features not listed here.  For more info see wiki or examples directory.

### License
**`BSD 3-Clause`**


### Author
**`pouriya.jahanbakhsh@gmail.com`**


### Hex version
[**`17.11.30`**](https://hex.pm/packages/director)
