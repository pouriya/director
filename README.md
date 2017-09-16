![directror travis test status](https://travis-ci.org/Pouriya-Jahanbakhsh/director.png?branch=master) [![Hex version](https://img.shields.io/hexpm/v/director.svg "Hex version")](https://hex.pm/packages/director)


# Welcome
**Director** is a production-ready **supervisor** and **manager** for Erlang/Elixir processes with focus on speed and flexibility.

## Synopsis
According to the Erlang's manual documentation:  
>A supervisor is a process that supervises other processes called child processes.  
>A child process can either be another supervisor or a worker process.  
>Supervisors are used to build a hierarchical process structure called a supervision tree, a nice way to structure a fault-tolerant application.  
>In Erlang/Elixir we tell supervisors to start other processes and restart them if they crash, etc.


## Features:  
* If a child crashed, **Director** can do following works (Depends on your childspec plan):
    * Restart child.
    * Restart child after time interval.
    * Delete child from its children.
    * Wait for calling `restart_child/2-3` for restarting.
    * Crash itself.
* A **Director** process can give response for all API functions of OTP/Supervisor module (for example `director:which_children(Pid)` `=:=` `supervisor:which_children(Pid)`).
* **Director** does not write top of generic behaviors like `gen_sever`, etc. It was written as Erlang special process and it is blazingly fast.
* **Director** has its own clean debug output for any working state (Standard OTP/sys debug).
* In **Director** every child can has its own filter for validating its log for starting and crashing.
* **Director** makes robust and flexible approach for starting and managing many children with one childspec.
* **Director** gives pids of all running children in separate request called `get_pids/1-2`.

All features not listed here.

For more info see [**Wiki**](https://github.com/Pouriya-Jahanbakhsh/director/wiki) page.

### License
`BSD 3-Clause`

### Links
[**Github**](https://github.com/Pouriya-Jahanbakhsh/director)  
This documentation is availible in [http://docs.codefather.org/director](http://docs.codefather.org/director) too.
