![directror travis test status](https://travis-ci.org/Pouriya-Jahanbakhsh/director.png?branch=master)


# Welcome
**Director** is fast, powerful and flexible Erlang/Elixir process supervisor.  

## Synopsis
According to the Erlang's manual documentation:  
>A supervisor is a process that supervises other processes called child processes.  
>A child process can either be another supervisor or a worker process.  
>Supervisors are used to build a hierarchical process structure called a supervision tree, a nice way to structure a fault-tolerant application.  
>In Erlang/Elixir we tell supervisors to start other processes and restart them if they crash, etc.


## Features:  
* Restart child if child crashed with any reason.  
* Restart child if child crashed with some specific reason(s).
* Ignore restarting child if child crashed with any reason.  
* Ignore restarting child if child crashed with some specific reason(s).  
* Delete child from children if child crashed with any reason.  
* Delete child from children if child crashed with some specific reason(s).
* Restart child after spending time if child crashed with any reason.
* Restart child after spending time if child crashed with some specific reason(s).  
* Get pid of child if child is running in separate and atomic request.  
* Get pids of all running children.
* Change plan of restarting child dynamically.  
* Set some values as defaults and start children with that default values. (for example `mfa` of child, its more like `simple_one_for_one` strategy of OTP/Supervisor)  
* Change default values for starting children dynamically.  
* A **Director** process can give response for all API functions of OTP/Supervisor module !
* **Director** is not a generic behavior like `gen_sever`, etc. It was written as Erlang special process and it's so fast ! (Don't worry about handling system messages, it handles)
* **Director** has its own clean debug output for any working state.  
* **Director** makes necessary reports to `error_logger` just like OTP/Supervisor.(So some useful libraries like `lager` can use its output and don't need to write new code for understanding **Director**'s reports)  
* **Director** has 3 modes for giving details to `error_logger`: `short`, `long` and `off`.

If you are familiar with OTP/supervisor, by comparing **Director** and OTP/Supervisor you can understand that it's more flexible and more useful.

For more info see [**Wiki**](https://github.com/Pouriya-Jahanbakhsh/director/wiki) page.

### License
`BSD 3-Clause`

### Links
[**Github**](https://github.com/Pouriya-Jahanbakhsh/director)  
This documentation is availible in [http://docs.codefather.org/director](http://docs.codefather.org/director) too.
