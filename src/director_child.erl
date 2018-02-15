-module(director_child).
-export([proplist/1
        ,childspec/1]).
-include("internal/director_child.hrl").


proplist(#?CHILD{pid = Pid
                ,id = Id
                ,restart_count = ResCount
                ,start = Start
                ,timer= Timer
                ,terminate_timeout = TerminateTimeout
                ,extra = Extra
                ,modules = Mods
                ,type = Type
                ,append = Append
                ,supervisor = Sup
                ,state = State
                ,delete = DelBeforeTerminate}) ->
    [{id, Id}
    ,{pid, Pid}
    ,{restart_count, ResCount}
    ,{mfargs, Start}
    ,{timer, Timer}
    ,{restart_type, temporary}
    ,{shutdown, case TerminateTimeout of
                    0 ->
                        brutal_kill;
                    Timeout ->
                        Timeout
                end}
    ,{child_type, Type}
    ,{extra, Extra}
    ,{modules, Mods}
    ,{append, Append}
    ,{supervisor, Sup}
    ,{state, State}
    ,{delete, DelBeforeTerminate}].


childspec(#?CHILD{id = Id
                 ,start = Start
                 ,terminate_timeout = TerminateTimeout
                 ,modules = Modules
                 ,type = Type
                 ,append = Append
                 ,state = State
                 ,delete = DelBeforeTerminate}) ->
    #{id => Id
    ,start => Start
    ,terminate_timeout => TerminateTimeout
    ,modules => Modules
    ,type => Type
    ,append => Append
    ,state => State
    ,delete => DelBeforeTerminate}.