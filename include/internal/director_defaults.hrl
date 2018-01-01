%%% ------------------------------------------------------------------------------------------------
%%% Director is available for use under the following license, commonly known as the 3-clause (or
%%% "modified") BSD license:
%%%
%%% Copyright (c) 2018-2019, Pouriya Jahanbakhsh
%%% (pouriya.jahanbakhsh@gmail.com)
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without modification, are permitted
%%% provided that the following conditions are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright notice, this list of
%%%    conditions and the following disclaimer.
%%%
%%% 2. Redistributions in binary form must reproduce the above copyright notice, this list of
%%%    conditions and the following disclaimer in the documentation and/or other materials provided
%%%    with the distribution.
%%%
%%% 3. Neither the name of the copyright holder nor the names of its contributors may be used to
%%%    endorse or promote products derived from this software without specific prior written
%%%    permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
%%% IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
%%% FITNESS FOR A  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.
%%% ------------------------------------------------------------------------------------------------

-define(DEF_START_OPTIONS, []).
-define(DEF_STOP_TIMEOUT, 'infinity').
-define(DEF_CALL_TIMEOUT, 5000).

-define(DEF_DEBUG_OPTIONS, []).
-define(GEN_CALL_TAG, '$gen_call').
-define(COUNT_CHILDREN_TAG, 'count_children').
-define(DELETE_CHILD_TAG, 'delete_child').
-define(GET_CHILDSPEC_TAG, 'get_childspec').
-define(RESTART_CHILD_TAG, 'restart_child').
-define(START_CHILD_TAG, 'start_child').
-define(TERMINATE_CHILD_TAG, 'terminate_child').
-define(WHICH_CHILDREN_TAG, 'which_children').
-define(GET_PID_TAG, 'get_pid').
-define(GET_PIDS_TAG, 'get_pids').
-define(CHANGE_PLAN_TAG, 'change_plan').
-define(CHANGE_COUNT_TAG, 'change_count').
-define(GET_PLAN_TAG, 'get_plan').
-define(GET_RESTART_COUNT_TAG, 'get_restart_count').
-define(GET_DEF_CHILDSPEC, 'get_default_childspec').
-define(CHANGE_DEF_CHILDSPEC, 'change_default_childspec').
-define(CHANGE_LOG_VALIDATOR, 'change_log_validator').
-define(TERMINATE_AND_DELETE_CHILD_TAG, 'terminate_and_delete_child').
%%-define(GET_LOG_VALIDATOR, 'get_log_validator').
-define(DEF_LOG_VALIDATOR, fun director:log_validator/4).
-define(DEF_LOG_MODE, 'short').
-define(DEF_TABLE_MOD, 'director_table_list').
-define(DEF_TABLE_INIT_ARG, 'undefined').
-define(DEF_DELETE_TABLE, 'true').
-define(DEF_CHILDSPEC_STATE, 'undefined').
-define(BECOME_SUPERVISOR_TAG, 'become_supervisor').
-define(CHANGE_PARENT_TAG, 'change_parent').
-define(DELETE_RUNNING_CHILD_TAG, 'delete_running_child').
-define(DEF_DELETE_BEFORE_TERMINATE, 'true').

-define(DEF_PLAN, fun director:plan/4).
-define(DEF_TYPE, worker).
-define(DEF_APPEND, false).
-define(DEF_WORKER_TERMINATE_TIMEOUT, 1000).
-define(DEF_SUPERVISOR_TERMINATE_TIMEOUT, infinity).

-define(DEF_DEF_CHILDSPEC_PLAN, fun director:plan/4).
-define(DEF_DEF_CHILDSPEC_TERMINATE_TIMEOUT, 0).
-define(DEF_DEF_CHILDSPEC_MODULES, []).
-define(DEF_DEF_CHILDSPEC, #{plan => ?DEF_DEF_CHILDSPEC_PLAN
                            ,terminate_timeout => ?DEF_DEF_CHILDSPEC_TERMINATE_TIMEOUT
                            ,modules => ?DEF_DEF_CHILDSPEC_MODULES}).