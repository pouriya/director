%%% --------------------------------------------------------------------
%%% BSD 3-Clause License
%%%
%%% Copyright (c) 2017-2018, Pouriya Jahanbakhsh
%%% (pouriya.jahanbakhsh@gmail.com)
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright
%%% notice, this list of conditions and the following disclaimer.
%%%
%%% 2. Redistributions in binary form must reproduce the above copyright
%%% notice, this list of conditions and the following disclaimer in the
%%% documentation and/or other materials provided with the distribution.
%%%
%%% 3. Neither the name of the copyright holder nor the names of its
%%% contributors may be used to endorse or promote products derived from
%%% this software without specific prior written permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%%% FOR A  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%%% COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.
%%% --------------------------------------------------------------------

-define(DEFAULT_START_OPTIONS, []).
-define(DEFAULT_STOP_TIMEOUT, 5000).
-define(DEFAULT_CALL_TIMEOUT, 5000).





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
-define(GET_COUNT_TAG, 'get_count').
-define(GET_DEFAULT_CHILDSPEC, 'get_default_childspec').
-define(CHANGE_DEFAULT_CHILDSPEC, 'change_default_childspec').






-define(DEFAULT_PLAN_ELEMENT, fun director:default_plan_element_fun/2).
-define(DEFAULT_PLAN, [?DEFAULT_PLAN_ELEMENT]).
-define(DEFAULT_COUNT, 1).
-define(DEFAULT_TYPE, worker).
-define(DEFAULT_APPEND, false).
-define(DEFAULT_WORKER_TERMINATE_TIMEOUT, 1000).
-define(DEFAULT_SUPERVISOR_TERMINATE_TIMEOUT, infinity).






-define(DEFAULT_DEFAULT_CHILDSPEC_PLAN, []).
-define(DEFAULT_DEFAULT_CHILDSPEC_COUNT, 0).
-define(DEFAULT_DEFAULT_CHILDSPEC_TERMINATE_TIMEOUT, 0).
-define(DEFAULT_DEFAULT_CHILDSPEC_MODULES, []).
-define(DEFAULT_DEFAULT_CHILDSPEC
       ,#{plan => ?DEFAULT_DEFAULT_CHILDSPEC_PLAN
         ,count => ?DEFAULT_DEFAULT_CHILDSPEC_COUNT
         ,terminate_timeout =>
          ?DEFAULT_DEFAULT_CHILDSPEC_TERMINATE_TIMEOUT
         ,modules => ?DEFAULT_DEFAULT_CHILDSPEC_MODULES}).