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
%% @author   Pouriya Jahanbakhsh <pouriya.jahanbakhsh@gmail.com>
%% @version  17.6.4
%% @doc
%%           Debug helpers.
%% @end
%% @hidden
%% ---------------------------------------------------------------------


-module(director_debug).
-author("pouriya.jahanbakhsh@gmail.com").


%% ---------------------------------------------------------------------
%% Exports:





%% API:
-export([debug/3
        ,debug_options/2
        ,progress_report/2
        ,error_report/4]).





%% ---------------------------------------------------------------------
%% Records & Macros & Includes:





-define(DEFAULT_DEBUG, []).





%% Dependencies:
%%  ?DEFAULT_GEN_CALL_TAG
-include("internal/director_defaults.hrl").





%% ---------------------------------------------------------------------
%% API functions:





debug_options(Name, Opts) ->
    case proplists:lookup(debug, Opts) of
        {_,Options} ->
            try sys:debug_options(Options)
            catch _:_ ->
                error_logger:format(
                    "~p: ignoring erroneous debug options - ~p~n",
                    [Name,Options]),
                ?DEFAULT_DEBUG
            end;
        none ->
            ?DEFAULT_DEBUG
    end.







progress_report(Name, Child) ->
    error_logger:info_report(progress
                            ,[{supervisor, Name}
                             ,{started
                              ,director_wrapper:c_r2p(Child)}]).







error_report(Name, ErrorContext, Reason, Child) ->
    error_logger:error_report(supervisor_report
                             ,[{supervisor, Name}
                              ,{errorContext, ErrorContext}
                              ,{reason, Reason}
                              ,{offender
                               ,director_wrapper:c_r2p(Child)}]).







%% @hidden
debug([], _Name, _MsgInfo) ->
    [];
debug(Dbg, Name, MsgInfo) ->
    sys:handle_debug(Dbg, fun print/3, Name, MsgInfo).





%% ---------------------------------------------------------------------
%% Internal functions:





print(IODev, {in, Msg}, Name) ->
    io:format(IODev
             ,"*DBG* director \"~p\" got message \"~p\" ~n"
             ,[Name, Msg]);

print(IODev, {?GEN_CALL_TAG, {Pid, Tag}, Request}, Name) ->
    io:format(IODev
             ,"*DBG* director \"~p\" got request \"~p\" from \"~p\" wit"
              "h tag \"~p\" ~n"
             ,[Name, Request, Pid, Tag]);

print(IODev, {'EXIT', Pid, Reason}, Name) ->
    io:format(IODev
             ,"*DBG* director \"~p\" got exit signal for pid \"~p\" wit"
              "h reason \"~p\"~n"
             ,[Name, Pid, Reason]);

print(IODev, {timeout, Ref, Id}, Name) ->
    io:format(IODev
             ,"*DBG* director \"~p\" got restart event for child-id \"~"
              "p\" with timer reference \"~p\"~n"
             ,[Name, Id, Ref]);

print(IODev, {out, To, Msg}, Name) ->
    io:format(IODev
             ,"*DBG* director \"~p\" sent \"~p\" to \"~p\"~n"
             ,[Name, Msg, To]);

print(IODev, {plan, Id, Strategy}, Name) ->
    io:format(IODev
             ,"*DBG* director \"~p\" is running plan \"~p\" for id \"~p\"~n"
             ,[Name, Strategy, Id]);

print(IODev, Other, Name) ->
    io:format(IODev
             ,"*DBG* director \"~p\" got debug \"~p\" ~n"
             ,[Name, Other]).