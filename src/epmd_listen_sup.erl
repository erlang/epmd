%%
%% %CopyrightBegin%
%% 
%% Copyright Peer Stritzinger GmbH 2013-2015. All Rights Reserved.
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% 
%% %CopyrightEnd%
%%
%%

-module(epmd_listen_sup).
-behaviour(supervisor).
-include("erl_epmd.hrl").

-export([start_link/0, start_listener/0]).
-export([init/1]).

-define(CHILD(I,Type,Args), {I,{I,start_link,[Args]},temporary,3000,Type,[I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    start_listener().

start_listener() ->
    supervisor:start_child(?MODULE, []).

init([]) ->
    PortNo = get_port_no(),
    Relaxed = application:get_env(epmd, relaxed_command_check, false),
    {ok, Listen} = gen_tcp:listen(PortNo, [{active, once},
                                            binary, inet6,
                                            {reuseaddr, true},
                                            {packet, 2}]),
    {ok, {{simple_one_for_one, 60, 3600},
           [?CHILD(epmd_srv, worker, [Listen,PortNo,Relaxed])]}}.
    
get_port_no() ->
    case os:getenv("ERL_EPMD_PORT") of
	false -> application:get_env(epmd, port, ?EPMD_DEFAULT_PORT);
	Port ->
	    case (catch list_to_integer(Port)) of
		N when is_integer(N) -> N;
		_ -> application:get_env(epmd, port, ?EPMD_DEFAULT_PORT)
	    end
    end.
