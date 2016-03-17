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

-export([start_link/0, start_listener/0]).
-export([init/1]).

-define(erlang_daemon_port, 4369).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    start_listener().

start_listener() ->
    supervisor:start_child(?MODULE, []).

init([]) ->
    {ok, Listen} = gen_tcp:listen(get_port_no(), [{active, once},
						  binary,
                                                  {reuseaddr, true},
						  {packet, 2}]),
    {ok, {{simple_one_for_one, 60, 3600},
	  [{epmd_srv,
	    {epmd_srv, start_link, [Listen]},
	    temporary, 3000, worker, [epmd_srv]}]}}.
    
get_port_no() ->
    case os:getenv("ERL_EPMD_PORT") of
	false ->
	    application:get_env(epmd, port, ?erlang_daemon_port);
	Port ->
	    case (catch list_to_integer(Port)) of
		N when is_integer(N) ->
		    N;
		_ ->
		    application:get_env(epmd, port, ?erlang_daemon_port)
	    end
    end.
