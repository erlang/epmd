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

-module(epmd_srv).
-behaviour(gen_server).

-record(state, {socket}).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 code_change/3, terminate/2]).

-include("erl_epmd.hrl").

start_link(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).

init([Socket]) ->
    gen_server:cast(self(), accept),
    {ok, #state{socket=Socket}}.

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(accept, #state{socket=Listen}=State) ->
    {ok, Accept} = gen_tcp:accept(Listen),
    epmd_listen_sup:start_listener(),
    {noreply, State#state{socket=Accept}}.

handle_info({tcp, Socket, 
	     <<?EPMD_ALIVE2_REQ, Tcp_port:16, Type, Protocol, High:16, Low:16, 
	       Len:16, Name:Len/binary, Elen:16, Extra:Elen/binary>>}, 
	    #state{socket=Socket}=State) ->
    case epmd_reg:node_reg(Name, Tcp_port, Type, Protocol, High, Low, Extra) of
	{ok, Creation} ->
	    do_reply(Socket, <<?EPMD_ALIVE2_RESP, 0, Creation:16>>);
	{error, _} ->
	    do_reply(Socket, <<?EPMD_ALIVE2_RESP, 1, 99:16>>)
    end,
    {noreply, State};
handle_info({tcp, Socket, <<?EPMD_PORT_PLEASE2_REQ, Name/binary>>}, 
	    #state{socket=Socket}=State) ->
    case epmd_reg:lookup(Name) of
	{ok, Name, Port, Nodetype, Protocol, Highvsn, Lowvsn, Extra} ->
	    Len = byte_size(Name),
	    Elen = byte_size(Extra),
	    do_reply(Socket, <<?EPMD_PORT2_RESP, 0, Port:16, Nodetype,
			       Protocol, Highvsn:16, Lowvsn:16, 
			       Len:16, Name/binary, Elen:16, Extra/binary>>);
	{error, _} ->
	    do_reply(Socket, <<?EPMD_PORT2_RESP, 1>>)
    end,
    gen_tcp:close(Socket),
    {stop, normal, State};
handle_info({tcp, Socket, <<?EPMD_KILL_REQ>>}, #state{socket=Socket}=S) ->
    %% Check if local peer
    %% Check if KILL_REQ is allowed
    do_reply(Socket, <<"OK">>),
    erlang:halt(),
    {noreply, S};
handle_info({tcp_closed, Socket}, #state{socket=Socket}=State) ->
    {stop, normal, State};
handle_info(Info, State) ->
    error_logger:error_msg("Unhandled info ~p~n", [Info]),
    {noreply, State}.

do_reply(Socket, Data) ->
    %% we need {packet, 2} on receive but raw on send
    inet:setopts(Socket, [{packet, raw}]),
    gen_tcp:send(Socket, Data),
    inet:setopts(Socket, [{packet, 2}, {active, once}]).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
