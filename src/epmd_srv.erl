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

-define(maxsymlen, (255*4)).

-record(env, {port,
              relaxed = false,
              socket}).

-export([start_link/1]).
-export([init/1, terminate/2,
         handle_call/3, handle_cast/2,
         handle_info/2, code_change/3]).

-include("epmd.hrl").

start_link([Socket,PortNo,Relaxed]) ->
    gen_server:start_link(?MODULE, [Socket,PortNo,Relaxed], []).

init([S,Port,Relaxed]) ->
    error_logger:info_msg("EPMD Service started~n"),
    gen_server:cast(self(), accept),
    {ok, #env{socket=S,port=Port,relaxed=Relaxed}}.

handle_call(_Req, _From, Env) ->
    {noreply, Env}.

handle_cast(accept, #env{socket=Listen}=Env) ->
    {ok, Accept} = gen_tcp:accept(Listen),
    epmd_listen_sup:start_listener(),
    {noreply, Env#env{socket=Accept}}.

handle_info({tcp, S, <<?EPMD_ALIVE2_REQ, Port:16, Type, Protocol, High:16, Low:16,
                       Len:16, Name:Len/binary, Elen:16, Extra:Elen/binary>>},
            #env{socket=S}=Env) ->
    error_logger:info_msg("EPMD Alive Request: ~p registered at ~w~n", [safe_string(Name), Port]),
    error_logger:info_msg("EPMD Alive Request: name size ~w~n", [byte_size(Name)]),
    case name_is_valid(Name) of
        ok ->
            case epmd_reg:node_reg(Name, Port, Type, Protocol, High, Low, Extra) of
                {ok, Creation} ->
                    reply(S, <<?EPMD_ALIVE2_RESP, 0, Creation:16>>);
                {error, _} ->
                    reply(S, <<?EPMD_ALIVE2_RESP, 1, 99:16>>)
            end,
            {noreply, Env};
        invalid_size ->
            %% really? .. this the way?
            error_logger:info_msg("EPMD Alive Request: Invalid size (too large)~n"),
            reply(S, <<?EPMD_ALIVE2_RESP, 1, 99:16>>),
            {noreply, Env};
        invalid_string ->
            error_logger:info_msg("EPMD Alive Request: Invalid name (nulls)~n"),
            gen_tcp:close(S),
            {stop, normal, Env}
    end;
handle_info({tcp, S, <<?EPMD_ALIVE2_REQ, _/binary>>}, #env{socket=S}=Env) ->
    %% why do we handle too large and too small differently?
    error_logger:info_msg("EPMD Alive Request: Invalid size (too small)~n"),
    gen_tcp:close(S),
    {stop, normal, Env};
handle_info({tcp, S, <<?EPMD_PORT_PLEASE2_REQ, Name/binary>>}, #env{socket=S}=Env) when byte_size(Name) =< ?maxsymlen ->
    case epmd_reg:lookup(Name) of
	{ok, Name, Port, Nodetype, Protocol, Highvsn, Lowvsn, Extra} ->
            error_logger:info_msg("EPMD Port Request: ~p -> ~w", [safe_string(Name), Port]),
	    Len = byte_size(Name),
	    Elen = byte_size(Extra),
            reply(S, <<?EPMD_PORT2_RESP, 0, Port:16, Nodetype,
                       Protocol, Highvsn:16, Lowvsn:16,
                       Len:16, Name/binary, Elen:16, Extra/binary>>);
	{error, _} ->
            error_logger:info_msg("EPMD Port Request: ~p -> Not registered", [safe_string(Name)]),
	    reply(S, <<?EPMD_PORT2_RESP, 1>>)
    end,
    gen_tcp:close(S),
    {stop, normal, Env};
handle_info({tcp, S, <<?EPMD_PORT_PLEASE2_REQ, _/binary>>}, #env{socket=S}=Env) ->
    error_logger:info_msg("EPMD Port Request: Invalid size~n"),
    gen_tcp:close(S),
    {stop, normal, Env};
handle_info({tcp, S, <<?EPMD_NAMES_REQ>>}, #env{port=ServerPort, socket=S}=Env) ->
    error_logger:info_msg("EPMD Names Request"),
    Format = "name ~s at port ~w~n",
    Nodes = iolist_to_binary([io_lib:format(Format, [Name, Port]) ||
                              {Name, Port} <- epmd_reg:nodes()]),
    reply(S, <<ServerPort:32, Nodes/binary>>),
    gen_tcp:close(S),
    {stop, normal,Env};
handle_info({tcp, S, <<?EPMD_STOP_REQ, Name/binary>>}, #env{relaxed=true,socket=S}=Env) ->
    error_logger:info_msg("EPMD Stop Request: ~p", [safe_string(Name)]),
    %% Check if local peer
    %% Check if STOP_REQ is allowed
    case epmd_reg:node_unreg(Name) of
        ok    -> reply(S, <<"STOPPED">>);
        error -> reply(S, <<"NOEXIST">>)
    end,
    gen_tcp:close(S),
    {stop, normal,Env};
handle_info({tcp, S, <<?EPMD_STOP_REQ, Name/binary>>}, #env{socket=S}=Env) ->
    error_logger:info_msg("EPMD Stop Request: ~p (Disallowed)", [safe_string(Name)]),
    gen_tcp:close(S),
    {stop, normal,Env};
handle_info({tcp, S, <<?EPMD_KILL_REQ>>}, #env{relaxed=true,socket=S}=Env) ->
    error_logger:info_msg("EPMD Kill Request - Allowed (Relaxed)"),
    %% Check if local peer
    reply(S, <<"OK">>),
    erlang:halt(),
    gen_tcp:close(S),
    {stop, normal, Env};
handle_info({tcp, S, <<?EPMD_KILL_REQ>>}, #env{socket=S}=Env) ->
    case epmd_reg:nodes() of
        [] ->
            error_logger:info_msg("EPMD Kill Request - Allowed"),
            %% Check if local peer
            reply(S, <<"OK">>),
            erlang:halt();
        _ ->
            error_logger:info_msg("EPMD Kill Request - Disallowed (live nodes)"),
            reply(S, <<"NO">>)
    end,
    gen_tcp:close(S),
    {stop, normal,Env};
handle_info({tcp, S, <<>>}, #env{socket=S}=Env) ->
    error_logger:info_msg("EPMD Empty Request"),
    gen_tcp:close(S),
    {stop, normal, Env};
handle_info({tcp_closed, S}, #env{socket=S}=Env) ->
    {stop, normal, Env};
handle_info(Info, Env) ->
    error_logger:error_msg("Unhandled info ~p~n", [Info]),
    {noreply, Env}.

terminate(_Reason, _Env) ->
    ok.

code_change(_OldVsn, Env, _Extra) ->
    {ok, Env}.

reply(S, Data) ->
    %% we need {packet, 2} on receive but raw on send
    inet:setopts(S, [{packet, raw}]),
    gen_tcp:send(S, Data),
    inet:setopts(S, [{packet, 2}, {active, once}]).


safe_string(<<Name:255/binary, _/binary>>) -> Name;
safe_string(Name) -> Name.

%% check for valid name
%% - valid utf8
%% - no nulls
%% - total length no more than maxsymlen
%% - total characters no more than 255
name_is_valid(Name) when byte_size(Name) > ?maxsymlen ->
    invalid_size;
name_is_valid(Name) when is_binary(Name) ->
    Ls = unicode:characters_to_list(Name),
    name_is_valid(Ls, 0).

name_is_valid(_, N) when N > 255 -> invalid_size;
name_is_valid([], _) -> ok;
name_is_valid([0|_], _) -> invalid_string;
name_is_valid([_|Ls], N) -> name_is_valid(Ls, N + 1).
