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

-record(env, {socket :: gen_tcp:socket(),
              port :: non_neg_integer(),
              %% debug settings
              relaxed = false :: boolean(),
              delay_write = 0 :: non_neg_integer()}).

-export([start_link/1]).
-export([init/1, terminate/2,
         handle_call/3, handle_cast/2,
         handle_info/2, code_change/3]).

-include("epmd.hrl").

start_link([Socket,PortNo,Relaxed,DelayWrite]) ->
    gen_server:start_link(?MODULE, [Socket,PortNo,Relaxed,DelayWrite], []).

init([S,Port,Relaxed,DelayWrite]) ->
    gen_server:cast(self(), accept),
    {ok, #env{socket=S,port=Port,
              relaxed=Relaxed,
              delay_write=DelayWrite}}.

handle_call(_Req, _From, Env) ->
    {noreply, Env}.

handle_cast(accept, #env{socket=Listen}=Env) ->
    {ok, Accept} = gen_tcp:accept(Listen),
    epmd_listen_sup:start_listener(),
    {noreply, Env#env{socket=Accept}}.

handle_info({tcp, S, <<?EPMD_ALIVE2_REQ, Port:16, Type, Protocol, High:16, Low:16,
                       Len:16, Name:Len/binary, Elen:16, Extra:Elen/binary>>},
            #env{socket=S}=Env) ->
    case {name_is_valid(Name),extra_is_valid(Extra)} of
        {ok,ok} ->
            case epmd_reg:node_reg(Name, Port, Type, Protocol, High, Low, Extra) of
                {ok, Creation} ->
                    error_logger:info_msg("epmd - ALIVE2_REQ: registering '~ts:~w' on port ~w~n", [safe_string(Name),
                                                                                                   Creation, Port]),
                    reply(S, <<?EPMD_ALIVE2_RESP, 0, Creation:16>>, Env);
                {error, _} ->
                    error_logger:info_msg("epmd - ALIVE2_REQ: failed to register '~ts'~n", [safe_string(Name)]),
                    reply(S, <<?EPMD_ALIVE2_RESP, 1, 99:16>>, Env)
            end,
            {noreply, Env};
        {invalid_string,_} ->
            error_logger:info_msg("epmd - ALIVE2_REQ: invalid name (nulls)~n"),
            gen_tcp:close(S),
            {stop, normal, Env};
        {NameValid,ExtraValid} when NameValid =:= invalid_size orelse
                                    ExtraValid =:= invalid ->
            %% really? .. this the way?
            error_logger:info_msg("epmd - ALIVE2_REQ: invalid size (too large)~n"),
            reply(S, <<?EPMD_ALIVE2_RESP, 1, 99:16>>, Env),
            {noreply, Env}
    end;
handle_info({tcp, S, <<?EPMD_ALIVE2_REQ, _/binary>>}, #env{socket=S}=Env) ->
    %% why do we handle too large and too small differently?
    error_logger:info_msg("epmd - ALIVE2_REQ: invalid size (too small)~n"),
    gen_tcp:close(S),
    {stop, normal, Env};
handle_info({tcp, S, <<?EPMD_PORT_PLEASE2_REQ, Name/binary>>}, #env{socket=S}=Env) when byte_size(Name) =< ?maxsymlen ->
    case epmd_reg:lookup(Name) of
	{ok, Name, Port, Nodetype, Protocol, Highvsn, Lowvsn, Extra} ->
            error_logger:info_msg("epmd - PORT_PLEASE2_REQ: '~ts' is registered on port ~w~n", [safe_string(Name), Port]),
	    Len = byte_size(Name),
	    Elen = byte_size(Extra),
            reply(S, <<?EPMD_PORT2_RESP, 0, Port:16, Nodetype,
                       Protocol, Highvsn:16, Lowvsn:16,
                       Len:16, Name/binary, Elen:16, Extra/binary>>, Env);
	{error, _} ->
            error_logger:info_msg("epmd - PORT_PLEASE2_REQ: '~ts' is not registered~n", [safe_string(Name)]),
	    reply(S, <<?EPMD_PORT2_RESP, 1>>, Env)
    end,
    gen_tcp:close(S),
    {stop, normal, Env};
handle_info({tcp, S, <<?EPMD_PORT_PLEASE2_REQ, _/binary>>}, #env{socket=S}=Env) ->
    error_logger:info_msg("epmd - PORT_PLEASE2_REQ: invalid name size (too large)~n"),
    gen_tcp:close(S),
    {stop, normal, Env};
handle_info({tcp, S, <<?EPMD_NAMES_REQ>>}, #env{port=ServerPort, socket=S}=Env) ->
    error_logger:info_msg("epmd - NAMES_REQ~n"),
    Format = "name ~s at port ~w~n",
    Nodes = iolist_to_binary([io_lib:format(Format, [Name, Port]) ||
                              {Name, Port} <- epmd_reg:nodes()]),
    reply(S, <<ServerPort:32, Nodes/binary>>, Env),
    gen_tcp:close(S),
    {stop, normal,Env};
handle_info({tcp, S, <<?EPMD_STOP_REQ, Name/binary>>}, #env{relaxed=true,socket=S}=Env) ->
    error_logger:info_msg("epmd - STOP_REQ: stop request from '~ts'~n", [safe_string(Name)]),
    %% Check if local peer
    %% Check if STOP_REQ is allowed
    case epmd_reg:node_unreg(Name) of
        ok    -> reply(S, <<"STOPPED">>, Env);
        error -> reply(S, <<"NOEXIST">>, Env)
    end,
    gen_tcp:close(S),
    {stop, normal,Env};
handle_info({tcp, S, <<?EPMD_STOP_REQ, Name/binary>>}, #env{socket=S}=Env) ->
    error_logger:info_msg("epmd - STOP_REQ: stop request from '~ts' DISALLOWED~n", [safe_string(Name)]),
    gen_tcp:close(S),
    {stop, normal,Env};
handle_info({tcp, S, <<?EPMD_KILL_REQ>>}, #env{relaxed=true,socket=S}=Env) ->
    error_logger:info_msg("epmd - KILL_REQ: allowed (relaxed)~n"),
    %% Check if local peer
    reply(S, <<"OK">>, Env),
    erlang:halt(),
    gen_tcp:close(S),
    {stop, normal, Env};
handle_info({tcp, S, <<?EPMD_KILL_REQ>>}, #env{socket=S}=Env) ->
    case epmd_reg:nodes() of
        [] ->
            error_logger:info_msg("epmd - KILL_REQ: allowed (no live nodes)~n"),
            %% Check if local peer
            reply(S, <<"OK">>, Env),
            erlang:halt();
        _ ->
            error_logger:info_msg("epmd - KILL_REQ: DISALLOWED (live nodes)~n"),
            reply(S, <<"NO">>, Env)
    end,
    gen_tcp:close(S),
    {stop, normal,Env};
handle_info({tcp, S, <<>>}, #env{socket=S}=Env) ->
    gen_tcp:close(S),
    {stop, normal, Env};
handle_info({tcp_closed, S}, #env{socket=S}=Env) ->
    {stop, normal, Env};
handle_info(Info, Env) ->
    error_logger:error_msg("epmd - unrecognized request: ~p~n", [Info]),
    {noreply, Env}.

terminate(_Reason, _Env) ->
    ok.

code_change(_OldVsn, Env, _Extra) ->
    {ok, Env}.

reply(S, Data, #env{delay_write=Delay}) ->
    sleep_seconds(Delay), %% debug
    %% we need {packet, 2} on receive but raw on send
    inet:setopts(S, [{packet, raw}]),
    gen_tcp:send(S, Data),
    inet:setopts(S, [{packet, 2}, {active, once}]).


safe_string(<<Name:255/binary, _/binary>>) ->
    <<Name/binary,"..">>;
safe_string(Name) ->
    Name.

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

name_is_valid(_, N) when N > 255 ->
    invalid_size;
name_is_valid([], _) ->
    ok;
name_is_valid([0|_], _) ->
    invalid_string;
name_is_valid([_|Ls], N) ->
    name_is_valid(Ls, N + 1).

extra_is_valid(Extra) when byte_size(Extra) > ?maxsymlen ->
    invalid;
extra_is_valid(_) ->
    ok.

sleep_seconds(0) ->
    ok;
sleep_seconds(Time) ->
    timer:sleep(Time*1000).
