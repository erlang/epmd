%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2016. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
%%
%% File:    epmd.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2016-03-12
%%

-module(epmd).

-export([main/1]).

-include("epmd.hrl").

main(Args) ->
    try parse_args(Args) of
        error ->
            usage();
        {interactive, names} ->
            get_names();
        {interactive, normal} ->
            ok = application:start(epmd),
            receive after infinity -> ok end
    catch
        C:E ->
            io:format("badness ~w:~w~n~p~n", [C,E,erlang:get_stacktrace()])
    end,
    init:stop().

parse_args(Args) ->
    parse_args(Args, interactive, normal).
%% parse interactive commands
parse_args(["-names"|Args], Type, _Cmd) ->
    parse_args(Args, Type, names);
parse_args(["-kill"|Args], Type, _Cmd) ->
    parse_args(Args, Type, kill);
parse_args(["-stop", _Name|Args], Type, _Cmd) ->
    parse_args(Args, Type, stop);
parse_args(["-systemd"|Args], Type, _Cmd) ->
    parse_args(Args, Type, systemd);

%% parse options
parse_args(["-daemon"|Args], _Type, Cmd) ->
    application:set_env(epmd, daemon, true),
    parse_args(Args, daemon, Cmd);
parse_args(["-relaxed_command_check"|Args], Type, Cmd) ->
    application:set_env(epmd, relaxed_command_check, true),
    parse_args(Args, Type, Cmd);
parse_args(["-port",PortArg|Args], Type, Cmd) ->
    Port = list_to_integer(PortArg),
    application:set_env(epmd, port, Port),
    parse_args(Args, Type, Cmd);
parse_args(["-address",AddrList|Args], Type, Cmd) ->
    Ls = string:tokens(AddrList,","),
    F = fun(Str) ->
                {ok,Addr} = inet_parse:address(Str),
                Addr
        end,
    Addrs = lists:map(F,Ls),
    application:set_env(epmd, adddres, Addrs),
    parse_args(Args, Type, Cmd);

%% parse debug
parse_args(["-d"|Args], Type, Cmd) ->
    application:set_env(epmd, debug, true),
    parse_args(Args, Type, Cmd);
parse_args(["-debug"|Args], Type, Cmd) ->
    application:set_env(epmd, debug, true),
    parse_args(Args, Type, Cmd);
parse_args(["-packet_timeout",T|Args], Type, Cmd) ->
    Time = list_to_integer(T),
    application:set_env(epmd, packet_timeout, Time),
    parse_args(Args, Type, Cmd);
parse_args(["-delay_accept",T|Args], Type, Cmd) ->
    Time = list_to_integer(T),
    application:set_env(epmd, delay_accept, Time),
    parse_args(Args, Type, Cmd);
parse_args(["-delay_write",T|Args], Type, Cmd) ->
    Time = list_to_integer(T),
    application:set_env(epmd, delay_write, Time),
    parse_args(Args, Type, Cmd);
parse_args([], Type, Cmd) ->
    {Type, Cmd};
parse_args([A|_], _, _) ->
    io:format("unrecognized argument: ~p~n", [A]),
    error.

get_names() ->
    Port = get_port(),
    case request(get_address(), Port, <<?EPMD_NAMES_REQ>>) of
        {ok, <<_:32,Names/binary>>} ->
            io:format("epmd: up and running on port ~w with data:~n", [Port]),
            io:format("~ts", [Names]),
            ok;
        error ->
            io:format(standard_error, "epmd: Cannot connect to ~ts epmd~n", ["local"]),
            ok
    end.

get_address() ->
    Addr = "0.0.0.0",
    Addr.

get_port() ->
    application:get_env(epmd, port, ?EPMD_DEFAULT_PORT).

request(Addr, Port, Req) ->
    request(Addr, Port, Req, 5000).

request(Addr, Port, Req, Tmo) ->
    case gen_tcp:connect(Addr, Port, [binary,
                                      {packet, 2},
                                      {active, false}],
                         Tmo) of
        {ok, Fd} ->
            ok = gen_tcp:send(Fd, Req),
            inet:setopts(Fd, [{packet, raw}]),
            {ok, Res} = gen_tcp:recv(Fd, 0, Tmo),
            ok = gen_tcp:close(Fd),
            {ok, Res};
        {error, econnrefused} ->
            error
    end.


%% usage
usage() ->
    io:put_chars(usage_string()).

usage_string() ->
    ["usage: epmd [-d|-debug] [DbgExtra...] [-address List]\n",
     "            [-port No] [-daemon] [-relaxed_command_check]\n",
     "       epmd [-d|-debug] [-port No] [-names|-kill|-stop name]\n\n",
     "See the Erlang epmd manual page for info about the usage.\n\n",
     "Regular options\n",
     "    -address List\n",
     "        Let epmd listen only on the comma-separated list of IP\n",
     "        addresses (and on the loopback interface).\n",
     "    -port No\n",
     "        Let epmd listen to another port than default ",
     integer_to_list(?EPMD_DEFAULT_PORT),"\n",
     "    -d\n",
     "    -debug\n",
     "        Enable debugging. This will give a log to\n",
     "        the standard error stream. It will shorten\n",
     "        the number of saved used node names to 5.\n\n",
     "        If you give more than one debug flag you may\n",
     "        get more debugging information.\n",
     "    -daemon\n",
     "        Start epmd detached (as a daemon)\n",
     "    -relaxed_command_check\n",
     "        Allow this instance of epmd to be killed with\n",
     "        epmd -kill even if there are registered nodes.\n",
     "        Also allows forced unregister (epmd -stop).\n",
     "\nDbgExtra options\n",
     "    -packet_timeout Seconds\n",
     "        Set the number of seconds a connection can be\n",
     "        inactive before epmd times out and closes the\n",
     "        connection (default 60).\n\n",
     "    -delay_accept Seconds\n",
     "        To simulate a busy server you can insert a\n",
     "        delay between epmd gets notified about that\n",
     "        a new connection is requested and when the\n",
     "        connections gets accepted.\n\n",
     "    -delay_write Seconds\n",
     "        Also a simulation of a busy server. Inserts\n",
     "        a delay before a reply is sent.\n",
     "\nInteractive options\n",
     "    -names\n",
     "        List names registered with the currently running epmd\n",
     "    -kill\n",
     "        Kill the currently running epmd\n",
     "        (only allowed if -names show empty database or\n",
     "        -relaxed_command_check was given when epmd was started).\n",
     "    -stop Name\n",
     "        Forcibly unregisters a name with epmd\n",
     "        (only allowed if -relaxed_command_check was given when \n",
     "        epmd was started).\n",
     "    -systemd\n",
     "        Wait for socket from systemd. The option makes sense\n",
     "        when started from .socket unit.\n"].
