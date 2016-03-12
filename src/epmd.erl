%%
%% Copyright (C) 2016 Björn-Egil Dahlberg
%%
%% File:    epmd.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2016-03-12
%%

-module(epmd).

-export([main/1]).

-define(EPMD_DEFAULT_PORT, 4369).

main(Args) ->
    try parse_args(Args) of
        ok ->
            ok = application:start(epmd),
            receive after infinity -> ok end;
        error ->
            usage()
    catch
        C:E ->
            io:format("badness ~w:~w~n~p~n", [C,E,erlang:get_stacktrace()])
    end,
    init:stop().

%% parse interactive commands
parse_args(["-names"|Args]) ->
    parse_args(Args);
parse_args(["-kill"|Args]) ->
    parse_args(Args);
parse_args(["-stop", _Name|Args]) ->
    parse_args(Args);
parse_args(["-systemd"|Args]) ->
    parse_args(Args);

%% parse options
parse_args(["-daemon"|Args]) ->
    application:set_env(epmd, daemon, true),
    parse_args(Args);
parse_args(["-relaxed_command_check"|Args]) ->
    application:set_env(epmd, relaxed_command_check, true),
    parse_args(Args);
parse_args(["-port",PortArg|Args]) ->
    Port = list_to_integer(PortArg),
    application:set_env(epmd, port, Port),
    parse_args(Args);
parse_args(["-address",AddrList|Args]) ->
    Ls = string:tokens(AddrList,","),
    F = fun(Str) ->
                {ok,Addr} = inet_parse:address(Str),
                Addr
        end,
    _ = lists:map(F,Ls),
    parse_args(Args);

%% parse debug
parse_args(["-d"|Args]) ->
    application:set_env(epmd, debug, true),
    parse_args(Args);
parse_args(["-debug"|Args]) ->
    application:set_env(epmd, debug, true),
    parse_args(Args);
parse_args(["-packet_timeout",T|Args]) ->
    Time = list_to_integer(T),
    application:set_env(epmd, packet_timeout, Time),
    parse_args(Args);
parse_args(["-delay_accept",T|Args]) ->
    Time = list_to_integer(T),
    application:set_env(epmd, delay_accept, Time),
    parse_args(Args);
parse_args(["-delay_write",T|Args]) ->
    Time = list_to_integer(T),
    application:set_env(epmd, delay_write, Time),
    parse_args(Args);
parse_args([]) ->
    ok;
parse_args([A|_]) ->
    io:format("unrecognized argument: ~p~n", [A]),
    error.

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

