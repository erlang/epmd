Erlang Port Mapper Daemon
=========================
[![Build Status](https://travis-ci.org/erlang/epmd.svg?branch=master)](https://travis-ci.org/erlang/epmd)

An Erlang implementation of Erlang/OTPs port mapper daemon.

The Erlang code was originally written by Peer Stritzinger (@peerst) which was
a direct port of Erlang/OTPs C-implementation of EPMD.

* This code is not blessed for production use just yet.

This EPMD has two levels of interfaces. The top level being an escript, `epmd` that emulates
the behavior of Erlang/OTP's `epmd`. The escript utilizes the `epmd` application interface.

### EPMD Application

Usage:

    1> application:start(epmd).

    =INFO REPORT==== 2-Jun-2016::17:04:38 ===
    EPMD Service started
    ok

#### Environment variables

The application respects the following erlang environment variables within the epmd scope:

* `address :: [string()]` - If set, EPMD will listen only on the specified address(es) and the loopback address.
  The default behavior is to listen on all available IP addresses.
* `port :: non_neg_integer()` - Identifies which port EPMD will listen to.
   Uses OS environment variable `$ERL_EPMD_PORT` if not set or defaults to 4369 if neither is set.
* `relaxed_command_check :: boolean()` - Allows for the epmd to be taken down or forced node unregister by outside influence,
   i.e. `epmd -kill` and `epmd -stop`
* `delay_write :: non_neg_integer()` - Simulates a busy server. Delays messages before replying.

The application respects the following optional OS environment variables:

* `$ERL_EPMD_ADDRESS` - Expects a comma-separated list of IP addresses. Same behavior as `address` above.
* `$ERL_EPMD_PORT` - Expects an integer. Same behavior as `port` above.
* `$ERL_EPMD_RELAXED_COMMAND_CHECK` - Treated as `true` if set otherwise `false`. Same behavior as `relaxed_command_check`.

_Note: `$ERL_EPMD_ADDRESS` and `address` is not yet implemented._

### EPMD Escript

    usage: epmd [-d|-debug] [DbgExtra...] [-address List]
                [-port No] [-daemon] [-relaxed_command_check]
           epmd [-d|-debug] [-port No] [-names|-kill|-stop name]

    See the Erlang epmd manual page for info about the usage.

    Regular options
        -address List
            Let epmd listen only on the comma-separated list of IP
            addresses (and on the loopback interface).
        -port No
            Let epmd listen to another port than default 4369
        -d
        -debug
            Enable debugging. This will give a log to
            the standard error stream. It will shorten
            the number of saved used node names to 5.

            If you give more than one debug flag you may
            get more debugging information.
        -daemon
            Start epmd detached (as a daemon)
        -relaxed_command_check
            Allow this instance of epmd to be killed with
            epmd -kill even if there are registered nodes.
            Also allows forced unregister (epmd -stop).

    DbgExtra options
        -delay_write Seconds
            Also a simulation of a busy server. Inserts
            a delay before a reply is sent.

    Interactive options
        -names
            List names registered with the currently running epmd
        -kill
            Kill the currently running epmd
            (only allowed if -names show empty database or
            -relaxed_command_check was given when epmd was started).
        -stop Name
            Forcibly unregisters a name with epmd
            (only allowed if -relaxed_command_check was given when
            epmd was started).


_Note: `-address`, `-debug` and `-daemon` is not yet implemented._
