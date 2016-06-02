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

-module(epmd_reg).
-behaviour(gen_server).

-export([start_link/0, node_reg/7, node_unreg/1, lookup/1, nodes/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 code_change/3, terminate/2]).

-type creation() :: 1..3.
-type name() :: binary().

-record(node, {symname  :: name(),
               port     :: inet:port_number(),
               nodetype :: byte(), %% specify
               protocol :: byte(), %% specify
               highvsn  :: 0..65535,
               lowvsn   :: 0..65535,
               extra    :: binary(),
               creation :: creation(),
               monref   :: reference()}).

-record(state, {reg = []        :: [#node{}],
                unreg = []      :: [#node{}],
                unreg_count = 0 :: non_neg_integer()}).

-define(max_unreg_count, 1000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec node_reg(Name, Port, Nodetype, Protocol, Highvsn, Lowvsn, Extra) ->
          {'ok', creation()} | {'error','name_occupied'} when
      Name :: name(),
      Port :: inet:port_number(),
      Nodetype :: byte(),
      Protocol :: byte(),
      Highvsn :: 0..65535,
      Lowvsn :: 0..65535,
      Extra :: binary().

node_reg(Name, Port, Nodetype, Protocol, Highvsn, Lowvsn, Extra) ->
    gen_server:call(?MODULE, {node_reg, Name, Port, Nodetype, 
			      Protocol, Highvsn, Lowvsn, Extra, self()}).

-spec node_unreg(Name :: name()) -> 'ok' | 'error'.

node_unreg(Name) ->
    gen_server:call(?MODULE, {node_unreg, Name}).

-spec lookup(Name :: binary()) -> Result when
      Result :: {'ok', Name, Port, Nodetype, Protocol, Highvsn, Lowvsn, Extra}
              | {'error','not_found'},
      Name :: binary(),
      Port :: inet:port_number(),
      Nodetype :: byte(),
      Protocol :: byte(),
      Highvsn :: 0..65535,
      Lowvsn :: 0..65535,
      Extra :: binary().

lookup(Name) ->
    gen_server:call(?MODULE, {lookup, Name}).

-spec nodes() -> [{name(),inet:port_number()}].

nodes() ->
    gen_server:call(?MODULE, nodes).

init([]) ->
    {ok, #state{}}.

handle_call({node_reg, Name, Port, Nodetype, Protocol, Highvsn, Lowvsn, 
	     Extra, Srv_pid}, _From, 
	    #state{reg=Reg, unreg=Unreg, unreg_count=Uc}=State) ->
    case lists:keyfind(Name, #node.symname, Reg) of
	#node{} -> {reply, {error, name_occupied}, State};
	false ->
	    Mref = monitor(process, Srv_pid),
	    %% if the following doesn't make sense: I'm just trying to emulate
	    %% what the original epmd does 
	    case lists:keytake(Name, #node.symname, Unreg) of
		{value, #node{creation=Cr}, New_unreg} -> 
		    Creation = (Cr rem 3) + 1,
		    New_unreg_count = Uc-1;
		false -> 
		    Creation = (time_seconds() rem 3) + 1,
		    New_unreg = Unreg,
		    New_unreg_count = Uc
	    end,
	    Node = #node{symname=Name, port=Port, nodetype=Nodetype,
			 protocol=Protocol, highvsn=Highvsn, lowvsn=Lowvsn,
			 extra=Extra, creation=Creation, monref=Mref},
	    {reply, {ok, Creation}, 
	     State#state{reg=[Node|Reg], 
			 unreg=New_unreg, unreg_count=New_unreg_count}}
    end;
handle_call({lookup, Name}, _From, #state{reg=Reg}=State) ->
    case lists:keyfind(Name, #node.symname, Reg) of
	#node{symname=Name, port=Port, nodetype=Nodetype,
	      protocol=Protocol, highvsn=Highvsn, lowvsn=Lowvsn,
	      extra=Extra} -> 
	    {reply, {ok, Name, Port, Nodetype, Protocol, 
		     Highvsn, Lowvsn, Extra}, State};
	false ->
	    {reply, {error, notfound}, State}
    end;
handle_call(nodes, _From, #state{reg=Reg}=State) ->
    {reply,[{Name,Port}||#node{symname=Name,port=Port}<-Reg],State};
handle_call({node_unreg, Name}, _From, #state{reg=Reg,unreg=Unreg,unreg_count=Uc}=S) ->
    case lists:keytake(Name, #node.symname, Reg) of
        {value, Node, New_reg} ->
            error_logger:info_msg("epmd: unregistering '~ts:~w' on port ~w~n",
                                  [safe_string(Node#node.symname),
                                   Node#node.creation,
                                   Node#node.port]),
            S1 = S#state{reg=New_reg, unreg=[Node|Unreg], unreg_count=Uc+1},
            {reply, ok, S1};
        _ ->
            {reply, error, S}
    end.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({'DOWN', Mref, _, _, _}, 
	    #state{reg=Reg, unreg=Unreg, unreg_count=Uc}=State) 
  when Uc < 2*?max_unreg_count ->
    {value, Node, New_reg} = lists:keytake(Mref, #node.monref, Reg),
    error_logger:info_msg("epmd: unregistering '~ts:~w' on port ~w~n",
                          [safe_string(Node#node.symname),
                           Node#node.creation,
                           Node#node.port]),
    {noreply, State#state{reg=New_reg, unreg=[Node|Unreg], 
			  unreg_count=Uc+1}};
handle_info({'DOWN', Mref, _, _, _}, 
	    #state{reg=Reg, unreg=Unreg, unreg_count=Uc}=State) 
  when Uc =:= 2*?max_unreg_count ->
    {value, Node, New_reg} = lists:keytake(Mref, #node.monref, Reg),
    error_logger:info_msg("epmd: unregistering '~ts:~w' on port ~w~n",
                          [safe_string(Node#node.symname),
                           Node#node.creation,
                           Node#node.port]),
    {Ur, _} = lists:split(?max_unreg_count, Unreg),
    {noreply, State#state{reg=New_reg, unreg=[Node|Ur], 
			  unreg_count=?max_unreg_count+1}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

time_seconds() ->
    {_, Sec, _} = os:timestamp(),
    Sec.

safe_string(<<Name:255/binary, _/binary>>) ->
    <<Name/binary,"..">>;
safe_string(Name) ->
    Name.
