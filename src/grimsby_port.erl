%% Copyright (c) 2023 Peter Morgan <peter.james.morgan@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.


%% @doc This module lazily opens a single port to a controller that
%% coordinates the spawning of other processes. Communication with the
%% controller is using binary erlang terms (BERT), framed with a 4
%% byte big endian length header (BURP). The controller is written in
%% Rust. Spawned processes are identified by their spawn ID, which is
%% a reference. Each request and response to the controller are
%% correlated with an interaction ID, which is also a
%% reference. Asynchronous notifications are emitted from the
%% controller, e.g., data received from the spawned process, eof or
%% closure of a stream, or the exit status of the process.
%%
%% Notifications use gen:send_request/4, with the following requests:
%%
%% <dl>
%%
%% <dt>```{grimsby_port, {error, SpawnId, Stream}}'''</dt><dd>indicates that an
%% error has occured on a specific stream of a spawned process.</dd>
%%
%% <dt>```{grimsby_port, {Stream, SpawnId, Data}}'''</dt><dd>data has been
%% received from a stream of a spawned process.</dd>
%%
%% <dt>```{grimsby_port, {eof, SpawnId, Stream}}'''</dt><dd>end of file has been
%% indicated on a stream for a spawned process.</dd>
%%
%% <dt>```{grimsby_port, {exit, SpawnId, StatusCode}}'''</dt><dd>the spawned
%% process has exited with a status code or signal.</dd>
%%
%% </dl>

-module(grimsby_port).


-behaviour(gen_statem).
-export([all/0]).
-export([callback_mode/0]).
-export([close/2]).
-export([handle_event/4]).
-export([init/1]).
-export([kill/1]).
-export([run/1]).
-export([send/2]).
-export([start_link/0]).
-export([wait_for_exit/1]).
-export_type([run_arg/0]).
-import(grimsby_statem, [nei/1]).
-include_lib("kernel/include/logger.hrl").


start_link() ->
    gen_statem:start_link({local, ?MODULE},
                          ?MODULE,
                          [],
                          envy_gen:options(?MODULE)).


-type run_arg() :: #{executable := string(),
                     args => [string()],
                     envs => #{string() => string()},
                     arg0 => string(),
                     cd => file:filename(),
                     send_to => gen_statem:server_ref()}.


%% @doc spawn and run an executable
-spec run(run_arg()) -> {ok, grimsby:spawn_id()}.
run(Args) ->
    gen_statem:call(?MODULE, {?FUNCTION_NAME, Args}).


%% @doc ask the port controller to wait for the spawned process to
%% exit. Note that this function itself does not wait, and returns as
%% soon as the request has been acknowledged by the
%% controller. Asynchronously, a callback: ```{grimsby_port, {exit,
%% SpawnId, StatusCode}}''' will be received when the spawned process has
%% exited.
-spec wait_for_exit(grimsby:spawn_id()) -> ok.
wait_for_exit(SpawnId) ->
    gen_statem:call(?MODULE, {?FUNCTION_NAME, SpawnId}).


%% @doc ask the port controller to kill the spawned process.
-spec kill(grimsby:spawn_id()) -> ok.
kill(SpawnId) ->
    gen_statem:call(?MODULE, {?FUNCTION_NAME, SpawnId}).


%% @doc ask the port controller to close a stream on the spawned
%% process.
-spec close(grimsby:spawn_id(), grimsby:stream()) -> ok.
close(SpawnId, Stream) ->
    gen_statem:call(?MODULE, {?FUNCTION_NAME, SpawnId, Stream}).


%% @doc send data to the stdin of a spawned process.
-spec send(grimsby:spawn_id(), iodata()) -> ok.
send(SpawnId, IOData) ->
    gen_statem:call(?MODULE, {?FUNCTION_NAME, SpawnId, IOData}).


%% @doc return a list of all spawned processes with their OS pid.
-spec all() -> [{grimsby:spawn_id(), pos_integer()}].
all() ->
    gen_statem:call(?MODULE, ?FUNCTION_NAME).


%% @private
init([]) ->
    process_flag(trap_exit, true),
    {ok,
     unready,
     #{requests => gen_statem:reqids_new(),
       inflight => #{}}}.


%% @private
callback_mode() ->
    handle_event_function.


%% @private
handle_event({call, _}, _, unready, _) ->
    {keep_state_and_data, [nei(open_port), postpone]};

handle_event({call, From}, all = Command, _, #{inflight := Inflight} = Data) ->
    InteractionId = make_ref(),
    {keep_state,
     Data#{inflight := Inflight#{InteractionId => From}},
     nei({port_command, {Command, InteractionId}})};

handle_event({call, {To, _} = From},
             {run, Arg},
             _,
             #{inflight := Inflight} = Data) ->
    SpawnId = make_ref(),
    InteractionId = make_ref(),
    SendTo = maps:get(send_to, Arg, To),
    {keep_state,
     Data#{inflight := Inflight#{InteractionId => #{from => From,
                                                    send_to => SendTo,
                                                    spawn_id => SpawnId}}},
     nei({port_command, {spawn, SpawnId, InteractionId, Arg}})};

handle_event({call, From},
             {Command, SpawnId},
             _,
             #{inflight := Inflight} = Data) when Command == wait_for_exit;
                                                  Command == kill ->
    InteractionId = make_ref(),
    {keep_state,
     Data#{inflight := Inflight#{InteractionId => From}},
     nei({port_command, {Command, SpawnId, InteractionId}})};

handle_event({call, From},
             {close, SpawnId, Stream},
             _,
             #{inflight := Inflight} = Data) ->
    InteractionId = make_ref(),
    {keep_state,
     Data#{inflight := Inflight#{InteractionId => From}},
     nei({port_command, {close, SpawnId, InteractionId, Stream}})};

handle_event({call, From},
             {send, SpawnId, IOData},
             _,
             #{inflight := Inflight} = Data) ->
    InteractionId = make_ref(),
    {keep_state,
     Data#{inflight := Inflight#{InteractionId => From}},
     nei({port_command,
          {send, SpawnId, InteractionId, iolist_to_binary(IOData)}})};

handle_event(internal,
             {port_command, Command},
             _,
             #{port := Port}) ->
    erlang:port_command(Port, term_to_binary(Command)),
    keep_state_and_data;

handle_event(info,
             {Port, {data, Encoded}},
             _,
             #{port := Port}) ->
    {keep_state_and_data,
     nei({data, binary_to_term(Encoded)})};

handle_event(internal,
             {data, {reply, Id, Result}},
             _,
             #{inflight := Inflight} = Data)
  when is_map_key(Id, Inflight) ->
    case Inflight of
        #{Id := #{from := From, send_to := SendTo, spawn_id := SpawnId}} ->
            {keep_state,
             Data#{inflight := maps:put(
                                 SpawnId,
                                 SendTo,
                                 maps:remove(Id, Inflight))},
             {reply, From, {Result, SpawnId}}};

        #{Id := From} ->
            {keep_state,
             Data#{inflight := maps:remove(Id, Inflight)},
             {reply, From, Result}}
    end;

handle_event(internal, {data, Request},
             _,
             #{inflight := Inflight, requests := Requests} = Data)
  when is_tuple(Request),
       is_map_key(element(2, Request), Inflight) ->
    #{element(2, Request) := SendTo} = Inflight,
    {keep_state,
     Data#{requests := gen_statem:send_request(
                         SendTo,
                         {?MODULE, Request},
                         #{module => ?MODULE, spawn_id => element(2, Request)},
                         Requests)}};

handle_event(internal, open_port, unready, Data) ->
    {next_state,
     ready,
     Data#{port => erlang:open_port(
                     {spawn_executable, grimsby_config:executable(spawn)},
                     [{packet, 4},
                      in,
                      out,
                      exit_status,
                      eof,
                      use_stdio,
                      binary])}};

handle_event(info, Msg, _, #{requests := Existing} = Data) ->
    case gen_statem:check_response(Msg, Existing, true) of
        {{reply, ok}, #{module := ?MODULE}, Updated} ->
            {keep_state, Data#{requests := Updated}};

        {{error, {Reason, _}}, _, UpdatedRequests} ->
            {stop, Reason, Data#{requests := UpdatedRequests}}
    end.
