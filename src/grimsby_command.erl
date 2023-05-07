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


-module(grimsby_command).


-behaviour(gen_statem).
-export([callback_mode/0]).
-export([close/1]).
-export([handle_event/4]).
-export([info/1]).
-export([init/1]).
-export([kill/1]).
-export([send/2]).
-export([start/1]).
-export([start_link/1]).
-export([stop/1]).
-export([wait_for_exit/1]).
-export_type([info/0]).
-export_type([start_arg/0]).
-import(grimsby_statem, [nei/1]).
-include_lib("kernel/include/logger.hrl").


-type start_arg() :: #{executable := string(),
                       args => [string()],
                       envs => #{string() => string()},
                       arg0 => string(),
                       cd => file:filename()}.


-spec start(start_arg()) -> gen_statem:start_ret().

start(Arg) ->
    gen_statem:start(?MODULE,
                     [Arg],
                     envy_gen:options(?MODULE)).


-spec start_link(start_arg()) -> gen_statem:start_ret().

start_link(Arg) ->
    gen_statem:start_link(?MODULE,
                          [Arg],
                          envy_gen:options(?MODULE)).


-spec close(pid()) -> ok.
close(Command) ->
    gen_statem:call(Command, {?FUNCTION_NAME, stdin}).


-spec send(pid(), iodata()) -> ok.
send(Command, Arg) ->
    gen_statem:call(Command, {?FUNCTION_NAME, Arg}).


-spec wait_for_exit(pid()) -> {ok, integer() | signal}.
wait_for_exit(Command) ->
    gen_statem:call(Command, ?FUNCTION_NAME).


-spec kill(pid()) -> {ok, integer() | signal}.
kill(Command) ->
    gen_statem:call(Command, ?FUNCTION_NAME).


-type info() :: #{exit => integer() | signal,
                  eof := [grimsby:stream()],
                  stderr := iodata(),
                  stderr := iodata()}.

-spec info(pid()) -> info().
info(Command) ->
    gen_statem:call(Command, ?FUNCTION_NAME).


-spec stop(pid()) -> ok.
stop(Command) ->
    gen_statem:stop(Command).


%% @private
init([Arg]) ->
    {ok,
     unready,
     #{requests => gen_statem:send_request(
                     grimsby_port,
                     {run, maps:with([executable, args, envs, arg0, cd], Arg)},
                     #{request => run},
                     gen_statem:reqids_new()),
       eof => ordsets:new(),
       stdout => [],
       stderr => []}}.


%% @private
callback_mode() ->
    handle_event_function.


%% @private
handle_event({call, PortManager},
             {grimsby_port, {error, SpawnId, Stream}},
             ready,
             #{spawn := SpawnId} = Data) ->
    ?LOG_DEBUG(#{from => PortManager}),
    {next_state, {error, Stream}, Data, {reply, PortManager, ok}};

handle_event({call, PortManager},
             {grimsby_port, {eof, SpawnId, stdin = Stream}},
             {close, Stream, Client},
             #{eof := Streams, spawn := SpawnId} = Data) ->
    ?LOG_DEBUG(#{eof => Streams, spawn => SpawnId}),
    {next_state,
     ready,
     Data#{eof := ordsets:add_element(Stream, Streams)},
     [{reply, PortManager, ok},
      {reply, Client, ok}]};

handle_event({call, PortManager},
             {grimsby_port, {eof, SpawnId, Stream}},
             _,
             #{eof := Streams, spawn := SpawnId} = Data)
 when Stream == stdin;
      Stream == stdout;
      Stream == stderr ->
    ?LOG_DEBUG(#{eof => Streams, spawn => SpawnId}),
    {keep_state,
     Data#{eof := ordsets:add_element(Stream, Streams)},
     {reply, PortManager, ok}};

handle_event({call, PortManager},
             {grimsby_port, {exit, SpawnId, StatusCodeOrSignal}},
             {State, Client},
             #{spawn := SpawnId} = Data)
  when State == wait_for_exit;
       State == kill ->
    ?LOG_DEBUG(#{spawn => SpawnId,
                 state => State,
                 client => Client,
                 status_code_or_signal => StatusCodeOrSignal}),
    {next_state,
     ready,
     Data#{exit => StatusCodeOrSignal},
     [{reply, PortManager, ok},
      {reply, Client, {ok, StatusCodeOrSignal}}]};

handle_event({call, PortManager},
             {grimsby_port, {exit, SpawnId, StatusCodeOrSignal}},
             _,
             #{spawn := SpawnId} = Data) ->
    ?LOG_DEBUG(#{spawn => SpawnId}),
    {keep_state, Data#{exit => StatusCodeOrSignal}, {reply, PortManager, ok}};

handle_event({call, PortManager},
             {grimsby_port, {Stream, SpawnId, New}},
             _,
             #{spawn := SpawnId} = Data)
  when Stream == stdout; Stream == stderr ->
    ?LOG_DEBUG(#{from => PortManager, stream => Stream, spawn_id => SpawnId, new => New}),
    #{Stream := Existing} = Data,
    {keep_state, Data#{Stream := [Existing, New]}, {reply, PortManager, ok}};

handle_event({call, From}, info, _, Data) ->
    {keep_state_and_data,
     {reply, From, maps:with([exit, eof, stderr, stdout], Data)}};


handle_event({call, From}, _, {error, _} = Error, _) ->
    %% any client request in the error state immediately returns the
    %% error
    ?LOG_DEBUG(#{from => From}),
    {keep_state_and_data, {reply, From, Error}};

handle_event({call, _}, _, State, _) when State /= ready ->
    %% any client request when not in the ready state will postpone
    %% until we are
    ?LOG_DEBUG(#{state => State}),
    {keep_state_and_data, postpone};

handle_event({call, From},
             Request,
             _,
             #{exit := StatusOrSignal})
  when Request == wait_for_exit; Request == kill ->
    ?LOG_DEBUG(#{from => From}),
    {keep_state_and_data, {reply, From, {ok, StatusOrSignal}}};

handle_event({call, From},
             Request,
             ready,
             #{spawn := Spawn} = Data)
  when Request == wait_for_exit;
       Request == kill ->
    ?LOG_DEBUG(#{from => From, request => Request, spawn => Spawn}),
    {next_state,
     {Request, From},
     Data,
     nei({send_request, {Request, Spawn}})};

handle_event({call, From},
             {close = Request, stdin = Stream},
             State,
             #{spawn := SpawnId, eof := Streams} = Data) ->
    ?LOG_DEBUG(#{from => From}),
    case ordsets:is_element(Stream, Streams) of
        true ->
            {keep_state_and_data, {reply, From, ok}};

        false when State == ready ->
            {next_state,
             {close, Stream, From},
             Data,
             nei({send_request, {Request, SpawnId, Stream}})}
    end;

handle_event({call, From},
             {send = Request, Input},
             _,
             #{spawn := SpawnId}) ->
    ?LOG_DEBUG(#{from => From}),
    {keep_state_and_data,
     [{reply, From, ok},
      nei({send_request, {Request, SpawnId, Input}})]};

handle_event(internal,
             {send_request = Action, Request},
             _,
             #{requests := Requests} = Data) ->
    ?LOG_DEBUG(#{action => Action, request => Request}),
    {keep_state,
     Data#{requests := gen_statem:send_request(
                         grimsby_port,
                         Request,
                         #{request => Request},
                         Requests)}};

handle_event(info, Msg, unready, #{requests := Existing} = Data) ->
    ?LOG_DEBUG(#{msg => Msg}),
    case gen_statem:check_response(Msg, Existing, true) of
        {{reply, {ok, SpawnId}}, #{request := run}, Updated} ->
            {next_state, ready, Data#{requests := Updated, spawn => SpawnId}};

        {{reply, {{error, _} = Error, _}}, #{request := run}, Updated} ->
            {next_state, Error, Data#{requests := Updated}}
    end;

handle_event(info, Msg, _, #{requests := Existing} = Data) ->
    ?LOG_DEBUG(#{msg => Msg}),
    case gen_statem:check_response(Msg, Existing, true) of
        {{reply, ok}, _, Updated} ->
            {keep_state, Data#{requests := Updated}};

        {{reply, {error, Reason}}, _, Updated} ->
            {stop, Reason, Data#{requests := Updated}};

        {{error, {Reason, _}}, _, Updated} ->
            {stop, Reason, Data#{requests := Updated}}
    end.
