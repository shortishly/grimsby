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


-module(grimsby_SUITE).


-compile(export_all).
-compile(nowarn_export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


all() ->
    common:all(?MODULE).


init_per_suite(Config) ->
    _ = application:load(grimsby),
    %%    application:set_env(grimsby, command_trace, true),
    {ok, _} = grimsby:start(),
    Config.


end_per_suite(_Config) ->
    ok = application:stop(grimsby).


invalid_current_dir_test(_Config) ->
    {ok, Spawn} = grimsby_command_sup:start_child(
                    #{executable => find_executable("cat"),
                      cd => "/this_dir_does_not_exist"}),
    [] = grimsby_port:all(),
    ?assertMatch(
       {error, _},
       grimsby_command:send(Spawn, <<"hello \n">>)).


invalid_command_test(_Config) ->
    {ok, Spawn} = grimsby_command_sup:start_child(
                    #{executable => "/this_command_does_not_exist"}),
    [] = grimsby_port:all(),
    ?assertMatch(
       {error, _},
       grimsby_command:send(Spawn, <<"hello \n">>)).


line_buffered_cat_test(_Config) ->
    {ok, Spawn} = grimsby_command_sup:start_child(
                    #{executable => find_executable("cat")}),
    [{_, _}] = grimsby_port:all(),

    grimsby_command:send(Spawn, <<"hello \n">>),
    wait_for(
      #{eof => [],
        stderr => [],
        stdout => [[], <<"hello \n">>]},
      fun
          () ->
              grimsby_command:info(Spawn)
      end),

    grimsby_command:send(Spawn, "world!\n"),
    wait_for(
      #{eof => [],
        stderr => [],
        stdout => [[[], <<"hello \n">>], <<"world!\n">>]},
      fun
          () ->
              grimsby_command:info(Spawn)
      end),

    grimsby_command:close(Spawn),
    wait_for(
      #{stderr => [],
        eof => [stderr, stdin, stdout],
        stdout => [[[], <<"hello \n">>], <<"world!\n">>]},
      fun
          () ->
              grimsby_command:info(Spawn)
      end),

    {ok, 0} = grimsby_command:wait_for_exit(Spawn),
    wait_for(
      #{exit => 0,
        eof => [stderr, stdin, stdout],
        stderr => [],
        stdout => [[[], <<"hello \n">>], <<"world!\n">>]},
      fun
          () ->
              grimsby_command:info(Spawn)
      end),
    [] = grimsby_port:all().


env_test(_Config) ->
    K = alpha(5),
    V = alpha(5),
    {ok, Spawn} = grimsby_command_sup:start_child(
                    #{executable => find_executable("env"),
                      envs => #{K => V}}),
    Expected = iolist_to_binary([K, "=", V, "\n"]),
    wait_for(
      true,
      fun
          () ->
              #{stdout := IOData} = grimsby_command:info(Spawn),
              binary:match(iolist_to_binary(IOData), Expected) /= nomatch
      end),
    {ok, 0} = grimsby_command:wait_for_exit(Spawn).


echo_test(_Config) ->
    {ok, Spawn} = grimsby_command_sup:start_child(
                    #{executable => find_executable("echo"),
                      args => ["abc"]}),
    ok = grimsby_command:send(Spawn, <<"hello ">>),

    wait_for(
      #{eof => [stderr, stdout],
        stderr => [],
        stdout => [[], <<"abc\n">>]},
      fun
          () ->
              grimsby_command:info(Spawn)
      end),

    %% may be in error state at this point...
    _ = grimsby_command:send(Spawn, <<"cruel ">>),

    wait_for(
      {error,stdin},
      fun
          () ->
              element(1, sys:get_state(Spawn))
      end),

    {error, stdin} = grimsby_command:send(Spawn, "world!").


cat_test(_Config) ->
    {ok, Spawn} = grimsby_command_sup:start_child(
                    #{executable => find_executable("cat")}),
    ok = grimsby_command:send(Spawn, <<"hello ">>),
    ok = grimsby_command:send(Spawn, "world!"),
    wait_for(
      #{eof => [], stderr => [], stdout => []},
      fun
          () ->
                    grimsby_command:info(Spawn)
      end),
    grimsby_command:close(Spawn),
    wait_for(
      #{stderr => [],
        eof => [stderr, stdin, stdout],
        stdout => [[],<<"hello world!">>]},
      fun
          () ->
                    grimsby_command:info(Spawn)
            end),
    grimsby_command:wait_for_exit(Spawn),
    wait_for(
      #{exit => 0,
        eof => [stderr, stdin, stdout],
        stderr => [],
        stdout => [[],<<"hello world!">>]},
      fun
          () ->
                    grimsby_command:info(Spawn)
            end).


kill_test(_Config) ->
    {ok, Spawn} = grimsby_command_sup:start_child(
                    #{executable => find_executable("cat")}),
    {ok, signal} = grimsby_command:kill(Spawn),
    wait_for(
      #{exit => signal,
        eof => [],
        stderr => [],
        stdout => []},
      fun
          () ->
                    grimsby_command:info(Spawn)
            end).


wait_for(Expected, Check) ->
    ct:log("expected: ~p, check: ~p~n", [Expected, Check]),
    ?FUNCTION_NAME(Expected, Check, 5).

wait_for(Expected, Check, 0 = N) ->
    case Check() of
        Expected ->
            Expected;

        Unexpected ->
            ct:log("expected: ~p~ncheck: ~p~nn: ~p~nactual: ~p~n",
                   [Expected, Check, N, Unexpected]),
            Expected = Unexpected
    end;

wait_for(Expected, Check, N) ->
    case Check() of
        Expected ->
            ct:log("matched: ~p,~ncheck: ~p,~nn: ~p~n",
                   [Expected, Check, N]),
            Expected;

        Unexpected ->
            ct:log("expected: ~p,~ncheck: ~p,~nn: ~p,~nactual: ~p~n",
                   [Expected, Check, N, Unexpected]),
            timer:sleep(timer:seconds(1)),
            ?FUNCTION_NAME(Expected, Check, N - 1)
    end.


find_executable(Name) ->
    case os:find_executable(Name) of
        false ->
            error(badarg, [Name]);
        Filename ->
            Filename
    end.


alpha(N) ->
    pick(N, lists:seq($a, $z)).


pick(N, Pool) ->
    ?FUNCTION_NAME(N, Pool, []).


pick(0, _, A) ->
    A;

pick(N, Pool, A) ->
    ?FUNCTION_NAME(N - 1,
                   Pool,
                   [lists:nth(rand:uniform(length(Pool)), Pool) | A]).
