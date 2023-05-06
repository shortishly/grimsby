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


-module(grimsby_command_sup).


-behaviour(supervisor).
-export([init/1]).
-export([start_child/1]).
-export([start_link/0]).
-import(grimsby_sup, [worker/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%% @private
init([]) ->
    {ok, {#{}, []}}.


-spec start_child(grimsby_command:start_arg()) -> supervisor:startchild_ret().

start_child(Arg) ->
    supervisor:start_child(
      ?MODULE,
      worker(#{m => grimsby_command,
               restart => temporary,
               id => maps:get(id, Arg, make_ref()),
               args => [maps:without([id], Arg)]})).
