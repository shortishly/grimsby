-module(grimsby_statem).

-export([nei/1]).

nei(Event) ->
    {next_event, internal, Event}.
