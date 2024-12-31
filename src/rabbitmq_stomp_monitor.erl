-module(rabbitmq_stomp_monitor).

-behaviour(application).

-export([start/2, stop/1]).

start(normal, []) ->
    rabbitmq_stomp_monitor_sup:start_link().

stop(_State) ->
    ok.
