-module(rabbitmq_stomp_circutor).

-behaviour(application).

-export([start/2, stop/1]).

start(normal, []) ->
    rabbitmq_stomp_circutor_sup:start_link().

stop(_State) ->
    ok.
