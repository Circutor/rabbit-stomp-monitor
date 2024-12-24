-module(rabbitmq_stomp_circutor_interceptor).

-include_lib("rabbit_common/include/rabbit_framing.hrl").

-behaviour(rabbit_channel_interceptor).

-export([description/0, intercept/3, applies_to/0, init/1]).

-import(rabbit_misc, [pget/2]).

-rabbit_boot_step({?MODULE,
                   [{description, "STOMP Circutor message tracer"},
                    {mfa, {rabbit_registry, register,
                           [channel_interceptor,
                            <<"STOMP Circutor message tracer">>, ?MODULE]}},
                    {cleanup, {rabbit_registry, unregister,
                               [channel_interceptor,
                                <<"STOMP Circutor message tracer">>]}},
                    {requires, rabbit_registry},
                    {enables, recovery}]}).

%%----------------------------------------------------------------------------
%% Behaviour callbacks
%%----------------------------------------------------------------------------

init(_Ch) ->
    ok.


description() ->
    [{description, <<"STOMP Circutor message tracer">>}].

intercept(#'basic.publish'{} = Method, Content, _Context) ->
    maybe_log_message(Method, Content),
    {Method, Content};

intercept(Method, Content, _Context) ->
    {Method, Content}.

applies_to() ->
    ['basic.publish'].

%%----------------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------------
connection_protocol(ChPid) ->
    case ets:lookup(channel_created, ChPid) of
        [] ->
            rabbit_log:error("Unable to find channel info for ~p", [ChPid]),
            undefined;

        [{_pid, Infos}] ->
            case pget(connection, Infos) of
                undefined ->
                    rabbit_log:error(
                      "Unable to find connection info for the channel ~p (~tp)",
                      [self(), Infos]
                     ),
                    undefined;

                ConnectionPid ->
                    case ets:lookup(connection_created, ConnectionPid) of
                        [] ->
                            rabbit_log:error(
                              "Unable to find connection info for the connection ~p",
                                [ConnectionPid]
                            ),
                            undefined;

                        [{_Pid, ConnectionInfos}] ->
                            pget(protocol, ConnectionInfos)
                    end
            end
    end.

maybe_log_message(Method, Content) ->
    maybe_log_message(get({?MODULE, connection_protocol}), Method, Content).

maybe_log_message({Protocol, _}, Method, Content) when Protocol == 'STOMP';
                                                       Protocol == 'Web STOMP' ->
    log_message(Method, Content);
maybe_log_message(undefined, Method, Content) ->
    Protocol = connection_protocol(self()),
    put({?MODULE, connection_protocol}, Protocol),
    maybe_log_message(Protocol, Method, Content);
maybe_log_message(_Other, _Method, _Content) ->
    ok.

log_message(#'basic.publish'{exchange = X, routing_key = RK}, Content) ->
    {_Props, Payload} = rabbit_basic_common:from_content(Content),
    rabbit_log:info("Received STOMP message for ~s exchange and ~s routing key: ~tp",
                    [X, RK, Payload]).
