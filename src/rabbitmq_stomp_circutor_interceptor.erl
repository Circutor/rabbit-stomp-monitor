-module(rabbitmq_stomp_circutor_interceptor).

-include_lib("rabbit_common/include/rabbit_framing.hrl").
-include_lib("rabbit_common/include/rabbit.hrl").

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
    case connection_protocol(self()) of
        {Protocol, _} when Protocol == 'STOMP';
                           Protocol == 'Web STOMP' ->
            intercept_stomp(Method, Content);
        _ ->
            {Method, Content}
    end;

intercept(Method, Content, _Context) ->
    {Method, Content}.

applies_to() ->
    ['basic.publish'].

%%----------------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------------
intercept_stomp(Method, Content) ->
    DecodedContent = rabbit_binary_parser:ensure_content_decoded(Content),
    log_message(Method, DecodedContent),
    Content2 = add_headers(Content),
    {Method, Content2}.

connection_protocol(ChPid) ->
    case get({?MODULE, connection_protocol}) of
        undefined ->
            Protocol = get_connection_protocol(ChPid),
            put({?MODULE, connection_protocol}, Protocol),
            Protocol;

        Protocol ->
            Protocol
    end.

get_connection_protocol(ChPid) ->
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

log_message(#'basic.publish'{exchange = X, routing_key = RK}, Content) ->
    {_Props, Payload} = rabbit_basic_common:from_content(Content),
    rabbit_log:info("Received STOMP message for ~s exchange and ~s routing key: ~tp",
                    [X, RK, Payload]).

add_headers(#content{properties = #'P_basic'{headers = Headers0} = Props0} = Content) ->
    TSHeader = timestamp_header(),
    IDHeader = id_header(),
    Headers1 = add_headers(Headers0, [TSHeader, IDHeader]),
    Props1 = Props0#'P_basic'{headers = Headers1},
    Content#content{properties = Props1, properties_bin = none}.

timestamp_header() ->
    {<<"myc-timestamp">>, long, os:system_time(milli_seconds)}.

id_header() ->
    {<<"myc-msg-id">>, longstr, rabbit_guid:to_string(rabbit_guid:gen())}.

add_headers(Headers, []) ->
    Headers;
add_headers(undefined, [Header | Tail]) ->
    add_headers([Header], Tail);
add_headers(Headers, [Header | Tail]) ->
    add_headers(lists:keystore(element(1, Header), 1, Headers, Header), Tail).


