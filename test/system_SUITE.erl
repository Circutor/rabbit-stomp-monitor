-module(system_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

-define(QUEUE, <<"TestQueue">>).
-define(DESTINATION, "/amq/queue/TestQueue").
-define(STOMP_VERSION, "1.2").

all() ->
    [{group, main}].

groups() ->
    [
     {main, [sequence],
      [
       headers_test,
       forwarding_test
      ]}
    ].

init_per_suite(Config) ->
    rabbit_ct_helpers:log_environment(),
    Config1 =
        rabbit_ct_helpers:set_config(Config,
                                     [{rmq_nodename_suffix, ?MODULE}]),
    rabbit_ct_helpers:run_setup_steps(Config1,
      rabbit_ct_broker_helpers:setup_steps() ++
      rabbit_ct_client_helpers:setup_steps()).

end_per_suite(Config) ->
    rabbit_ct_helpers:run_teardown_steps(Config,
      rabbit_ct_client_helpers:teardown_steps() ++
      rabbit_ct_broker_helpers:teardown_steps()).

init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

init_per_testcase(TestCase, Config) ->
    Config1 = rabbit_ct_helpers:testcase_started(Config, TestCase),
    StompPort = rabbit_ct_broker_helpers:get_node_config(Config, 0, tcp_port_stomp),
    {ok, Connection} = amqp_connection:start(#amqp_params_direct{
        node = rabbit_ct_broker_helpers:get_node_config(Config1, 0, nodename)
    }),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    {ok, Client} = rabbit_stomp_client:connect(?STOMP_VERSION, StompPort),
    rabbit_ct_helpers:set_config(Config1, [
        {amqp_connection, Connection},
        {amqp_channel, Channel},
        {stomp_client, Client}
    ]).

end_per_testcase(TestCase, Config) ->
    Config1 = rabbit_ct_helpers:testcase_finished(Config, TestCase),
    Connection = ?config(amqp_connection, Config1),
    Channel = ?config(amqp_channel, Config1),
    Client = ?config(stomp_client, Config1),
    purge_queues(Channel),
    rabbit_stomp_client:disconnect(Client),
    amqp_channel:close(Channel),
    amqp_connection:close(Connection).

%% -------------------------------------------------------------------
%% Testcases
%% -------------------------------------------------------------------
headers_test(Config) ->
    Channel = ?config(amqp_channel, Config),
    Client = ?config(stomp_client, Config),
    #'queue.declare_ok'{} =
        amqp_channel:call(Channel, #'queue.declare'{queue       = ?QUEUE,
                                                    auto_delete = true}),
    #'basic.consume_ok'{} =
        amqp_channel:subscribe(Channel, #'basic.consume'{queue  = ?QUEUE,
                                                         no_ack = true}, self()),

    NowMs = os:system_time(milli_seconds),
    %% send from stomp
    rabbit_stomp_client:send(
      Client, "SEND", [{"destination", ?DESTINATION}], ["hello"]),


    {ok, Msg} =
        receive
            {#'basic.deliver'{},
             M = #'amqp_msg'{payload = <<"hello">>}} ->
                {ok, M}
        after 1000 ->
                {error, timeout}
        end,

    {_, long, Timestamp} = get_single_header(<<"myc-timestamp">>, Msg),
    ?assertNotEqual(Timestamp, false),
    ?assert(Timestamp >= NowMs),

    {_, longstr, UUID} = get_single_header(<<"myc-msg-id">>, Msg),
    ?assertNotEqual(UUID, false),

    Pattern = "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$",
    ?assertMatch({match, _}, re:run(UUID, Pattern, [unicode])),
    ok.


forwarding_test(Config) ->
    Channel = ?config(amqp_channel, Config),
    Client = ?config(stomp_client, Config),

    #'basic.consume_ok'{} =
        amqp_channel:subscribe(Channel, #'basic.consume'{queue  = <<"stomp-messages">>,
                                                         no_ack = true}, self()),

    NowMs = os:system_time(milli_seconds),
    rabbit_stomp_client:send(
      Client, "SEND", [{"destination", ?DESTINATION}], ["hello"]),

    {ok, Msg} =
        receive
            {#'basic.deliver'{},
             M = #'amqp_msg'{payload = <<"hello">>}} ->
                {ok, M}
        after 1000 ->
                {error, timeout}
        end,

    {_, long, Timestamp} = get_single_header(<<"myc-timestamp">>, Msg),
    ?assertNotEqual(Timestamp, false),
    ?assert(Timestamp >= NowMs),

    {_, longstr, UUID} = get_single_header(<<"myc-msg-id">>, Msg),
    ?assertNotEqual(UUID, false),

    Pattern = "^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$",
    ?assertMatch({match, _}, re:run(UUID, Pattern, [unicode])),
    ok.

get_single_header(Target,
    #amqp_msg{props = #'P_basic'{headers = Headers}}) ->
    lists:keyfind(Target, 1, Headers).

purge_queues(Channel) ->
    [begin
         #'queue.purge_ok'{} =
             amqp_channel:call(Channel, #'queue.purge'{queue = Q})
     end
     || Q <- [<<"stomp-messages">>]].
