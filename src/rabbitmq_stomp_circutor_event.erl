-module(rabbitmq_stomp_circutor_event).
-feature(maybe_expr, enable).

-include_lib("rabbit_common/include/rabbit.hrl").

-behaviour(gen_event).

-export([init/1, handle_call/2, handle_event/2, handle_info/2,
         terminate/2, code_change/3]).

-import(rabbit_misc, [pget/2]).

-rabbit_boot_step({?MODULE,
                   [{description, "RabbitMQ STOMP Circutor connection tracker"},
                    {mfa,         {gen_event, add_handler,
                                   [rabbit_event, ?MODULE, []]}},
                    {cleanup,     {gen_event, delete_handler,
                                   [rabbit_event, ?MODULE, []]}},
                    {requires,    rabbit_event},
                    {enables,     recovery}]}).

%%----------------------------------------------------------------------------
%% Behaviour callbacks
%%----------------------------------------------------------------------------
init([]) ->
    {ok, #{}}.

handle_call(_Request, State) ->
    {ok, not_understood, State}.

handle_event(Event = #event{type = connection_created}, State) ->
    rabbit_log:debug("Received connection_created event: ~tp", [Event]),
    State2 = maybe_start_tracking(Event, State),
    {ok, State2};

handle_event(_Event, State) ->
    {ok, State}.

handle_info({'DOWN', Ref, process, _Pid, _Reason}, State) ->
    case maps:find(Ref, State) of
        {ok, Props} ->
            log_disconnected(Props),
            {ok, maps:remove(Ref, State)};

        error ->
            rabbit_log:warning("Unable to find connection info for ~p", [Ref]),
            {ok, State}
    end;

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Arg, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------------
%% Internal functions
%%----------------------------------------------------------------------------
maybe_start_tracking(Event, State) ->
    maybe
        ok ?= verify_stomp_protocol(Event),
        {pid, CPid} ?= lists:keyfind(pid, 1, Event#event.props),
        log_connected(Event#event.props),
        Ref = erlang:monitor(process, CPid),
        maps:put(Ref, Event#event.props, State)
    else
        {error, Protocol} ->
            rabbit_log:debug("Ignoring connection tracking for a protocol ~p", [Protocol]),
            State;

        undefined ->
            rabbit_log:warning("Unable start tracking for the connection ~tp", [Event#event.props]),
            State
    end.

verify_stomp_protocol(Event) ->
    case lists:keyfind(protocol, 1, Event#event.props) of
        {protocol, {Protocol, _}} when Protocol == 'STOMP';
                                       Protocol == 'Web STOMP' ->
            ok;

        {protocol, Other} ->
            {error, Other};

        undefined ->
            {error, undefined}
    end.

log_connected(Props) ->
    log_activity("User connected via STOMP protocol", Props).

log_disconnected(Props) ->
    log_activity("User disconnected from STOMP", Props).

log_activity(Prefix, Props) ->
    PropKeys = [user, vhost, name, client_properties],
    rabbit_log:info("~s: ~p",
                    [Prefix, [{Key, pget(Key, Props)} || Key <- PropKeys]]).
