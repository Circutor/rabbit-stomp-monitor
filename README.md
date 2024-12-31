# RabbitMQ STOMP Monitor Plugin

## Build

### Prerequisites
- [Erlang/OTP 26](https://www.erlang.org/doc/system/install.html)
- [Elixir 1.16](https://elixir-lang.org/install.html) (required for `rabbitmq-server` CLI tools)
- Make (>= 4.0) (via `build-essential` on Debian-based systems or `brew install make` on macOS)

### Steps
#### Building Locally
1. Clone the repository.
2. Run the following command in the project root directory:
    ```bash
    DIST_AS_EZS=yes make dist
    ```
3. The plugin archive will be created in the `./plugins` directory with the name `rabbitmq_stomp_monitor-<version>.ez`.

#### Building Using GitHub Actions Dockerfile
1. Ensure Docker is installed and running on your machine.
2. Run the following command in the project root directory:
    ```bash
    docker build \
        -t build-plugin \
        -f .github/actions/build-plugin/Dockerfile \
        .github/actions/build-plugin/
    ```
3. Execute the build process using the Docker image:
    ```bash
    docker run --rm -v $(pwd):/github/workspace build-plugin
    ```
4. The plugin archive will be created in the `./plugins` directory with the name `rabbitmq_stomp_monitor-<version>.ez`.

### Running Tests
You can run tests to validate the plugin functionality:
```bash
make tests
```

## Deployment

### Using Kubernetes Operator

It is highly recommended to use a custom image with the plugin pre-installed, rather than installing it at runtime. This ensures the plugin is available immediately after the RabbitMQ cluster starts, which is critical for high availability and production reliability (eliminating network latency and potential plugin installation issues).

1. Build the plugin as described in the [Build](#build) section.
2. Create a custom Docker image with the plugin in the `plugins` directory of the RabbitMQ base image:
    ```Dockerfile
    FROM docker.io/bitnami/rabbitmq:<version> # Or any other RabbitMQ image you'd like to extend

    COPY plugins/rabbitmq_stomp_monitor-<version>.ez /opt/bitnami/rabbitmq/plugins/
    ```
3. Use the custom image in your Kubernetes deployment configuration with the plugin enabled:
   ```yaml
   apiVersion: rabbitmq.com/v1beta1
   kind: RabbitmqCluster
   metadata:
     name: rabbitmq
   spec:
     image: <IMAGE>                # The custom RabbitMQ image to use
     rabbitmq:
       additionalPlugins:
       - rabbitmq_stomp_monitor
   ```
   Refer to the [documentation](https://www.rabbitmq.com/kubernetes/operator/using-operator#images) for setting up access to private registries via `imagePullSecrets` if necessary.

## Overall Functionality
This plugin:
- Tracks STOMP protocol connections to log user connection and disconnection events.
- Intercepts and logs messages sent via the STOMP protocol, including details like the exchange, routing key, and payload.
- Adds the following headers:
  - `myc-timestamp` - The timestamp of the message on the broker (in milliseconds).
  - `myc-msg-id` - A unique identifier for the message (UUID).

It consists of two modules:

1. `rabbitmq_stomp_monitor_event.erl`
2. `rabbitmq_stomp_monitor_interceptor.erl`

### `rabbitmq_stomp_monitor_event`
This module:
- Listens for `connection_created` events in RabbitMQ.
- Tracks STOMP protocol-based connections for logging purposes.
- Logs events when:
   - A STOMP user connects.
   - A STOMP user disconnects.

**Key Details**:
- **Callbacks**:
   - `handle_event/2`: Processes the `connection_created` event and begins tracking the STOMP connection.
   - `handle_info/2`: Handles process termination notifications (via monitors) and logs the disconnection.
- **State Tracking**: Uses a map to store information about monitored STOMP connections.

**Core Functions**:
- `maybe_start_tracking/2`: Monitors the STOMP connection process and logs the connection.
- `log_connected/1` and `log_disconnected/1`: Log connection and disconnection events, including user and vhost information.
- `log_activity/2`: Formats and logs user connection activities.

### `rabbitmq_stomp_monitor_interceptor`
This module:
- Intercepts `basic.publish` messages and logs their details when they are associated with the STOMP protocol.
- Targets messages published through RabbitMQ's channel interceptors.

**Key Details**:
- **Callbacks**:
   - `intercept/3`: Intercepts `basic.publish` frames and logs message details.
   - `applies_to/0`: Specifies that the interceptor applies to `basic.publish`.
- **Message Logging**:
   - Checks if the channel uses the STOMP protocol.
   - Logs the exchange, routing key, and payload of the message.

**Core Functions**:
- `maybe_log_message/3`: Logs messages sent via the STOMP protocol.
- `connection_protocol/1`: Resolves the protocol for a given RabbitMQ channel.
- `log_message/2`: Logs the payload and metadata of intercepted messages.
- `add_headers/1`: Adds timestamp and message ID headers to intercepted messages for traceability.

## Development

Refer to the official RabbitMQ [plugin development guide](https://www.rabbitmq.com/plugin-development) to understand how the plugin system works.
