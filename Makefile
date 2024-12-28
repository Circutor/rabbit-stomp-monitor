PROJECT = rabbitmq_stomp_circutor
PROJECT_DESCRIPTION = RabbitMQ STOMP Circutor plugin

RABBITMQ_VERSION ?= v3.12.x
current_rmq_ref = $(RABBITMQ_VERSION)

dep_amqp_client   = git_rmq-subfolder rabbitmq-erlang-client $(RABBITMQ_VERSION)
dep_rabbit_common = git_rmq-subfolder rabbitmq-common $(RABBITMQ_VERSION)
dep_rabbit        = git_rmq-subfolder rabbitmq-server $(RABBITMQ_VERSION)

DEPS = rabbit_common rabbit
TEST_DEPS = rabbitmq_ct_helpers rabbitmq_ct_client_helpers rabbitmq_stomp amqp_client

DEP_EARLY_PLUGINS = rabbit_common/mk/rabbitmq-early-plugin.mk
DEP_PLUGINS = rabbit_common/mk/rabbitmq-plugin.mk

# FIXME: Use erlang.mk patched for RabbitMQ, while waiting for PRs to be
# reviewed and merged.

ERLANG_MK_REPO = https://github.com/rabbitmq/erlang.mk.git
ERLANG_MK_COMMIT = rabbitmq-tmp

include rabbitmq-components.mk
include erlang.mk
