FROM elixir:1.16-otp-26

RUN apt-get update && \
    apt-get install -y --no-install-recommends rsync zip && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /github/workspace

COPY entrypoint.sh /usr/local/bin/entrypoint.sh

ENTRYPOINT ["/usr/local/bin/entrypoint.sh", "/github/workspace"]
