ARG RUST_VERSION=1.98.1
ARG APP_NAME=tamil_dictionary
ARG RESOURCE_PATH="/res"

################################################################################
# Build stage (Rust image)
################################################################################

FROM docker.io/library/rust:${RUST_VERSION}-alpine AS build

ARG APP_NAME

WORKDIR /app

RUN apk add --no-cache clang lld musl-dev git

RUN --mount=type=bind,source=src,target=src \
    --mount=type=bind,source=Cargo.toml,target=Cargo.toml \
    --mount=type=bind,source=Cargo.lock,target=Cargo.lock \
    --mount=type=cache,target=/app/target/ \
    --mount=type=cache,target=/var/cache/cargo \
    CARGO_HOME=/var/cache/cargo cargo build --locked --release && \
    cp ./target/release/$APP_NAME /bin/server

################################################################################
# Runtime stage (distroless image)
################################################################################

FROM cgr.dev/chainguard/static:latest AS final

ARG RESOURCE_PATH

COPY --from=build /bin/server /bin/

WORKDIR /app

COPY --chmod=a=rX res/ res/
COPY --chmod=a=rX templates/ templates/

COPY --chmod=a=r dictionary.json .
COPY --chmod=a=r verbs.json* .

ENV ROCKET_ADDRESS=0.0.0.0
ENV RESOURCE_PATH=${RESOURCE_PATH}

EXPOSE 8000

ENTRYPOINT ["/bin/server"]
