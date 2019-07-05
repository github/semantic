# Put protoc and twirp tooling in it's own image
FROM golang:1.12-stretch AS protoc
RUN apt-get update && apt-get install -y unzip
ENV PROTOBUF_VERSION=3.7.1
RUN wget "https://github.com/protocolbuffers/protobuf/releases/download/v3.7.1/protoc-$PROTOBUF_VERSION-linux-x86_64.zip" && \
    unzip "protoc-$PROTOBUF_VERSION-linux-x86_64.zip" -d "/protobuf"

RUN go get github.com/golang/protobuf/proto && \
    go get github.com/twitchtv/protogen/typemap && \
    go get github.com/tclem/twirp-haskell/pkg/gen/haskell && \
    go get github.com/tclem/twirp-haskell/protoc-gen-haskell

ENTRYPOINT ["/protobuf/bin/protoc", "-I/protobuf", "-I=/go/src/github.com/tclem/twirp-haskell"]

# Build semantic
FROM haskell:8.6 as build
WORKDIR /build

# Build and cache the dependencies first so we can cache these layers.
COPY semantic.cabal .
COPY semantic-core semantic-core
RUN cabal new-update hackage.haskell.org,HEAD
RUN cabal new-configure semantic semantic-core
RUN cabal new-build --only-dependencies

# Copy in and build the entire project
COPY . .
RUN cabal new-build --flags="release" semantic:exe:semantic

# A fake `install` target until we can get `cabal new-install` to work
RUN cp $(find dist-newstyle/build/x86_64-linux -name semantic -type f -perm -u=x) /usr/local/bin/semantic

# Create a fresh image containing only the compiled CLI program, so that the
# image isn't bulked up by all of the extra build state.
FROM debian:stretch-slim

RUN apt-get update && \
  apt-get install -y \
    libgmp10 \
    && \
  apt-get autoremove -y && \
  apt-get clean -y && \
  rm -rf /var/lib/apt/lists/*

COPY --from=build /usr/local/bin/semantic /usr/local/bin/semantic

ENTRYPOINT ["/usr/local/bin/semantic"]
