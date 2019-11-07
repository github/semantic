# Put protoc and twirp tooling in its own image
FROM haskell:8.6 as haskell
RUN cabal v2-update && \
    cabal v2-install proto-lens-protoc
RUN which proto-lens-protoc

FROM golang:1.13-stretch AS protoc
RUN apt-get update && apt-get install -y unzip
ENV PROTOBUF_VERSION=3.7.1
RUN wget "https://github.com/protocolbuffers/protobuf/releases/download/v3.7.1/protoc-$PROTOBUF_VERSION-linux-x86_64.zip" && \
    unzip "protoc-$PROTOBUF_VERSION-linux-x86_64.zip" -d "/protobuf"

RUN go get github.com/golang/protobuf/proto && \
    go get github.com/twitchtv/protogen/typemap && \
    GO111MODULE=on go get github.com/tclem/proto-lens-jsonpb/protoc-gen-jsonpb_haskell@e4d10b77f57ee25beb759a33e63e2061420d3dc2

COPY --from=haskell /root/.cabal/bin/proto-lens-protoc /usr/local/bin/proto-lens-protoc

ENTRYPOINT ["/protobuf/bin/protoc", "-I/protobuf", "--plugin=protoc-gen-haskell=/usr/local/bin/proto-lens-protoc"]

# Build semantic
FROM haskell:8.6 as build
WORKDIR /build

# Build all of semantic
COPY . .
RUN cabal v2-update && \
    cabal v2-configure --flags="release" && \
    cabal v2-build semantic:exe:semantic

# A fake `install` target until we can get `cabal v2-install` to work
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
