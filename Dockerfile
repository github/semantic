FROM haskell:8.6 as build
WORKDIR /build
RUN cabal new-update
COPY . /build
RUN cabal new-build semantic:exe:semantic

# A fake `install` target until we can get `cabal new-install` to work
RUN cp $(find dist-newstyle -name semantic -type f -perm -u=x) /semantic

FROM debian:stretch
RUN apt-get update && \
  apt-get install -y \
    libgmp10 \
    && \
  apt-get autoremove -y && \
  apt-get clean -y && \
  rm -rf /var/lib/apt/lists/*
COPY --from=build /semantic /usr/local/bin/semantic
ENTRYPOINT ["/usr/local/bin/semantic"]
