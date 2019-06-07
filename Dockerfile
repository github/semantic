FROM haskell:8.6 as build
WORKDIR /build

# Build our upstream dependencies after copying in only enough to tell cabal
# what they are.  This will make these layers cache better even as we change the
# code of semantic itself.
# COPY semantic.cabal .
# COPY cabal.project .
# COPY semantic-core/semantic-core.cabal semantic-core/semantic-core.cabal
# COPY vendor vendor
# RUN cabal new-update
# RUN cabal new-build --only-dependencies

# Once the dependencies are built, copy in the rest of the code and compile
# semantic itself.
COPY . .
RUN cabal new-update
RUN cabal new-build

# A fake `install` target until we can get `cabal new-install` to work
RUN cp $(find dist-newstyle -name semantic -type f -perm -u=x) /usr/local/bin/semantic

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
