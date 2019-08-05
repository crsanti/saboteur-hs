## Dockerfile for a haskell environment
FROM haskell:8.6.5

# Configure apt
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update \
  && apt-get -y install --no-install-recommends apt-utils 2>&1 \
  && apt-get autoremove -y \
  && apt-get clean -y \
  && rm -rf /var/lib/apt/lists/*

# Install haskell ide engine dependencies
RUN apt-get update \
  && apt-get -y install libicu-dev libtinfo-dev libgmp-dev \
  && apt-get autoremove -y \
  && apt-get clean -y \
  && rm -rf /var/lib/apt/lists/*

# Create symlink bind directory for build or haskell ide engine
RUN mkdir -p $HOME/.local/bin

# Install haskell ide engine
RUN git clone https://github.com/haskell/haskell-ide-engine --recurse-submodules \
  && cd haskell-ide-engine \
  && stack upgrade \
  && sed -i "s|lts-13.18 # GHC 8.6.4|lts-13.23 # GHC 8.6.5|g" install/shake.yaml \
  && sed -i "s|, shake == 0.17.8|, shake == 0.17.9|g" install/hie-install.cabal \
  && stack install.hs hie-8.6.5 \
  && stack install.hs stack-build-data \
  && cd .. \
  && rm -rf haskell-ide-engine

ENV DEBIAN_FRONTEND=dialog

# Set the default shell to bash rather than sh
ENV SHELL /bin/bash

CMD [ "ghci" ]
