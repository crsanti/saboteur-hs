## Dockerfile for a haskell environment
FROM haskell:8.6.5

# Create symlink bind directory for build or haskell ide engine
RUN mkdir -p $HOME/.local/bin

# Configure apt and install packages
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update
RUN apt-get -y install --no-install-recommends apt-utils 2>&1
#
# Set up locales
RUN apt-get install -y locales \
  && localedef -i en_US -c -f UTF-8 -A /usr/share/locale/locale.alias en_US.UTF-8
#
# Verify git, process tools, lsb-release (common in install instructions for CLIs) installed
RUN apt-get -y install git procps lsb-release \
  && apt-get -y install curl wget screen build-essential
#
# Install HIE Dependencies
RUN apt-get -y install libicu-dev libtinfo-dev libgmp-dev
#
# Ensure cabal is up to date
# && cabal update \
#
# Install HIE
RUN git clone https://github.com/haskell/haskell-ide-engine --branch=0.12.0.0 --recurse-submodules
WORKDIR /haskell-ide-engine
RUN sed -i "s|lts-13.18 # GHC 8.6.4|lts-13.30 # GHC 8.6.5|g" install/shake.yaml
RUN sed -i "s|, shake == 0.17.8|, shake == 0.17.9|g" install/hie-install.cabal

# RUN stack --resolver ghc-8.6.5 setup
RUN stack upgrade

RUN stack ./install.hs stack-hie-8.6.5
RUN stack ./install.hs stack-build-data
#
# Clean HIE build files
# && rm -rf /haskell-ide-engine \
#
# Clean up
# && apt-get autoremove -y \
# && apt-get clean -y \
# && rm -rf /var/lib/apt/lists/*

ENV LANG en_US.utf8

# Switch back to dialog for any ad-hoc use of apt-get
ENV DEBIAN_FRONTEND=
