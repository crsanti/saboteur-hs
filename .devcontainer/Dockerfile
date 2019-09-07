FROM haskell:8.6.5 as builder

# Create symlink bind directory for build or haskell ide engine
RUN mkdir -p $HOME/.local/bin

# Configure apt and install packages
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update \
  && apt-get -y install --no-install-recommends apt-utils 2>&1 \
  #
  # Set up locales
  && apt-get install -y locales \
  && localedef -i en_US -c -f UTF-8 -A /usr/share/locale/locale.alias en_US.UTF-8 \
  #
  # Verify git, process tools, lsb-release (common in install instructions for CLIs) installed
  && apt-get -y install git procps lsb-release \
  && apt-get -y install curl wget screen build-essential \
  #
  # Install HIE Dependencies
  && apt-get -y install libicu-dev libtinfo-dev libgmp-dev \
  #
  # Install HIE
  && git clone https://github.com/haskell/haskell-ide-engine --branch=0.12.0.0 --recurse-submodules \
  && cd /haskell-ide-engine \
  && sed -i "s|lts-13.18 # GHC 8.6.4|lts-13.30 # GHC 8.6.5|g" install/shake.yaml \
  && sed -i "s|, shake == 0.17.8|, shake == 0.17.9|g" install/hie-install.cabal \
  #
  # Ensure stack is up to date
  && stack upgrade \
  && stack ./install.hs stack-hie-8.6.5 \
  && stack ./install.hs stack-build-data \
  #
  # Clean HIE build files
  && rm -rf /haskell-ide-engine \
  #
  # Clean up
  && apt-get autoremove -y \
  && apt-get clean -y \
  && rm -rf /var/lib/apt/lists/*

ENV LANG=en_US.UTF-8 LANGUAGE=en_US:en LC_ALL=en_US.UTF-8

# Switch back to dialog for any ad-hoc use of apt-get
ENV DEBIAN_FRONTEND=

FROM haskell:8.6.5

# Configure apt and install packages
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update \
  && apt-get -y install --no-install-recommends apt-utils 2>&1 \
  #
  # Set up locales
  && apt-get install -y locales \
  && localedef -i en_US -c -f UTF-8 -A /usr/share/locale/locale.alias en_US.UTF-8 \
  #
  # Ensure stack is up to date
  && stack upgrade \
  #
  # Clean up
  && apt-get autoremove -y \
  && apt-get clean -y \
  && rm -rf /var/lib/apt/lists/*

ENV LANG=en_US.UTF-8 LANGUAGE=en_US:en LC_ALL=en_US.UTF-8
COPY --from=builder /root/.hoogle $HOME
COPY --from=builder /root/.local $HOME
COPY --from=builder /root/.stack $HOME

# Switch back to dialog for any ad-hoc use of apt-get
ENV DEBIAN_FRONTEND=
