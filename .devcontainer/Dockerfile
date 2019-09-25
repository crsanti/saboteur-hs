FROM haskell:8.6.5 as builder

# Create symlink bind directory for build or haskell ide engine
RUN mkdir -p $HOME/.local/bin

# Configure apt and install packages
ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update
RUN apt-get -y install --no-install-recommends apt-utils 2>&1

# Set up locales
RUN apt-get install -y locales \
  && localedef -i en_US -c -f UTF-8 -A /usr/share/locale/locale.alias en_US.UTF-8

# Verify git, process tools, lsb-release (common in install instructions for CLIs) installed
RUN apt-get -y install git procps lsb-release curl wget screen build-essential \
  # Install HIE Dependencies
  libicu-dev libtinfo-dev libgmp-dev

ENV LANG=en_US.UTF-8 LANGUAGE=en_US:en LC_ALL=en_US.UTF-8
RUN stack upgrade

RUN git clone https://github.com/digital-asset/ghcide.git
RUN git clone https://github.com/alanz/haskell-lsp.git

WORKDIR /ghcide
RUN stack install

WORKDIR /haskell-lsp
RUN stack install

RUN stack install hlint
