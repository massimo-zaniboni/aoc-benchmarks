# syntax=docker/dockerfile:1
FROM ubuntu:21.10

# Add additional repositories

RUN apt-get -y update && \
    apt-get -y upgrade && \
    apt-get -y install bash gnupg gnupg-utils gpg && \
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 40976EAF437D05B5 && \
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 3B4FE6ACC0B21F32

COPY docker-files/oldrepo.list /etc/apt/sources.list.d/oldrepo.list

ENV TZ="Europe/Rome"

# Install applications needed by the bencher

RUN apt-get -y update && apt-get -y upgrade && \
    DEBIAN_FRONTEND="noninteractive" TZ="Europe/Rome" apt-get -y install tzdata && \
    apt-get -y install \
                 python2 python2-dev \
                 python-gtk2 libgtop2-7 libgtop2-dev \
                 ndiff highlight less bash curl vim nano lrzip \
                 wget bzip2 pixz build-essential gcc \
                 php php-cli php-common php-gmp && \
    mkdir -p /downloads && \
    cd /downloads && \
    wget https://downloads.asterisell.com/aoc/python-gtop_2.32.0+dfsg-1_amd64.deb && \
    dpkg -i python-gtop_2.32.0+dfsg-1_amd64.deb && \
    apt-get clean

# Install recent GHC compiler using ghcup

RUN apt-get install -y \
            git jq bc make automake \
            rsync htop curl build-essential \
            pkg-config libffi-dev libgmp-dev \
            libssl-dev libtinfo-dev libsystemd-dev \
            zlib1g-dev make g++ wget libncursesw5 libtool autoconf && \
    apt-get clean && \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    bash -c "curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh" && \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    bash -c "curl -sSL https://get.haskellstack.org/ | sh"

ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
ENV PATH=${PATH}:/root/.local/bin
ENV PATH=${PATH}:/root/.ghcup/bin

RUN bash -c "ghcup upgrade" && \
    bash -c "ghcup install cabal 3.6.2.0" && \
    bash -c "ghcup set cabal 3.6.2.0" && \
    bash -c "ghcup install ghc 9.2.1" && \
    bash -c "ghcup set ghc 9.2.1" && \
    bash -c "echo PATH="$HOME/.local/bin:$PATH" >> $HOME/.bashrc" && \
    bash -c "echo export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH" >> $HOME/.bashrc" && \
    bash -c "source $HOME/.bashrc"

RUN bash -c "cabal update"

# Install Java and Clojure

RUN apt-get -y install \
                 llvm llvm-dev \
                 openjdk-17-jdk \
                 clojure libdata-priority-map-clojure && \
    apt-get clean && \
    cd /downloads && \
    curl -sLO https://raw.githubusercontent.com/babashka/babashka/master/install && \
    chmod u+x install && \
    ./install && \
    rm install

# Install SBCL

COPY docker-files/install-quicklisp.lisp /downloads/install-quicklisp.lisp

RUN cd /downloads && \
    wget https://downloads.asterisell.com/aoc/sbcl-2.1.11-x86-64-linux-binary.tar.bz2 && \
    tar xf sbcl-2.1.11-x86-64-linux-binary.tar.bz2 && \
    cd sbcl-2.1.11-x86-64-linux && \
    bash ./install.sh && \
    cd /downloads && \
    rm -r -f sbcl-* && \
    wget https://beta.quicklisp.org/quicklisp.lisp && \
    yes "" | sbcl --script install-quicklisp.lisp

# Install DEV libraries

RUN apt-get -y install libjudy-dev && \
    apt-get clean

# Install bencher utility

COPY apps/bencher /bencher

# The default CMD to run
LABEL description="Compile and benchmark aoc-benchmarks programs."
WORKDIR /bencher
CMD bash -c "python bin/bencher.py"
