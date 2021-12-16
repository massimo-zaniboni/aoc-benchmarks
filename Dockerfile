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
                 ndiff highlight less bash curl vim nano \
                 wget bzip2 pixz build-essential gcc \
                 php php-cli php-common php-gmp && \
    mkdir -p /downloads && \
    cd /downloads && \
    wget https://downloads.asterisell.com/aoc/python-gtop_2.32.0+dfsg-1_amd64.deb && \
    dpkg -i python-gtop_2.32.0+dfsg-1_amd64.deb

# Install recent compilers

RUN apt-get -y install \
                 llvm llvm-dev \
                 ghc \
                 openjdk-17-jdk

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

# Download task data-sets

RUN apt-get -y install lrzip && \
    mkdir -p /downloads/datasets && \
    echo "Download datasets 01 version 01" && \
    cd /downloads/datasets && \
    wget https://downloads.asterisell.com/aoc/tmp-datasets-01.tar.lrz && \
    lrzuntar tmp-datasets-01.tar.lrz && \
    rm -f tmp-datasets-01.tar.lrz

COPY docker-files/small-datasets /downloads/small-datasets

# Run the container services

COPY docker-files/entrypoint.sh /entrypoint.sh

EXPOSE 8000

VOLUME  ["/host"]
WORKDIR /host
ENTRYPOINT ["/usr/bin/bash", "/entrypoint.sh"]
