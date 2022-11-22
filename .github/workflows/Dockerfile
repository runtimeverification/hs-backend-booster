ARG BASE_OS
ARG BASE_DISTRO
ARG HPACK_VERSION
ARG FOURMOLU_VERSION
ARG HLINT_VERSION
ARG GHC_VERSION

FROM haskell:${GHC_VERSION}-slim-buster

ENV TZ America/Chicago
ENV DEBIAN_FRONTEND=noninteractive

ARG HPACK_VERSION
ARG FOURMOLU_VERSION
ARG HLINT_VERSION

RUN    apt-get update        \
    && apt-get install --yes \
        curl                 \
        libtinfo-dev         \
        locales              \
        git                  \
        z3

RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

# The image is built specifically for an environment with this user/group
ARG USER=github-user
ARG GROUP=$USER
ARG USER_ID=1000
ARG GROUP_ID=$USER_ID
RUN groupadd -g $GROUP_ID $GROUP && useradd -m -u $USER_ID -s /bin/sh -g $GROUP $USER

USER $USER:$GROUP
ENV PATH $PATH:$HOME/.cabal.bin

RUN    cabal update \
    && cabal install hlint-${HLINT_VERSION} hpack-${HPACK_VERSION} fourmolu-${FOURMOLU_VERSION} \
    && set -x \
    && hlint --version \
    && hpack --version \
    && fourmolu --version \
