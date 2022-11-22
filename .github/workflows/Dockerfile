ARG BASE_OS
ARG BASE_DISTRO
ARG HPACK_VERSION
ARG FOURMOLU_VERSION
ARG HLINT_VERSION
FROM ${BASE_OS}:${BASE_DISTRO}

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

RUN curl -sSL https://get.haskellstack.org/ | sh
RUN    curl -sSL https://github.com/sol/hpack/releases/download/${HPACK_VERSION}/hpack_linux.gz | gunzip -c > /usr/bin/hpack \
    && chmod +x /usr/bin/hpack
RUN    curl -sSL https://github.com/fourmolu/fourmolu/releases/download/v${FOURMOLU_VERSION}/fourmolu-${FOURMOLU_VERSION}-linux-x86_64 -o /usr/bin/fourmolu \
    && chmod +x /usr/bin/fourmolu                                                                                                                           \
    && fourmolu --version
RUN curl -sSL https://github.com/ndmitchell/hlint/releases/download/v${HLINT_VERSION}/hlint-${HLINT_VERSION}-x86_64-linux.tar.gz | tar xvz hlint-${HLINT_VERSION}/hlint \
    && mv hlint-${HLINT_VERSION}/hlint /usr/bin/hlint                                                                                                                   \
    && chmod +x /usr/bin/hlint

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
