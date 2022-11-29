ARG BASE_OS
ARG BASE_DISTRO
ARG CACHIX_AUTH_TOKEN

FROM $BASE_OS:$BASE_DISTRO

ENV TZ America/Chicago
ENV DEBIAN_FRONTEND=noninteractive
VOLUME ["/nix"]

RUN    apt-get update        \
    && apt-get install --yes \
        curl                 \
        git                  \
        xz-utils             \
        nix



RUN locale-gen en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL en_US.UTF-8

# install hpack and hlint from binary packages
RUN    curl -sSL https://github.com/sol/hpack/releases/download/${HPACK_VERSION}/hpack_linux.gz | gunzip -c > /usr/bin/hpack \
    && chmod +x /usr/bin/hpack \
    && hpack --version
RUN curl -sSL https://github.com/ndmitchell/hlint/releases/download/v${HLINT_VERSION}/hlint-${HLINT_VERSION}-x86_64-linux.tar.gz | tar xvz hlint-${HLINT_VERSION}/hlint \
    && mv hlint-${HLINT_VERSION}/hlint /usr/bin/hlint \
    && chmod +x /usr/bin/hlint \
    && hlint --version

# The image is built specifically for an environment with this user/group
ARG USER=github-user
ARG GROUP=$USER
ARG USER_ID=1000
ARG GROUP_ID=$USER_ID
RUN groupadd -g $GROUP_ID $GROUP && useradd -m -u $USER_ID -s /bin/sh -g $GROUP $USER

USER $USER:$GROUP

RUN    echo experimental-features = nix-command flakes | tee -a /etc/nix/nix.conf \
    && echo substituters = https://cache.nixos.org/ https://cache.iog.io | tee -a /etc/nix/nix.conf \
    && echo trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= | tee -a /etc/nix/nix.conf \
    && nix-env -iA cachix -f https://cachix.org/api/v1/install
    && cachix use runtimeverification

