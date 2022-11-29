ARG BASE_OS
ARG BASE_DISTRO

FROM $BASE_OS:$BASE_DISTRO

ENV TZ America/Chicago
ENV DEBIAN_FRONTEND=noninteractive
VOLUME ["/nix"]

RUN    apt-get update        \
    && apt-get install --yes \
        curl                 \
        git                  \
        xz-utils

# The image is built specifically for an environment with this user/group
ARG USER=github-user
ARG GROUP=$USER
ARG USER_ID=1000
ARG GROUP_ID=$USER_ID
RUN groupadd -g $GROUP_ID $GROUP && useradd -m -u $USER_ID -s /bin/sh -g $GROUP $USER

USER $USER:$GROUP
