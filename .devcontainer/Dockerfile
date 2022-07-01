# See here for image contents: https://github.com/microsoft/vscode-dev-containers/tree/v0.233.0/containers/debian/.devcontainer/base.Dockerfile

# [Choice] Debian version (use bullseye on local arm64/Apple Silicon): bullseye, buster
ARG VARIANT="buster"
FROM mcr.microsoft.com/vscode/devcontainers/base:0-${VARIANT}

# ** [Optional] Uncomment this section to install additional packages. **
RUN apt-get update && export DEBIAN_FRONTEND=noninteractive \
     && apt-get -y install --no-install-recommends build-essential curl libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5

# Install ghcup
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
ENV GHCUP_INSTALL_BASE_PREFIX=/home/vscode
ENV PATH=${PATH}:/home/vscode/.ghcup/bin

RUN bash -c "curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh"
RUN chown -R vscode /home/vscode/.ghcup

# Note: hopefully, the latest stack and hls lineup with the default cabal/ghc versions!
RUN ghcup install stack
RUN ghcup install hls

# Update cabal and install relevant dependencies
# RUN cabal update
RUN stack install hlint