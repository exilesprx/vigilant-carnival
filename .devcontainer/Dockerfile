ARG VERSION=9.2
FROM haskell:${VERSION}

# Use the noninteractive mode for minimal installation of ghcup
ENV BOOTSTRAP_HASKELL_MINIMAL=1
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
ENV BOOTSTRAP_HASKELL_GHC_VERSION=${VERSION}

# Create a non-root user
RUN useradd -m -s /bin/bash -u 1000 haskell
USER haskell

# Install ghcup and source it in the bashrc
RUN bash -c "curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh" \
  && echo "source /home/haskell/.ghcup/env" >> ~/.bashrc