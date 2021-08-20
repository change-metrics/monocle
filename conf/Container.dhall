let base = "registry.fedoraproject.org/fedora-minimal:34"

in  { builder =
        ''
        FROM ${base}
        ENV LANG=C.UTF-8
        RUN microdnf update -y && microdnf install -y git ghc cabal-install zlib-devel python3-virtualenv python3-devel openssl-devel gcc nodejs

        # Haskell toolchain
        RUN cabal v2-update && cabal v2-install cabal-install
        RUN cabal v2-install dhall dhall-yaml doctest hlint ghcid \
             --constraint='dhall ^>= 1.39'

        # Install project dependencies
        COPY haskell/cabal.project haskell/monocle.cabal /build/
        RUN cd /build; cabal v2-build -v1 --dependencies-only all
        ''
    }
