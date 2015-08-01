FROM agrafix/ghc7.8:latest
RUN cabal update && cabal install cabal-install
RUN cabal update && cabal install sync-mht

