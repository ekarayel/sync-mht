FROM agrafix/ghc7.8:latest
WORKDIR /sync-mht
ADD . /sync-mht
RUN cabal update && cabal install
VOLUME /repo
CMD ["true"]
