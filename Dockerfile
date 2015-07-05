FROM agrafix/ghc7.8:latest
WORKDIR /sync-mht
ADD . /sync-mht
RUN chmod +x src/build/scripts/*.sh
RUN src/build/scripts/build.sh
