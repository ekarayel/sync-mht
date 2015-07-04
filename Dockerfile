FROM agrafix/ghc7.8:latest
WORKDIR /sync-mht
ADD . /sync-mht
RUN chmod +x src/build/scripts/build.sh
RUN src/build/scripts/build.sh
