# Performance Benchmark compared to rsync
FROM agrafix/ghc7.8:latest
RUN apt-get update && apt-get install -y ssh openssh-server zip
ADD . /sync-mht/
WORKDIR /sync-mht
RUN chmod +x configure.hs
RUN ./configure.hs && cabal update && cabal install --enable-benchmarks
RUN chmod +x benchmarks/src/main/resources/benchmarks.sh
