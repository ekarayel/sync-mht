#!/usr/bin/env bash
apt-get update
apt-get install -y docker.io maven openjdk-8-jdk
service docker start
git clone https://github.com/ekarayel/sync-mht.git
cd sync-mht
git checkout -qf <<TRAVIS_COMMIT>>
git submodule init
git submodule update
export INSTANCE_ID="<<INSTANCE_ID>>"
docker build -f benchmarks/src/main/resources/Dockerfile -t ekarayel/sync-mht-benchmarks .
docker run --privileged ekarayel/sync-mht-benchmarks \
    /sync-mht/benchmarks/src/main/resources/benchmarks.sh \
    <<TRAVIS_COMMIT>> > "benchmarks.json"
mvn -f benchmarks/pom.xml test-compile exec:java \
    -DmainClass="com.github.ekarayel.syncmht.benchmarks.Stop"
