#!/usr/bin/env bash
apt-get update
apt-get install -y docker.io maven openjdk-7-jdk
service docker start
git clone https://github.com/ekarayel/sync-mht.git
cd sync-mht
git checkout -qf <<TRAVIS_COMMIT>>
export INSTANCE_ID="<<INSTANCE_ID>>"
mvn -f benchmarks/pom.xml test-compile exec:java \
    -DmainClass="com.github.ekarayel.syncmht.benchmarks.Stop"
