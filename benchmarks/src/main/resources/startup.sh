#!/usr/bin/env bash
apt-get update
apt-get install -y docker.io maven
service docker start
git clone https://github.com/ekarayel/sync-mht.git
cd sync-mht
git checkout -qf <<TRAVIS_COMMIT>>
mvn -f benchmarks/pom.xml test-compile exec:java \
    -D exec.mainClass="com.github.ekarayel.syncmht.benchmarks.Stop"
export INSTANCE_ID="<<INSTANCE_ID>>"
