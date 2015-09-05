package com.github.ekarayel.syncmht.benchmarks;

import com.google.common.io.ByteStreams;

import java.io.IOException;
import java.nio.charset.StandardCharsets;

public class StartupScript {
    public static String get(String instanceName) throws IOException {
        return new String(ByteStreams.toByteArray(
            StartupScript.class.getClassLoader().getResourceAsStream("startup.sh")),
            StandardCharsets.UTF_8
        ).replaceAll("<<TRAVIS_COMMIT>>", System.getenv("TRAVIS_COMMIT"))
        .replaceAll("<<INSTANCE_ID>>", instanceName);
    }
}
