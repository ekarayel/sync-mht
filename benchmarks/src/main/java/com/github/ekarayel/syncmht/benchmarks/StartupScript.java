package com.github.ekarayel.syncmht.benchmarks;

import com.google.common.io.ByteStreams;

import java.io.IOException;
import java.nio.charset.StandardCharsets;

public class StartupScript {
    public static String get(String instanceName) throws IOException {
        byte[] startFile = ByteStreams.toByteArray(
                Main.class.getClassLoader().getResourceAsStream("startup.sh"));

        return new String(startFile, StandardCharsets.UTF_8)
                .replaceAll("<<TRAVIS_ID>>", System.getenv("TRAVIS_ID"))
                .replaceAll("<<INSTANCE_ID>>", instanceName);
    }
}
