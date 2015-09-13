package com.github.ekarayel.syncmht.benchmarks;

import com.google.api.client.googleapis.compute.ComputeCredential;
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport;
import com.google.api.client.http.HttpTransport;
import com.google.api.client.http.InputStreamContent;
import com.google.api.client.json.jackson2.JacksonFactory;
import com.google.api.services.compute.Compute;
import com.google.api.services.storage.Storage;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.security.GeneralSecurityException;
import java.time.Instant;
import java.util.stream.Collectors;

public class Stop {

    private static String INSTANCE_ID_ENV = "INSTANCE_ID";
    private static String TRAVIS_COMMIT_ENV = "TRAVIS_COMMIT";

    public static void main(String[] args) throws GeneralSecurityException, IOException {
        HttpTransport httpTransport = GoogleNetHttpTransport.newTrustedTransport();

        ComputeCredential credential =
            new ComputeCredential.Builder(httpTransport, JacksonFactory.getDefaultInstance())
            .build();

        String name = "benchmark-"+Instant.now().toString();
        String jsContent = "benchmarkCallback("+Files.readAllLines(
            FileSystems.getDefault().getPath("benchmarks.json"),
            StandardCharsets.UTF_8
        ).stream().collect(Collectors.joining())+",\'"+name+"\');";

        InputStreamContent mediaContent = new InputStreamContent(
            "text/javascript",
            new ByteArrayInputStream(jsContent.getBytes(StandardCharsets.UTF_8))
        );

        // Upload metrics
        new Storage.Builder(httpTransport, JacksonFactory.getDefaultInstance(), credential)
        .setApplicationName(Constants.APP_NAME)
        .build()
        .objects()
        .insert(Constants.BUCKET_NAME, null, mediaContent)
        .setName(name)
        .execute();

        // Shutdown
        new Compute.Builder(httpTransport, JacksonFactory.getDefaultInstance(), credential)
        .setApplicationName(Constants.APP_NAME)
        .build()
        .instances()
        .delete(Constants.PROJECT_ID, Constants.ZONE_NAME, System.getenv(INSTANCE_ID_ENV))
        .execute();
    }
}
