package com.github.ekarayel.syncmht.benchmarks;

import com.google.api.client.googleapis.compute.ComputeCredential;
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport;
import com.google.api.client.http.HttpTransport;
import com.google.api.client.http.InputStreamContent;
import com.google.api.client.json.jackson2.JacksonFactory;
import com.google.api.services.compute.Compute;
import com.google.api.services.storage.Storage;

import java.io.FileInputStream;
import java.io.IOException;
import java.security.GeneralSecurityException;
import java.time.Instant;

public class Stop {

    private static String INSTANCE_ID_ENV = "INSTANCE_ID";
    private static String TRAVIS_COMMIT_ENV = "TRAVIS_COMMIT";

    public static void main(String[] args) throws GeneralSecurityException, IOException {
        HttpTransport httpTransport = GoogleNetHttpTransport.newTrustedTransport();

        ComputeCredential credential =
            new ComputeCredential.Builder(httpTransport, JacksonFactory.getDefaultInstance())
            .build();

        FileInputStream fis = new FileInputStream("benchmarks.json");
        InputStreamContent mediaContent = new InputStreamContent("application/json", fis);

        // Upload metrics
        new Storage.Builder(httpTransport, JacksonFactory.getDefaultInstance(), credential)
        .setApplicationName(Constants.APP_NAME)
        .build()
        .objects()
        .insert(Constants.BUCKET_NAME, null, mediaContent)
        .setName("benchmark-"+Instant.now().toString())
        .execute();

        // Shutdown
        new Compute.Builder(httpTransport, JacksonFactory.getDefaultInstance(), credential)
        .setApplicationName(Constants.APP_NAME)
        .build()
        .instances()
        .delete(Constants.PROJECT_ID, Constants.ZONE_NAME, System.getenv("X"+INSTANCE_ID_ENV))
        .execute();
    }
}
