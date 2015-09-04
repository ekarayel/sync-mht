package com.github.ekarayel.syncmht.benchmarks;

import com.google.api.client.googleapis.compute.ComputeCredential;
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport;
import com.google.api.client.http.HttpTransport;
import com.google.api.client.json.jackson2.JacksonFactory;
import com.google.api.services.compute.Compute;
import com.google.api.services.storage.Storage;
import com.google.api.services.storage.model.StorageObject;

import java.io.IOException;
import java.security.GeneralSecurityException;

public class Stop {

    public static void main(String[] args) throws GeneralSecurityException, IOException {
        HttpTransport httpTransport = GoogleNetHttpTransport.newTrustedTransport();

        ComputeCredential credential =
                new ComputeCredential.Builder(httpTransport, JacksonFactory.getDefaultInstance())
                .build();

        StorageObject obj = new StorageObject();
        obj.set("test", 0.1);

        // Upload metrics
        new Storage.Builder(httpTransport, JacksonFactory.getDefaultInstance(), credential)
        .setApplicationName(Constants.APP_NAME)
        .build()
        .objects()
        .insert(Constants.APP_NAME, obj).execute();

        String instanceName = System.getenv("INSTANCE_ID");

        // Shutdown
        new Compute.Builder(httpTransport, JacksonFactory.getDefaultInstance(), credential)
        .setApplicationName(Constants.APP_NAME)
        .build()
        .instances()
        .delete(Constants.PROJECT_ID, Constants.ZONE_NAME, instanceName)
        .execute();
    }
}
