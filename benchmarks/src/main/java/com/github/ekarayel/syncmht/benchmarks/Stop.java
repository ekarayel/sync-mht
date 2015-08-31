package com.github.ekarayel.syncmht.benchmarks;

import com.google.api.client.googleapis.compute.ComputeCredential;
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport;
import com.google.api.client.http.HttpTransport;
import com.google.api.client.json.jackson2.JacksonFactory;
import com.google.api.services.compute.Compute;

import java.io.IOException;
import java.security.GeneralSecurityException;

public class Stop {

    public static void main(String[] args) throws GeneralSecurityException, IOException {
        HttpTransport httpTransport = GoogleNetHttpTransport.newTrustedTransport();
        ComputeCredential credential =
                new ComputeCredential.Builder(httpTransport, JacksonFactory.getDefaultInstance())
                .build();
        Compute compute =
                new Compute.Builder(httpTransport, JacksonFactory.getDefaultInstance(), credential)
                        .setApplicationName(Constants.APP_NAME)
                        .build();
        String instanceName = System.getenv("INSTANCE_ID");
        compute.instances().delete(Constants.PROJECT_ID, Constants.ZONE_NAME, instanceName);
    }
}
