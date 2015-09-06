package com.github.ekarayel.syncmht.benchmarks;

import com.google.api.client.googleapis.auth.oauth2.GoogleCredential;
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport;
import com.google.api.client.http.HttpTransport;
import com.google.api.client.json.jackson2.JacksonFactory;
import com.google.api.services.compute.Compute;
import com.google.api.services.compute.ComputeScopes;
import com.google.api.services.compute.model.*;
import com.google.api.services.storage.StorageScopes;

import java.io.IOException;
import java.math.BigInteger;
import java.security.GeneralSecurityException;
import java.security.SecureRandom;
import java.util.Arrays;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Main {
    private static final String PROJECT_PREFIX = "https://www.googleapis.com/compute/v1/projects/";

    public static void main(String[] args) throws GeneralSecurityException, IOException {
        HttpTransport httpTransport = GoogleNetHttpTransport.newTrustedTransport();
        GoogleCredential cred = Authorize.getCredential();

        Compute compute =
            new Compute.Builder(httpTransport, JacksonFactory.getDefaultInstance(), cred)
            .setApplicationName(Constants.APP_NAME)
            .build();

        String instanceName = "sync-mht-benchmarks-instance-"+
            new BigInteger(130, new SecureRandom()).toString(32);

        Instance instance =
            new Instance()
            .setServiceAccounts(Collections.singletonList(
                new ServiceAccount()
                .setEmail("default")
                .setScopes(
                    Arrays.asList(StorageScopes.DEVSTORAGE_FULL_CONTROL, ComputeScopes.COMPUTE)
                )
            )).setName(instanceName)
            .setMachineType(
                PROJECT_PREFIX + Constants.PROJECT_ID + "/zones/" + Constants.ZONE_NAME
                + "/machineTypes/n1-standard-1"
            ).setNetworkInterfaces(Collections.singletonList(
                new NetworkInterface()
                .setNetwork(PROJECT_PREFIX + Constants.PROJECT_ID + "/global/networks/default")
                .setAccessConfigs(Collections.singletonList(
                    new AccessConfig()
                    .setType("ONE_TO_ONE_NAT")
                    .setName("External NAT")
                ))
            )).setDisks(Collections.singletonList(
                new AttachedDisk()
                .setBoot(true)
                .setAutoDelete(true)
                .setType("PERSISTENT")
                .setInitializeParams(
                    new AttachedDiskInitializeParams()
                    .setDiskName(instanceName)
                    .setSourceImage(
                        PROJECT_PREFIX +
                        "ubuntu-os-cloud/global/images/ubuntu-1504-vivid-v20150616a"
                    ).setDiskType(
                        PROJECT_PREFIX + Constants.PROJECT_ID + "/zones/" + Constants.ZONE_NAME +
                        "/diskTypes/pd-standard"
                    )
                )
            )).setMetadata(
                new Metadata()
                .setItems(Collections.singletonList(
                    new Metadata.Items()
                    .setKey("startup-script")
                    .setValue(StartupScript.get(instanceName))
                ))
            );

        compute.instances().insert(Constants.PROJECT_ID, Constants.ZONE_NAME, instance).execute();
    }
}
