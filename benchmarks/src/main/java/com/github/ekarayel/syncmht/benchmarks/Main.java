package com.github.ekarayel.syncmht.benchmarks;

import com.google.api.client.googleapis.auth.oauth2.GoogleCredential;
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport;
import com.google.api.client.http.HttpTransport;
import com.google.api.client.json.jackson2.JacksonFactory;
import com.google.api.services.compute.Compute;
import com.google.api.services.compute.model.*;

import java.io.IOException;
import java.math.BigInteger;
import java.security.GeneralSecurityException;
import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class Main {
    private static final String SOURCE_IMAGE_PREFIX =
            "https://www.googleapis.com/compute/v1/projects/";
    private static final String SOURCE_IMAGE_PATH =
            "ubuntu-os-cloud/global/images/ubuntu-1504-vivid-v20150616a";

    private static final String NETWORK_INTERFACE_CONFIG = "ONE_TO_ONE_NAT";
    private static final String NETWORK_ACCESS_CONFIG = "External NAT";

    public static void main(String[] args) throws GeneralSecurityException, IOException {
        HttpTransport httpTransport = GoogleNetHttpTransport.newTrustedTransport();
        GoogleCredential cred = Authorize.getCredential();

        Compute compute =
                new Compute.Builder(httpTransport, JacksonFactory.getDefaultInstance(), cred)
                .setApplicationName(Constants.APP_NAME)
                .build();

        Instance instance = new Instance();
        SecureRandom random = new SecureRandom();
        String instanceName = "sync-mht-benchmarks-instance-"+
                new BigInteger(130, random).toString(32);

        instance.setName(instanceName);
        instance.setMachineType("https://www.googleapis.com/compute/v1/projects/"
                + Constants.PROJECT_ID + "/zones/" + Constants.ZONE_NAME
                + "/machineTypes/n1-standard-1");
        NetworkInterface ifc = new NetworkInterface();
        ifc.setNetwork("https://www.googleapis.com/compute/v1/projects/"
                + Constants.PROJECT_ID + "/global/networks/default");
        List<AccessConfig> configs = new ArrayList<>();
        AccessConfig config = new AccessConfig();
        config.setType(NETWORK_INTERFACE_CONFIG);
        config.setName(NETWORK_ACCESS_CONFIG);
        configs.add(config);
        ifc.setAccessConfigs(configs);
        instance.setNetworkInterfaces(Collections.singletonList(ifc));

        AttachedDisk disk = new AttachedDisk()
                .setBoot(true)
                .setAutoDelete(true)
                .setType("PERSISTENT");
        AttachedDiskInitializeParams params = new AttachedDiskInitializeParams();
        params.setDiskName(instanceName);
        params.setSourceImage(SOURCE_IMAGE_PREFIX + SOURCE_IMAGE_PATH);
        params.setDiskType("https://www.googleapis.com/compute/v1/projects/"
                + Constants.PROJECT_ID + "/zones/" + Constants.ZONE_NAME
                + "/diskTypes/pd-standard");
        disk.setInitializeParams(params);
        instance.setDisks(Collections.singletonList(disk));

        Metadata meta = new Metadata();
        Metadata.Items item = new Metadata.Items();
        item.setKey("startup-script");
        item.setValue(StartupScript.get(instanceName));
        meta.setItems(Collections.singletonList(item));
        instance.setMetadata(meta);
        Operation insert =
                compute.instances().insert(Constants.PROJECT_ID, Constants.ZONE_NAME, instance)
                .execute();
    }
}
