package com.github.ekarayel.syncmht.benchmarks;

import java.io.IOException;
import java.security.GeneralSecurityException;
import java.security.KeyStore;
import java.security.PrivateKey;
import java.util.Collections;

import com.google.api.client.googleapis.auth.oauth2.GoogleCredential;
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport;
import com.google.api.client.json.jackson2.JacksonFactory;
import com.google.api.services.compute.ComputeScopes;

public class Authorize
{
    private static final String CONTAINER_FILE = "sync-mht-benchmark-3fe707ebe6a9.p12";
    private static final String CONTAINER_PASSWORD_ENV_VAR = "P12_PASSWORD";
    private static final String ACCOUNT_ID =
        "804884601791-v8g2n2sfnmi3c38ah66o2h98let2r7ud@developer.gserviceaccount.com";

    public static GoogleCredential getCredential() throws GeneralSecurityException, IOException {
        KeyStore keystore = KeyStore.getInstance("PKCS12");
        keystore.load(
            Authorize.class.getClassLoader().getResourceAsStream(CONTAINER_FILE)
            , System.getenv(CONTAINER_PASSWORD_ENV_VAR).toCharArray()
        );

        return new GoogleCredential.Builder()
        .setTransport(GoogleNetHttpTransport.newTrustedTransport())
        .setJsonFactory(JacksonFactory.getDefaultInstance())
        .setServiceAccountId(ACCOUNT_ID)
        .setServiceAccountPrivateKey(
            (PrivateKey)keystore.getKey("privatekey", "notasecret".toCharArray())
        ).setServiceAccountScopes(Collections.singleton(ComputeScopes.COMPUTE))
        .build();
    }
}
