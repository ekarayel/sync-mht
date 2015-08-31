package com.github.ekarayel.syncmht.benchmarks;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.security.GeneralSecurityException;
import java.security.KeyStore;
import java.security.PrivateKey;
import java.util.Collection;
import java.util.Collections;

import com.google.api.client.googleapis.auth.oauth2.GoogleCredential;
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport;
import com.google.api.client.http.HttpTransport;
import com.google.api.client.json.JsonFactory;
import com.google.api.client.json.jackson2.JacksonFactory;
import com.google.api.services.compute.ComputeScopes;

public class Authorize
{
    public static GoogleCredential getCredential() throws GeneralSecurityException, IOException {
        KeyStore keystore = KeyStore.getInstance("PKCS12");

        char[] p12Password = System.getenv("P12_PASSWORD").toCharArray();

        InputStream str = Authorize.class.getClassLoader()
                .getResourceAsStream("sync-mht-benchmark-3fe707ebe6a9.p12");
        keystore.load(str, p12Password);
        PrivateKey key = (PrivateKey)keystore.getKey("privatekey", "notasecret".toCharArray());

        String ac = "804884601791-v8g2n2sfnmi3c38ah66o2h98let2r7ud@developer.gserviceaccount.com";
        JsonFactory JSON_FACTORY = JacksonFactory.getDefaultInstance();
        HttpTransport httpTransport = GoogleNetHttpTransport.newTrustedTransport();
        GoogleCredential credential = new GoogleCredential.Builder()
            .setTransport(httpTransport)
            .setJsonFactory(JSON_FACTORY)
            .setServiceAccountId(ac)
            .setServiceAccountPrivateKey(key)
            .setServiceAccountScopes(Collections.singleton(ComputeScopes.COMPUTE))
            .build();

        return credential;
    }
}
