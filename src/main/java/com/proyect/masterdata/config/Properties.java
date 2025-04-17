package com.proyect.masterdata.config;

import lombok.Getter;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Getter
@Component
public class Properties {
    private final String cloudinaryApiKey;
    private final String apiSecret;
    private final String cloudName;
    public Properties(
            @Value("${cloudinary.api.key}") String cloudinaryApiKey,
            @Value("${cloudinary.api.secret}") String apiSecret,
            @Value("${cloudinary.cloud.name}") String cloudName) {
        this.cloudinaryApiKey = cloudinaryApiKey;
        this.apiSecret = apiSecret;
        this.cloudName = cloudName;
    }
}
