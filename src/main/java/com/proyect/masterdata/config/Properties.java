package com.proyect.masterdata.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class Properties {
    private final String cloudinaryApiKey;
    public Properties(@Value("${cloudinary.api.key}") String cloudinaryApiKey){
        this.cloudinaryApiKey = cloudinaryApiKey;
    }

    public String getCloudinaryApiKey(){
        return cloudinaryApiKey;
    }
}
