package com.proyect.masterdata.config;

import com.cloudinary.Cloudinary;
import com.cloudinary.utils.ObjectUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.HashMap;
import java.util.Map;

@Configuration
public class CloudinaryConfig {
    private final Properties properties;
    @Value("${cloudinary.api.secret}")
    private String apiSecret;
    @Value("${cloudinary.cloud.name}")
    private String cloudName;
    private final Boolean secure = true;

    public CloudinaryConfig(Properties properties) {
        this.properties = properties;
    }

    @Bean
    public Cloudinary cloudinary(){
        return new Cloudinary(ObjectUtils.asMap(
                "cloud_name", cloudName,
                "api_key", properties.getCloudinaryApiKey(),
                "api_secret", apiSecret,
                "secure", true
        ));
    }
}
