package com.proyect.masterdata.config;

import com.cloudinary.Cloudinary;
import com.cloudinary.utils.ObjectUtils;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
public class CloudinaryConfig {
    private final Properties properties;
    private final Boolean secure = true;
    public CloudinaryConfig(Properties properties) {
        this.properties = properties;
    }

    @Bean
    public Cloudinary cloudinary(){
        return new Cloudinary(ObjectUtils.asMap(
                "cloud_name", properties.getCloudName(),
                "api_key", properties.getCloudinaryApiKey(),
                "api_secret", properties.getApiSecret(),
                "secure", true
        ));
    }
}
