package com.proyect.masterdata;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.context.annotation.Configuration;

@SpringBootApplication
@Configuration
@EnableFeignClients
public class MasterdataApplication {

	public static void main(String[] args) {
		SpringApplication.run(MasterdataApplication.class, args);
	}

}
