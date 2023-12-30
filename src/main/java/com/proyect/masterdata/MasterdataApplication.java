package com.proyect.masterdata;

import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.proyect.masterdata.seeder.Seeder;

import lombok.RequiredArgsConstructor;

@SpringBootApplication
@EnableFeignClients
@Configuration
@RequiredArgsConstructor
public class MasterdataApplication {

	private final Seeder seeder;

	public static void main(String[] args) {
		SpringApplication.run(MasterdataApplication.class, args);
	}

	// @Bean
	// CommandLineRunner executeSeeder() {
	// return args -> seeder.run(args);
	// }

}
