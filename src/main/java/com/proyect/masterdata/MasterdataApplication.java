package com.proyect.masterdata;

import com.proyect.masterdata.seeder.Seeder;
import lombok.RequiredArgsConstructor;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Configuration;

@SpringBootApplication
@Configuration
@RequiredArgsConstructor
public class MasterdataApplication {

	private final Seeder seeder;

	public static void main(String[] args) {

		SpringApplication.run(MasterdataApplication.class, args);
	}

}
