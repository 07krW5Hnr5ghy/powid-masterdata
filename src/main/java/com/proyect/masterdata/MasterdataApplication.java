package com.proyect.masterdata;

import com.mercadopago.MercadoPagoConfig;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import com.proyect.masterdata.seeder.Seeder;

import lombok.RequiredArgsConstructor;

import java.util.logging.Level;

import static java.util.logging.Level.*;

@SpringBootApplication
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
