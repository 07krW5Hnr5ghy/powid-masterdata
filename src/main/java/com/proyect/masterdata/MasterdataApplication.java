package com.proyect.masterdata;

import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.openfeign.EnableFeignClients;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.security.crypto.password.PasswordEncoder;

import com.proyect.masterdata.domain.Access;
import com.proyect.masterdata.domain.Department;
import com.proyect.masterdata.domain.District;
import com.proyect.masterdata.domain.Province;
import com.proyect.masterdata.domain.Role;
import com.proyect.masterdata.domain.RoleAccess;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.UserRole;
import com.proyect.masterdata.repository.AccessRepository;
import com.proyect.masterdata.repository.DepartmentRepository;
import com.proyect.masterdata.repository.DistrictRepository;
import com.proyect.masterdata.repository.ProvinceRepository;
import com.proyect.masterdata.repository.RoleAccessRepository;
import com.proyect.masterdata.repository.RoleRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.repository.UserRoleRepository;

@SpringBootApplication
@EnableFeignClients
@Configuration
public class MasterdataApplication {

	public static void main(String[] args) {
		SpringApplication.run(MasterdataApplication.class, args);
	}

	@Bean
	CommandLineRunner run(AccessRepository accessRepository, RoleRepository roleRepository,
			UserRepository userRepository, PasswordEncoder passwordEncoder, DistrictRepository districtRepository,
			ProvinceRepository provinceRepository, DepartmentRepository departmentRepository,
			UserRoleRepository userRoleRepository, RoleAccessRepository roleAccessRepository) {

		return args -> {
			Access access = accessRepository
					.save(new Access(1L, "USER_GET", true, new Date(System.currentTimeMillis()),
							new Date(System.currentTimeMillis()), "TEST"));

			Role role = roleRepository.save(new Role(
					1L, "ADMINISTRATOR", true, new Date(System.currentTimeMillis()),
					new Date(System.currentTimeMillis()), "TEST"));

			Department department = departmentRepository
					.save(new Department(1L, "AMAZONAS", true, new Date(System.currentTimeMillis()), "TEST"));

			Province province = provinceRepository.save(new Province(1L, "CHACHAPOYAS", true,
					new Date(System.currentTimeMillis()), department.getId(), "TEST", department));

			District district = districtRepository
					.save(new District(1L, "ASUNCION", true, new Date(System.currentTimeMillis()), province.getId(),
							province, "TEST"));

			User adminUser = userRepository
					.save(new User(1L, "ADMIN1", "JEISON", "CAMACHO", "1234567819", "jca@gmail.com", "cr 12 h 34", "M",
							"1234567819", passwordEncoder.encode("123abc+"), true, new Date(System.currentTimeMillis()),
							new Date(System.currentTimeMillis()), district.getId(), district));

			User registerUser = userRepository.save(
					new User(2L, "REGISTER", "REGISTER", "REGISTER", "REGISTER", "REGISTER", "REGISTER", "REGISTER",
							"REGISTER", passwordEncoder.encode("321abc+"), true, new Date(System.currentTimeMillis()),
							new Date(System.currentTimeMillis()), district.getId(), district));

			userRoleRepository.save(
					new UserRole(1L, adminUser.getId(), role.getId(), "TEST", new Date(System.currentTimeMillis())));

			roleAccessRepository.save(
					new RoleAccess(1L, role.getId(), access.getId(), "TEST", new Date(System.currentTimeMillis())));

		};
	}

}
