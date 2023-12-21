package com.proyect.masterdata.seeder;

import java.util.Date;

import org.springframework.boot.CommandLineRunner;
import org.springframework.stereotype.Component;

import com.proyect.masterdata.domain.Access;
import com.proyect.masterdata.domain.Client;
import com.proyect.masterdata.domain.Department;
import com.proyect.masterdata.domain.District;
import com.proyect.masterdata.domain.Province;
import com.proyect.masterdata.domain.Role;
import com.proyect.masterdata.domain.RoleAccess;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.UserRole;
import com.proyect.masterdata.repository.AccessRepository;
import com.proyect.masterdata.repository.ClientRepository;
import com.proyect.masterdata.repository.DepartmentRepository;
import com.proyect.masterdata.repository.DistrictRepository;
import com.proyect.masterdata.repository.ProvinceRepository;
import com.proyect.masterdata.repository.RoleAccessRepository;
import com.proyect.masterdata.repository.RoleRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.repository.UserRoleRepository;
import org.springframework.security.crypto.password.PasswordEncoder;

import lombok.RequiredArgsConstructor;

@Component
@RequiredArgsConstructor
public class Seeder implements CommandLineRunner {

    private final AccessRepository accessRepository;
    private final ClientRepository clientRepository;
    private final DepartmentRepository departmentRepository;
    private final DistrictRepository districtRepository;
    private final ProvinceRepository provinceRepository;
    private final RoleAccessRepository roleAccessRepository;
    private final RoleRepository roleRepository;
    private final UserRepository userRepository;
    private final UserRoleRepository userRoleRepository;
    private final PasswordEncoder passwordEncoder;

    @Override
    public void run(String... args) throws Exception {

        Access access = accessRepository
                .save(new Access(1L, "USER_GET", true, new Date(System.currentTimeMillis()),
                        new Date(System.currentTimeMillis()), "TEST"));

        Role role = roleRepository.save(new Role(
                1L, "ADMINISTRATOR", true, new Date(System.currentTimeMillis()),
                new Date(System.currentTimeMillis()), "TEST"));

        Department department = departmentRepository
                .save(new Department(1L, "SISTEMA", true, new Date(System.currentTimeMillis()), "TEST"));

        Province province = provinceRepository.save(new Province(1L, "SISTEMA", true,
                new Date(System.currentTimeMillis()), department.getId(), "TEST", department));

        District district = districtRepository
                .save(new District(1L, "SISTEMA", true, new Date(System.currentTimeMillis()), province.getId(),
                        province, "TEST"));

        Client systemClient = clientRepository.save(new Client(1L, "SISTEMA", "SISTEMA", "SISTEMA", "SISTEMA",
                "SISTEMA", "SISTEMA", "SISTEMA", "SISTEMA", true, district.getId(),
                new Date(System.currentTimeMillis()), new Date(System.currentTimeMillis()), district));

        User adminUser = userRepository
                .save(new User(1L, "ADMIN1", "JEISON", "CAMACHO", "1234567819", "jca@gmail.com", "cr 12 h 34", "M",
                        "1234567819", passwordEncoder.encode("123abc+"), true,
                        new Date(System.currentTimeMillis()),
                        new Date(System.currentTimeMillis()), district.getId(), systemClient.getId(), "SISTEMA",
                        district, systemClient));

        User registerUser = userRepository.save(
                new User(2L, "REGISTER", "REGISTER", "REGISTER", "REGISTER", "REGISTER", "REGISTER", "REGISTER",
                        "REGISTER", passwordEncoder.encode("321abc+"), true,
                        new Date(System.currentTimeMillis()),
                        new Date(System.currentTimeMillis()), district.getId(), systemClient.getId(), "SISTEMA",
                        district, systemClient));

        userRoleRepository.save(
                new UserRole(1L, adminUser.getId(), role.getId(), "TEST", new Date(System.currentTimeMillis())));

        roleAccessRepository.save(
                new RoleAccess(1L, role.getId(), access.getId(), "TEST", new Date(System.currentTimeMillis())));
    }

}
