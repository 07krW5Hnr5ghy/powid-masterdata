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
import com.proyect.masterdata.dto.request.RequestClientSave;
import com.proyect.masterdata.dto.request.RequestProductSave;
import com.proyect.masterdata.repository.AccessRepository;
import com.proyect.masterdata.repository.ClientRepository;
import com.proyect.masterdata.repository.DepartmentRepository;
import com.proyect.masterdata.repository.DistrictRepository;
import com.proyect.masterdata.repository.ProvinceRepository;
import com.proyect.masterdata.repository.RoleAccessRepository;
import com.proyect.masterdata.repository.RoleRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.repository.UserRoleRepository;
import com.proyect.masterdata.services.IBrand;
import com.proyect.masterdata.services.ICategory;
import com.proyect.masterdata.services.IClient;
import com.proyect.masterdata.services.IColor;
import com.proyect.masterdata.services.IModel;
import com.proyect.masterdata.services.IProduct;
import com.proyect.masterdata.services.ISize;
import com.proyect.masterdata.services.ISizeType;
import com.proyect.masterdata.services.IUser;

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

        private final IBrand iBrand;
        private final ICategory iCategory;
        private final IModel iModel;
        private final IProduct iProduct;
        private final IColor iColor;
        private final ISize iSize;
        private final ISizeType iSizeType;

        @Override
        public void run(String... args) throws Exception {

                // example one role and one access

                Access access = accessRepository
                                .save(new Access(1L, "USER_GET", true, new Date(System.currentTimeMillis()),
                                                new Date(System.currentTimeMillis()), "TEST"));

                Role role = roleRepository.save(new Role(
                                1L, "ADMINISTRATOR", true, new Date(System.currentTimeMillis()),
                                new Date(System.currentTimeMillis()), "TEST"));

                // departament, province and district to create system user

                Department department = departmentRepository
                                .save(new Department(1L, "SISTEMA", true, new Date(System.currentTimeMillis()),
                                                "TEST"));

                Province province = provinceRepository.save(new Province(1L, "SISTEMA", true,
                                new Date(System.currentTimeMillis()), department.getId(), "TEST", department));

                District district = districtRepository
                                .save(new District(1L, "SISTEMA", true, new Date(System.currentTimeMillis()),
                                                province.getId(),
                                                province, "TEST"));

                Client systemClient = clientRepository.save(new Client(1L, "SISTEMA", "SISTEMA", "SISTEMA", "SISTEMA",
                                "SISTEMA", "SISTEMA", "SISTEMA", "SISTEMA", true, district.getId(),
                                new Date(System.currentTimeMillis()), new Date(System.currentTimeMillis()), district));

                User adminUser = userRepository
                                .save(new User(1L, "ADMIN1", "JEISON", "CAMACHO", "1234567819", "jca@gmail.com",
                                                "cr 12 h 34", "M",
                                                "1234567819", passwordEncoder.encode("123abc+"), true,
                                                new Date(System.currentTimeMillis()),
                                                new Date(System.currentTimeMillis()), district.getId(),
                                                systemClient.getId(), "SISTEMA",
                                                district, systemClient));

                userRoleRepository.save(
                                new UserRole(1L, adminUser.getId(), role.getId(), "TEST",
                                                new Date(System.currentTimeMillis())));

                roleAccessRepository.save(
                                new RoleAccess(1L, role.getId(), access.getId(), "TEST",
                                                new Date(System.currentTimeMillis())));

                // user for register new users

                User registerUser = userRepository.save(
                                new User(2L, "REGISTER", "REGISTER", "REGISTER", "REGISTER", "REGISTER", "REGISTER",
                                                "REGISTER",
                                                "REGISTER", passwordEncoder.encode("321abc+"), true,
                                                new Date(System.currentTimeMillis()),
                                                new Date(System.currentTimeMillis()), district.getId(),
                                                systemClient.getId(), "SISTEMA",
                                                district, systemClient));

                // mocks clients

                Client client1 = clientRepository.save(new Client(2L, "GONZALO", "JIMENEZ", "12345678910",
                                "12345678910",
                                "COMPANY 1", "123456789", "CRA 123", "gj@gmail.com", true, district.getId(),
                                new Date(System.currentTimeMillis()), new Date(System.currentTimeMillis()), district));

                Client client2 = clientRepository.save(new Client(3L, "FERNANDO", "CASAS", "12345678911", "12345678911",
                                "COMPANY 2", "223456789", "CRA 124", "fc@gmail.com", true, district.getId(),
                                new Date(System.currentTimeMillis()), new Date(System.currentTimeMillis()), district));

                // mocks users

                User user1 = userRepository
                                .save(new User(3L, "GJIMENEZ", "GONZALO", "JIMENEZ", "12345678910", "gj@gmail.com",
                                                "CRA 123", "M", "123456789", passwordEncoder.encode("123abc+"), true,
                                                new Date(System.currentTimeMillis()),
                                                new Date(System.currentTimeMillis()), district.getId(), client1.getId(),
                                                "ADMIN1", district, client1));

                User user2 = userRepository
                                .save(new User(4L, "FCASAS", "FERNANDO", "CASAS", "12345678911", "fc@gmail.com",
                                                "CRA 124", "M", "123456789", passwordEncoder.encode("123abc+"), true,
                                                new Date(System.currentTimeMillis()),
                                                new Date(System.currentTimeMillis()), district.getId(), client2.getId(),
                                                "ADMIN1", district, client2));

                // mock categories
                iCategory.save("camisetas", "camisetas", "admin1");
                iCategory.save("jeans", "jeans", "admin1");
                iCategory.save("tennis", "tennis", "admin1");
                iCategory.save("botas", "botas", "admin1");
                iCategory.save("blusas", "blusas", "admin1");

                // mock color
                iColor.save("rojo", "admin1");
                iColor.save("verde", "admin1");
                iColor.save("azul", "admin1");
                iColor.save("amarillo", "admin1");
                iColor.save("morado", "admin1");
                iColor.save("naranja", "admin1");
                iColor.save("negro", "admin1");

                // mock size type
                iSizeType.save("ropa", "admin1");
                iSizeType.save("calzado", "admin1");
                iSizeType.save("accesorios", "admin1");

                // mock size
                iSize.save("s", "ropa", "admin1");
                iSize.save("m", "ropa", "admin1");
                iSize.save("l", "ropa", "admin1");
                iSize.save("xs", "ropa", "admin1");
                iSize.save("xm", "ropa", "admin1");
                iSize.save("xl", "ropa", "admin1");

                iSize.save("12", "calzado", "admin1");
                iSize.save("18", "calzado", "admin1");
                iSize.save("24", "calzado", "admin1");
                iSize.save("28", "calzado", "admin1");
                iSize.save("32", "calzado", "admin1");
                iSize.save("40", "calzado", "admin1");

                // mock brands

                iBrand.save("nike", "gjimenez");
                iBrand.save("levis", "gjimenez");
                iBrand.save("gap", "gjimenez");
                iBrand.save("adidas", "fcasas");
                iBrand.save("kenzo", "fcasas");
                iBrand.save("lacoste", "fcasas");

                // mock models
                iModel.save("f90", "nike", "gjimenez");
                iModel.save("m2000", "nike", "gjimenez");
                iModel.save("mercurial", "nike", "gjimenez");
                iModel.save("indigo", "levis", "gjimenez");
                iModel.save("old navy", "levis", "gjimenez");
                iModel.save("ripper", "levis", "gjimenez");
                iModel.save("sweater", "gap", "gjimenez");
                iModel.save("kasper", "gap", "gjimenez");
                iModel.save("sustra", "gap", "gjimenez");
                iModel.save("krust", "adidas", "fcasas");
                iModel.save("gist", "adidas", "fcasas");
                iModel.save("thunder", "adidas", "fcasas");
                iModel.save("yitro", "kenzo", "fcasas");
                iModel.save("ulcast", "kenzo", "fcasas");
                iModel.save("reinder", "kenzo", "fcasas");
                iModel.save("realt", "lacoste", "fcasas");
                iModel.save("brust", "lacoste", "fcasas");
                iModel.save("frost", "lacoste", "fcasas");

                // mock

                RequestProductSave product1 = new RequestProductSave().builder().build();
                product1.setCategory("tennis");
                product1.setColor("negro");
                product1.setModel("f90");
                product1.setSize("12");
                product1.setSku("A00001");

                iProduct.save(product1, "gjimenez");

                RequestProductSave product2 = new RequestProductSave().builder().build();
                product2.setCategory("botas");
                product2.setColor("rojo");
                product2.setModel("m2000");
                product2.setSize("24");
                product2.setSku("A00002");

                iProduct.save(product2, "gjimenez");

                RequestProductSave product3 = new RequestProductSave().builder().build();
                product3.setCategory("tennis");
                product3.setColor("verde");
                product3.setModel("mercurial");
                product3.setSize("24");
                product3.setSku("A00003");

                iProduct.save(product3, "gjimenez");

                RequestProductSave product4 = new RequestProductSave().builder().build();
                product4.setCategory("camisetas");
                product4.setColor("rojo");
                product4.setModel("indigo");
                product4.setSize("s");
                product4.setSku("A00004");

                iProduct.save(product4, "gjimenez");

                RequestProductSave product5 = new RequestProductSave().builder().build();
                product5.setCategory("jeans");
                product5.setColor("azul");
                product5.setModel("old navy");
                product5.setSize("m");
                product5.setSku("A00005");

                iProduct.save(product5, "gjimenez");

                RequestProductSave product6 = new RequestProductSave().builder().build();
                product6.setCategory("blusas");
                product6.setColor("amarillo");
                product6.setModel("ripper");
                product6.setSize("l");
                product6.setSku("A00006");

                iProduct.save(product6, "gjimenez");

                RequestProductSave product7 = new RequestProductSave().builder().build();
                product7.setCategory("blusas");
                product7.setColor("morado");
                product7.setModel("sweater");
                product7.setSize("xs");
                product7.setSku("A00007");

                iProduct.save(product7, "gjimenez");

                RequestProductSave product8 = new RequestProductSave().builder().build();
                product8.setCategory("camisetas");
                product8.setColor("verde");
                product8.setModel("kasper");
                product8.setSize("xm");
                product8.setSku("A00008");

                iProduct.save(product8, "gjimenez");

                RequestProductSave product9 = new RequestProductSave().builder().build();
                product9.setCategory("blusas");
                product9.setColor("naranja");
                product9.setModel("sustra");
                product9.setSize("xl");
                product9.setSku("A00009");

                iProduct.save(product9, "gjimenez");

                RequestProductSave product10 = new RequestProductSave().builder().build();
                product10.setCategory("calzado");
                product10.setColor("rojo");
                product10.setModel("krust");
                product10.setSize("40");
                product10.setSku("B00001");

                iProduct.save(product10, "fcasas");

                RequestProductSave product11 = new RequestProductSave().builder().build();
                product11.setCategory("calzado");
                product11.setColor("verde");
                product11.setModel("gist");
                product11.setSize("36");
                product11.setSku("B00002");

                iProduct.save(product11, "fcasas");

                RequestProductSave product12 = new RequestProductSave().builder().build();
                product12.setCategory("calzado");
                product12.setColor("azul");
                product12.setModel("thunder");
                product12.setSize("18");
                product12.setSku("B00003");

                iProduct.save(product12, "fcasas");

                RequestProductSave product13 = new RequestProductSave().builder().build();
                product13.setCategory("camisetas");
                product13.setColor("negro");
                product13.setModel("yitro");
                product13.setSize("s");
                product13.setSku("B00004");

                iProduct.save(product13, "fcasas");

                RequestProductSave product14 = new RequestProductSave().builder().build();
                product14.setCategory("blusas");
                product14.setColor("morado");
                product14.setModel("ulcast");
                product14.setSize("m");
                product14.setSku("B00005");

                iProduct.save(product14, "fcasas");

                RequestProductSave product15 = new RequestProductSave().builder().build();
                product15.setCategory("jeans");
                product15.setColor("amarillo");
                product15.setModel("reinder");
                product15.setSize("l");
                product15.setSku("B00006");

                iProduct.save(product15, "fcasas");

                RequestProductSave product16 = new RequestProductSave().builder().build();
                product16.setCategory("camisetas");
                product16.setColor("rojo");
                product16.setModel("realt");
                product16.setSize("xl");
                product16.setSku("B00007");

                iProduct.save(product16, "fcasas");

                RequestProductSave product17 = new RequestProductSave().builder().build();
                product17.setCategory("blusas");
                product17.setColor("azul");
                product17.setModel("brust");
                product17.setSize("xs");
                product17.setSku("B00008");

                iProduct.save(product17, "fcasas");

                RequestProductSave product18 = new RequestProductSave().builder().build();
                product18.setCategory("camisetas");
                product18.setColor("naranja");
                product18.setModel("frost");
                product18.setSize("m");
                product18.setSku("B00009");

                iProduct.save(product18, "fcasas");

        }

}
