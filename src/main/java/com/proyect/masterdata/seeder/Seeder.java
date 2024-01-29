package com.proyect.masterdata.seeder;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.proyect.masterdata.dto.request.*;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.*;
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
import com.proyect.masterdata.dto.LocationDTO;

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
        private final ISupplier iSupplier;
        private final ISupplierProduct iSupplierProduct;
        private final IClosingChannel iClosingChannel;
        private final IEntryChannel iEntryChannel;
        private final IStoreType iStoreType;
        private final ICategoryProduct iCategoryProduct;
        private final IJsonFileReader iJsonFileReader;
        private final IDepartment iDepartment;
        private final IProvince iProvince;
        private final IDistrict iDistrict;
        private final IStockTransactionType iStockTransactionType;
        private final IWarehouse iWarehouse;
        private final IStockTransaction iStockTransaction;
        private final IModule iModule;
        private final ISubscription iSubscription;
        private final IShipment iShipment;
        private final IPurchase iPurchase;
        private final IOrderState iOrderState;
        private final IPaymentState iPaymentState;
        private final ISaleChannel iSaleChannel;
        private final IManagementType iManagementType;
        private final IPaymentMethod iPaymentMethod;
        private final IOrdering iOrdering;
        private final IOrderStock iOrderStock;

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

                // mock departments peru
                List<LocationDTO> listDepartment = iJsonFileReader.filterDepartment();

                for (LocationDTO locationDepartment : listDepartment) {
                        iDepartment.save(locationDepartment.getDepartment(), "ADMIN1");
                }

                List<LocationDTO> listProvince = iJsonFileReader.filterProvince();

                for (LocationDTO locationProvince : listProvince) {
                        iProvince.save(locationProvince.getProvince(), "ADMIN1", locationProvince.getDepartment());
                }

                List<LocationDTO> listDistrict = iJsonFileReader.filterDistrict();

                for (LocationDTO locationDistrict : listDistrict) {
                        iDistrict.save(locationDistrict.getDistrict(), "ADMIN1", locationDistrict.getProvince());
                }

                // mock modules
                iModule.save("Módulo de Ventas", 3.00, "ADMIN1");
                iModule.save("Módulo de Gestión", 5.00, "ADMIN1");
                iModule.save("Analítica de Ventas", 3.00, "ADMIN1");
                iModule.save("Integracion con Shopify", 5.00, "ADMIN1");
                iModule.save("Módulo de Almacén", 5.00, "ADMIN1");
                iModule.save("Facturación Electronica", 15.00, "ADMIN1");
                iModule.save("Módulo de Remarketing", 8.00, "ADMIN1");
                iModule.save("Integracion con Marketplace", 10.00, "ADMIN1");
                iModule.save("Integracion Tienda Virtual", 10.00, "ADMIN1");
                iModule.save("Modulo de Courier", 5.00, "ADMIN1");
                iModule.save("Modulo de Finanzas", 5.00, "ADMIN1");

                // mock subscriptions
                iSubscription.save("mensual", 1, 0.00, "ADMIN1");
                iSubscription.save("semestral", 6, 10.00, "ADMIN1");
                iSubscription.save("anual", 12, 20.00, "ADMIN1");

                // mock categories
                iCategory.save("Joyas y bisuteria", "Joyas y bisuteria", "admin1");
                iCategory.save("Moda", "Moda", "admin1");
                iCategory.save("Tecnologia", "Tecnologia", "admin1");
                iCategory.save("Cosmeticos", "Cosmeticos", "admin1");
                iCategory.save("Otro", "Otro", "admin1");

                // mock category products
                iCategoryProduct.save("camisetas", "camisetas", "admin1");
                iCategoryProduct.save("jeans", "jeans", "admin1");
                iCategoryProduct.save("tennis", "tennis", "admin1");
                iCategoryProduct.save("botas", "botas", "admin1");
                iCategoryProduct.save("blusas", "blusas", "admin1");

                // mock closing channels
                iClosingChannel.save("whatsapp", "admin1");
                iClosingChannel.save("instagram", "admin1");
                iClosingChannel.save("facebook", "admin1");
                iClosingChannel.save("twitter", "admin1");
                iClosingChannel.save("web", "admin1");
                iClosingChannel.save("marketplace", "admin1");
                iClosingChannel.save("tiktok", "admin1");
                iClosingChannel.save("otro", "admin1");

                // mock entry channels
                iEntryChannel.save("tiktok", "admin1");
                iEntryChannel.save("whatsapp", "admin1");
                iEntryChannel.save("instagram", "admin1");
                iEntryChannel.save("facebook", "admin1");
                iEntryChannel.save("twitter", "admin1");
                iEntryChannel.save("web", "admin1");
                iEntryChannel.save("otro", "admin1");

                // mock store types
                iStoreType.save("shopify", "admin1");
                iStoreType.save("woocommerce", "admin1");
                iStoreType.save("tiendada", "admin1");
                iStoreType.save("ninguna", "admin1");
                iStoreType.save("otro", "admin1");
                iStoreType.save("wix", "admin1");
                iStoreType.save("prestashop", "admin1");

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

                // order state
                iOrderState.save("pendiente","admin1");
                iOrderState.save("entregado", "admin1");
                iOrderState.save("preparado", "admin1");
                iOrderState.save("pendiente de stock","admin1");
                iOrderState.save("pagado","admin1");
                iOrderState.save("reservado", "admin1");
                iOrderState.save("fallido","admin1");
                iOrderState.save("por recoger","admin1");
                iOrderState.save("no hay stock","admin1");
                iOrderState.save("llamar","admin1");
                iOrderState.save("devolucion", "admin1");
                iOrderState.save("agendado","admin1");
                iOrderState.save("en ruta","admin1");
                iOrderState.save("llamado","admin1");

                // payment state
                iPaymentState.save("por recaudar","admin1");
                iPaymentState.save("recaudado","admin1");
                iPaymentState.save("perdida","admin1");

                // sale channel
                iSaleChannel.save("tienda online","admin1");
                iSaleChannel.save("almacen de cadena","admin1");

                // management type
                iManagementType.save("canje","admin1");
                iManagementType.save("venta", "admin1");
                iManagementType.save("reserva","admin1");
                iManagementType.save("cambio","admin1");
                iManagementType.save("preventa","admin1");
                iManagementType.save("recupero","admin1");

                // payment type
                iPaymentMethod.save("yape","admin1");
                iPaymentMethod.save("pos","admin1");
                iPaymentMethod.save("efectivo","admin1");
                iPaymentMethod.save("link","admin1");
                iPaymentMethod.save("cambio","admin1");
                iPaymentMethod.save("plin","admin1");
                iPaymentMethod.save("plataforma mp/web","admin1");
                iPaymentMethod.save("bcp","admin1");
                iPaymentMethod.save("contraentrega","admin1");
                iPaymentMethod.save("canje","admin1");
                iPaymentMethod.save("interbank","admin1");
                iPaymentMethod.save("banco de la nacion","admin1");
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

                // mock products

                RequestProductSave product1 = RequestProductSave.builder().build();
                product1.setCategory("tennis");
                product1.setColor("negro");
                product1.setModel("f90");
                product1.setSize("12");
                product1.setSku("A00001");
                product1.setPrice(2.30);

                iProduct.save(product1, "gjimenez");

                RequestProductSave product2 = RequestProductSave.builder().build();
                product2.setCategory("botas");
                product2.setColor("rojo");
                product2.setModel("m2000");
                product2.setSize("24");
                product2.setSku("A00002");
                product2.setPrice(5.41);

                iProduct.save(product2, "gjimenez");

                RequestProductSave product3 = RequestProductSave.builder().build();
                product3.setCategory("tennis");
                product3.setColor("verde");
                product3.setModel("mercurial");
                product3.setSize("24");
                product3.setSku("A00003");
                product3.setPrice(3.33);

                iProduct.save(product3, "gjimenez");

                RequestProductSave product4 = RequestProductSave.builder().build();
                product4.setCategory("camisetas");
                product4.setColor("rojo");
                product4.setModel("indigo");
                product4.setSize("s");
                product4.setSku("A00004");
                product4.setPrice(7.01);

                iProduct.save(product4, "gjimenez");

                RequestProductSave product5 = RequestProductSave.builder().build();
                product5.setCategory("jeans");
                product5.setColor("azul");
                product5.setModel("old navy");
                product5.setSize("m");
                product5.setSku("A00005");
                product5.setPrice(4.76);

                iProduct.save(product5, "gjimenez");

                RequestProductSave product6 = RequestProductSave.builder().build();
                product6.setCategory("blusas");
                product6.setColor("amarillo");
                product6.setModel("ripper");
                product6.setSize("l");
                product6.setSku("A00006");
                product6.setPrice(1.34);

                iProduct.save(product6, "gjimenez");

                RequestProductSave product7 = RequestProductSave.builder().build();
                product7.setCategory("blusas");
                product7.setColor("morado");
                product7.setModel("sweater");
                product7.setSize("xs");
                product7.setSku("A00007");
                product7.setPrice(8.23);

                iProduct.save(product7, "gjimenez");

                RequestProductSave product8 = RequestProductSave.builder().build();
                product8.setCategory("camisetas");
                product8.setColor("verde");
                product8.setModel("kasper");
                product8.setSize("xm");
                product8.setSku("A00008");
                product8.setPrice(6.27);

                iProduct.save(product8, "gjimenez");

                RequestProductSave product9 = RequestProductSave.builder().build();
                product9.setCategory("blusas");
                product9.setColor("naranja");
                product9.setModel("sustra");
                product9.setSize("xl");
                product9.setSku("A00009");
                product9.setPrice(9.05);

                iProduct.save(product9, "gjimenez");

                RequestProductSave product10 = RequestProductSave.builder().build();
                product10.setCategory("botas");
                product10.setColor("rojo");
                product10.setModel("krust");
                product10.setSize("40");
                product10.setSku("B00001");
                product10.setPrice(7.11);

                iProduct.save(product10, "fcasas");

                RequestProductSave product11 = RequestProductSave.builder().build();
                product11.setCategory("tennis");
                product11.setColor("verde");
                product11.setModel("gist");
                product11.setSize("32");
                product11.setSku("B00002");
                product11.setPrice(4.65);

                iProduct.save(product11, "fcasas");

                RequestProductSave product12 = RequestProductSave.builder().build();
                product12.setCategory("tennis");
                product12.setColor("azul");
                product12.setModel("thunder");
                product12.setSize("18");
                product12.setSku("B00003");
                product12.setPrice(8.38);

                iProduct.save(product12, "fcasas");

                RequestProductSave product13 = RequestProductSave.builder().build();
                product13.setCategory("camisetas");
                product13.setColor("negro");
                product13.setModel("yitro");
                product13.setSize("s");
                product13.setSku("B00004");
                product13.setPrice(4.02);

                iProduct.save(product13, "fcasas");

                RequestProductSave product14 = RequestProductSave.builder().build();
                product14.setCategory("blusas");
                product14.setColor("morado");
                product14.setModel("ulcast");
                product14.setSize("m");
                product14.setSku("B00005");
                product14.setPrice(1.99);

                iProduct.save(product14, "fcasas");

                RequestProductSave product15 = RequestProductSave.builder().build();
                product15.setCategory("jeans");
                product15.setColor("amarillo");
                product15.setModel("reinder");
                product15.setSize("l");
                product15.setSku("B00006");
                product15.setPrice(6.37);

                iProduct.save(product15, "fcasas");

                RequestProductSave product16 = RequestProductSave.builder().build();
                product16.setCategory("camisetas");
                product16.setColor("rojo");
                product16.setModel("realt");
                product16.setSize("xl");
                product16.setSku("B00007");
                product16.setPrice(2.97);

                iProduct.save(product16, "fcasas");

                RequestProductSave product17 = RequestProductSave.builder().build();
                product17.setCategory("blusas");
                product17.setColor("azul");
                product17.setModel("brust");
                product17.setSize("xs");
                product17.setSku("B00008");
                product17.setPrice(5.21);

                iProduct.save(product17, "fcasas");

                RequestProductSave product18 = RequestProductSave.builder().build();
                product18.setCategory("camisetas");
                product18.setColor("naranja");
                product18.setModel("frost");
                product18.setSize("m");
                product18.setSku("B00009");
                product18.setPrice(3.53);

                iProduct.save(product18, "fcasas");

                // mocks suppliers

                RequestSupplier supplier1 = RequestSupplier.builder().build();
                supplier1.setBusinessName("burgenvillia .corp");
                supplier1.setRuc("12345678922");
                supplier1.setCountry("Peru");
                supplier1.setEmail("bg@gmail.com");
                supplier1.setLocation("Lima, Street 123");
                supplier1.setPhoneNumber("323456789");

                iSupplier.save(supplier1, "gjimenez");

                RequestSupplier supplier2 = RequestSupplier.builder().build();
                supplier2.setBusinessName("coltran ltd");
                supplier2.setRuc("12345678924");
                supplier2.setCountry("India");
                supplier2.setEmail("coltran@gmail.com");
                supplier2.setLocation("Mumbai, Av 345");
                supplier2.setPhoneNumber("333456789");

                iSupplier.save(supplier2, "gjimenez");

                RequestSupplier supplier3 = RequestSupplier.builder().build();
                supplier3.setBusinessName("xincheng ptd");
                supplier3.setRuc("12345678925");
                supplier3.setCountry("China");
                supplier3.setEmail("xincheng@gmail.com");
                supplier3.setLocation("Shanghai, st 777");
                supplier3.setPhoneNumber("343456789");

                iSupplier.save(supplier3, "fcasas");

                RequestSupplier supplier4 = RequestSupplier.builder().build();
                supplier4.setBusinessName("tejidos sa");
                supplier4.setRuc("12345678926");
                supplier4.setCountry("España");
                supplier4.setEmail("tejidos@gmail.com");
                supplier4.setLocation("Valencia, tranv 843");
                supplier4.setPhoneNumber("353456789");

                iSupplier.save(supplier4, "fcasas");

                // mock supplier products

                RequestSupplierProduct requestSupplierProduct1 = RequestSupplierProduct.builder()
                                .productSku("A00001")
                                .purchasePrice(5.24)
                                .serial("A00001A")
                                .supplierRuc("12345678922")
                                .build();

                iSupplierProduct.save(requestSupplierProduct1, "gjimenez");

                RequestSupplierProduct requestSupplierProduct2 = RequestSupplierProduct.builder()
                                .productSku("A00001")
                                .purchasePrice(2.10)
                                .serial("A00001B")
                                .supplierRuc("12345678924")
                                .build();

                iSupplierProduct.save(requestSupplierProduct2, "gjimenez");

                RequestSupplierProduct requestSupplierProduct3 = RequestSupplierProduct.builder()
                                .productSku("A00002")
                                .purchasePrice(10.47)
                                .serial("A00002A")
                                .supplierRuc("12345678922")
                                .build();

                iSupplierProduct.save(requestSupplierProduct3, "gjimenez");

                RequestSupplierProduct requestSupplierProduct4 = RequestSupplierProduct.builder()
                                .productSku("A00002")
                                .purchasePrice(13.09)
                                .serial("A00002B")
                                .supplierRuc("12345678924")
                                .build();

                iSupplierProduct.save(requestSupplierProduct4, "gjimenez");

                RequestSupplierProduct requestSupplierProduct5 = RequestSupplierProduct.builder()
                                .productSku("A00003")
                                .purchasePrice(20.15)
                                .serial("A00003A")
                                .supplierRuc("12345678922")
                                .build();

                iSupplierProduct.save(requestSupplierProduct5, "gjimenez");

                RequestSupplierProduct requestSupplierProduct6 = RequestSupplierProduct.builder()
                                .productSku("A00003")
                                .purchasePrice(17.45)
                                .serial("A00003B")
                                .supplierRuc("12345678924")
                                .build();

                iSupplierProduct.save(requestSupplierProduct6, "gjimenez");

                RequestSupplierProduct requestSupplierProduct7 = RequestSupplierProduct.builder()
                                .productSku("A00004")
                                .purchasePrice(23.76)
                                .serial("A00004A")
                                .supplierRuc("12345678922")
                                .build();

                iSupplierProduct.save(requestSupplierProduct7, "gjimenez");

                RequestSupplierProduct requestSupplierProduct8 = RequestSupplierProduct.builder()
                                .productSku("A00004")
                                .purchasePrice(35.02)
                                .serial("A00004B")
                                .supplierRuc("12345678924")
                                .build();

                iSupplierProduct.save(requestSupplierProduct8, "gjimenez");

                RequestSupplierProduct requestSupplierProduct9 = RequestSupplierProduct.builder()
                                .productSku("A00005")
                                .purchasePrice(7.90)
                                .serial("A00005A")
                                .supplierRuc("12345678922")
                                .build();

                iSupplierProduct.save(requestSupplierProduct9, "gjimenez");

                RequestSupplierProduct requestSupplierProduct10 = RequestSupplierProduct.builder()
                                .productSku("A00005")
                                .purchasePrice(3.22)
                                .serial("A00005B")
                                .supplierRuc("12345678924")
                                .build();

                iSupplierProduct.save(requestSupplierProduct10, "gjimenez");

                RequestSupplierProduct requestSupplierProduct11 = RequestSupplierProduct.builder()
                                .productSku("A00006")
                                .purchasePrice(5.34)
                                .serial("A00006A")
                                .supplierRuc("12345678922")
                                .build();

                iSupplierProduct.save(requestSupplierProduct11, "gjimenez");

                RequestSupplierProduct requestSupplierProduct12 = RequestSupplierProduct.builder()
                                .productSku("A00006")
                                .purchasePrice(2.66)
                                .serial("A00006B")
                                .supplierRuc("12345678924")
                                .build();

                iSupplierProduct.save(requestSupplierProduct12, "gjimenez");

                RequestSupplierProduct requestSupplierProduct13 = RequestSupplierProduct.builder()
                                .productSku("A00007")
                                .purchasePrice(4.50)
                                .serial("A00007A")
                                .supplierRuc("12345678922")
                                .build();

                iSupplierProduct.save(requestSupplierProduct13, "gjimenez");

                RequestSupplierProduct requestSupplierProduct14 = RequestSupplierProduct.builder()
                                .productSku("A00007")
                                .purchasePrice(11.37)
                                .serial("A00007B")
                                .supplierRuc("12345678924")
                                .build();

                iSupplierProduct.save(requestSupplierProduct14, "gjimenez");

                RequestSupplierProduct requestSupplierProduct15 = RequestSupplierProduct.builder()
                                .productSku("A00008")
                                .purchasePrice(9.11)
                                .serial("A00008A")
                                .supplierRuc("12345678922")
                                .build();

                iSupplierProduct.save(requestSupplierProduct15, "gjimenez");

                RequestSupplierProduct requestSupplierProduct16 = RequestSupplierProduct.builder()
                                .productSku("A00008")
                                .purchasePrice(2.73)
                                .serial("A00008B")
                                .supplierRuc("12345678924")
                                .build();

                iSupplierProduct.save(requestSupplierProduct16, "gjimenez");

                RequestSupplierProduct requestSupplierProduct17 = RequestSupplierProduct.builder()
                                .productSku("A00009")
                                .purchasePrice(6.41)
                                .serial("A00009A")
                                .supplierRuc("12345678922")
                                .build();

                iSupplierProduct.save(requestSupplierProduct17, "gjimenez");

                RequestSupplierProduct requestSupplierProduct18 = RequestSupplierProduct.builder()
                                .productSku("A00009")
                                .purchasePrice(12.30)
                                .serial("A00009B")
                                .supplierRuc("12345678924")
                                .build();

                iSupplierProduct.save(requestSupplierProduct18, "gjimenez");

                RequestSupplierProduct requestSupplierProduct19 = RequestSupplierProduct.builder()
                                .productSku("B00001")
                                .purchasePrice(3.01)
                                .serial("B00001A")
                                .supplierRuc("12345678925")
                                .build();

                iSupplierProduct.save(requestSupplierProduct19, "gjimenez");

                RequestSupplierProduct requestSupplierProduct20 = RequestSupplierProduct.builder()
                                .productSku("B00001")
                                .purchasePrice(1.05)
                                .serial("B00001B")
                                .supplierRuc("12345678926")
                                .build();

                iSupplierProduct.save(requestSupplierProduct20, "gjimenez");

                RequestSupplierProduct requestSupplierProduct21 = RequestSupplierProduct.builder()
                                .productSku("B00002")
                                .purchasePrice(7.20)
                                .serial("B00002A")
                                .supplierRuc("12345678925")
                                .build();

                iSupplierProduct.save(requestSupplierProduct21, "gjimenez");

                RequestSupplierProduct requestSupplierProduct22 = RequestSupplierProduct.builder()
                                .productSku("B00002")
                                .purchasePrice(5.68)
                                .serial("B00002B")
                                .supplierRuc("12345678926")
                                .build();

                iSupplierProduct.save(requestSupplierProduct22, "gjimenez");

                RequestSupplierProduct requestSupplierProduct23 = RequestSupplierProduct.builder()
                                .productSku("B00003")
                                .purchasePrice(36.49)
                                .serial("B00003A")
                                .supplierRuc("12345678925")
                                .build();

                iSupplierProduct.save(requestSupplierProduct23, "gjimenez");

                RequestSupplierProduct requestSupplierProduct24 = RequestSupplierProduct.builder()
                                .productSku("B00003")
                                .purchasePrice(45.27)
                                .serial("B00003B")
                                .supplierRuc("12345678926")
                                .build();

                iSupplierProduct.save(requestSupplierProduct24, "gjimenez");

                RequestSupplierProduct requestSupplierProduct25 = RequestSupplierProduct.builder()
                                .productSku("B00004")
                                .purchasePrice(22.38)
                                .serial("B00004A")
                                .supplierRuc("12345678925")
                                .build();

                iSupplierProduct.save(requestSupplierProduct25, "gjimenez");

                RequestSupplierProduct requestSupplierProduct26 = RequestSupplierProduct.builder()
                                .productSku("B00004")
                                .purchasePrice(15.07)
                                .serial("B00004B")
                                .supplierRuc("12345678926")
                                .build();

                iSupplierProduct.save(requestSupplierProduct26, "gjimenez");

                RequestSupplierProduct requestSupplierProduct27 = RequestSupplierProduct.builder()
                                .productSku("B00005")
                                .purchasePrice(73.02)
                                .serial("B00005A")
                                .supplierRuc("12345678925")
                                .build();

                iSupplierProduct.save(requestSupplierProduct27, "gjimenez");

                RequestSupplierProduct requestSupplierProduct28 = RequestSupplierProduct.builder()
                                .productSku("B00005")
                                .purchasePrice(82.17)
                                .serial("B00005B")
                                .supplierRuc("12345678926")
                                .build();

                iSupplierProduct.save(requestSupplierProduct28, "gjimenez");

                RequestSupplierProduct requestSupplierProduct29 = RequestSupplierProduct.builder()
                                .productSku("B00006")
                                .purchasePrice(13.77)
                                .serial("B00006A")
                                .supplierRuc("12345678925")
                                .build();

                iSupplierProduct.save(requestSupplierProduct29, "gjimenez");

                RequestSupplierProduct requestSupplierProduct30 = RequestSupplierProduct.builder()
                                .productSku("B00006")
                                .purchasePrice(24.93)
                                .serial("B00006B")
                                .supplierRuc("12345678926")
                                .build();

                iSupplierProduct.save(requestSupplierProduct30, "gjimenez");

                RequestSupplierProduct requestSupplierProduct31 = RequestSupplierProduct.builder()
                                .productSku("B00007")
                                .purchasePrice(64.57)
                                .serial("B00007A")
                                .supplierRuc("12345678925")
                                .build();

                iSupplierProduct.save(requestSupplierProduct31, "gjimenez");

                RequestSupplierProduct requestSupplierProduct32 = RequestSupplierProduct.builder()
                                .productSku("B00007")
                                .purchasePrice(23.89)
                                .serial("B00007B")
                                .supplierRuc("12345678926")
                                .build();

                iSupplierProduct.save(requestSupplierProduct32, "gjimenez");

                RequestSupplierProduct requestSupplierProduct33 = RequestSupplierProduct.builder()
                                .productSku("B00008")
                                .purchasePrice(17.94)
                                .serial("B00008A")
                                .supplierRuc("12345678925")
                                .build();

                iSupplierProduct.save(requestSupplierProduct33, "gjimenez");

                RequestSupplierProduct requestSupplierProduct34 = RequestSupplierProduct.builder()
                                .productSku("B00008")
                                .purchasePrice(33.29)
                                .serial("B00008B")
                                .supplierRuc("12345678926")
                                .build();

                iSupplierProduct.save(requestSupplierProduct34, "gjimenez");

                RequestSupplierProduct requestSupplierProduct35 = RequestSupplierProduct.builder()
                                .productSku("B00009")
                                .purchasePrice(95.22)
                                .serial("B00009A")
                                .supplierRuc("12345678925")
                                .build();

                iSupplierProduct.save(requestSupplierProduct35, "gjimenez");

                RequestSupplierProduct requestSupplierProduct36 = RequestSupplierProduct.builder()
                                .productSku("B00009")
                                .purchasePrice(83.19)
                                .serial("B00009B")
                                .supplierRuc("12345678926")
                                .build();

                iSupplierProduct.save(requestSupplierProduct36, "gjimenez");

                // stock transaction types mocks
                iStockTransactionType.save("entrada", "admin1");
                iStockTransactionType.save("salida", "admin1");
                iStockTransactionType.save("transferencia", "admin1");

                // warehouse mocks
                RequestWarehouse warehouse1 = RequestWarehouse.builder()
                                .location("Cusco calle 123")
                                .name("luminous")
                                .build();

                iWarehouse.save(warehouse1, "gjimenez");

                RequestWarehouse warehouse2 = RequestWarehouse.builder()
                                .location("Lima Avenida 234")
                                .name("oikas")
                                .build();

                iWarehouse.save(warehouse2, "gjimenez");

                RequestWarehouse warehouse3 = RequestWarehouse.builder()
                                .location("Arequipa Calle 765")
                                .name("villalobos")
                                .build();

                iWarehouse.save(warehouse3, "fcasas");

                RequestWarehouse warehouse4 = RequestWarehouse.builder()
                                .location("Nazca calle 89")
                                .name("alcazar")
                                .build();

                iWarehouse.save(warehouse4, "fcasas");

                // purchase mocks

                List<RequestPurchase> requestPurchaseList1 = new ArrayList<>();

                RequestPurchase requestPurchase1 = RequestPurchase.builder()
                                .quantity(15)
                                .supplierProductSerial("A00001A")
                                .unitPrice(3.40)
                                .build();

                requestPurchaseList1.add(requestPurchase1);

                RequestPurchase requestPurchase2 = RequestPurchase.builder()
                                .quantity(4)
                                .supplierProductSerial("A00001B")
                                .unitPrice(7.8)
                                .build();

                requestPurchaseList1.add(requestPurchase2);

                RequestPurchase requestPurchase3 = RequestPurchase.builder()
                                .quantity(20)
                                .supplierProductSerial("A00002A")
                                .unitPrice(12.45)
                                .build();

                requestPurchaseList1.add(requestPurchase3);

                RequestPurchase requestPurchase4 = RequestPurchase.builder()
                                .quantity(25)
                                .supplierProductSerial("A00002B")
                                .unitPrice(20.03)
                                .build();

                requestPurchaseList1.add(requestPurchase4);

                RequestPurchase requestPurchase5 = RequestPurchase.builder()
                                .quantity(7)
                                .supplierProductSerial("A00003A")
                                .unitPrice(14.76)
                                .build();

                requestPurchaseList1.add(requestPurchase5);

                RequestPurchase requestPurchase6 = RequestPurchase.builder()
                                .quantity(15)
                                .supplierProductSerial("A00003B")
                                .unitPrice(2.91)
                                .build();

                requestPurchaseList1.add(requestPurchase6);

                List<RequestPurchase> requestPurchaseList2 = new ArrayList<>();

                RequestPurchase requestPurchase7 = RequestPurchase.builder()
                                .quantity(9)
                                .supplierProductSerial("B00001A")
                                .unitPrice(23.77)
                                .build();

                requestPurchaseList2.add(requestPurchase7);

                RequestPurchase requestPurchase8 = RequestPurchase.builder()
                                .quantity(15)
                                .supplierProductSerial("B00001B")
                                .unitPrice(13.91)
                                .build();

                requestPurchaseList2.add(requestPurchase8);

                RequestPurchase requestPurchase9 = RequestPurchase.builder()
                                .quantity(36)
                                .supplierProductSerial("B00002A")
                                .unitPrice(5.19)
                                .build();

                requestPurchaseList2.add(requestPurchase9);

                RequestPurchase requestPurchase10 = RequestPurchase.builder()
                                .quantity(13)
                                .supplierProductSerial("B00002B")
                                .unitPrice(2.72)
                                .build();

                requestPurchaseList2.add(requestPurchase10);

                RequestPurchase requestPurchase11 = RequestPurchase.builder()
                                .quantity(20)
                                .supplierProductSerial("B00003B")
                                .unitPrice(17.40)
                                .build();

                requestPurchaseList2.add(requestPurchase11);

                RequestPurchase requestPurchase12 = RequestPurchase.builder()
                                .quantity(27)
                                .supplierProductSerial("B00003A")
                                .unitPrice(10.99)
                                .build();

                requestPurchaseList2.add(requestPurchase12);

                iPurchase.save("AA00001", requestPurchaseList1, "gjimenez");
                iPurchase.save("BB00001", requestPurchaseList2, "fcasas");

                // shipments

                List<RequestShipment> requestShipmentList1 = new ArrayList<>();

                RequestShipment requestShipment1 = RequestShipment.builder()
                                .observations("no aplica")
                                .purchaseSerial("AA00001")
                                .quantity(15)
                                .supplierProductSerial("A00001A")
                                .build();

                requestShipmentList1.add(requestShipment1);

                RequestShipment requestShipment2 = RequestShipment.builder()
                                .observations("no aplica")
                                .purchaseSerial("AA00001")
                                .quantity(4)
                                .supplierProductSerial("A00001B")
                                .build();

                requestShipmentList1.add(requestShipment2);

                RequestShipment requestShipment3 = RequestShipment.builder()
                                .observations("no aplica")
                                .purchaseSerial("AA00001")
                                .quantity(20)
                                .supplierProductSerial("A00002A")
                                .build();

                requestShipmentList1.add(requestShipment3);

                RequestShipment requestShipment4 = RequestShipment.builder()
                                .observations("no aplica")
                                .purchaseSerial("AA00001")
                                .quantity(25)
                                .supplierProductSerial("A00002B")
                                .build();

                requestShipmentList1.add(requestShipment4);

                RequestShipment requestShipment5 = RequestShipment.builder()
                                .observations("no aplica")
                                .purchaseSerial("AA00001")
                                .quantity(7)
                                .supplierProductSerial("A00003A")
                                .build();

                requestShipmentList1.add(requestShipment5);

                RequestShipment requestShipment6 = RequestShipment.builder()
                                .observations("no aplica")
                                .purchaseSerial("AA00001")
                                .quantity(15)
                                .supplierProductSerial("A00003B")
                                .build();

                requestShipmentList1.add(requestShipment6);

                List<RequestShipment> requestShipmentList2 = new ArrayList<>();

                RequestShipment requestShipment7 = RequestShipment.builder()
                                .observations("no aplica")
                                .purchaseSerial("BB00001")
                                .quantity(9)
                                .supplierProductSerial("B00001A")
                                .build();

                requestShipmentList2.add(requestShipment7);

                RequestShipment requestShipment8 = RequestShipment.builder()
                                .observations("no aplica")
                                .purchaseSerial("BB00001")
                                .quantity(15)
                                .supplierProductSerial("B00001B")
                                .build();

                requestShipmentList2.add(requestShipment8);

                RequestShipment requestShipment9 = RequestShipment.builder()
                                .observations("no aplica")
                                .purchaseSerial("BB00001")
                                .quantity(36)
                                .supplierProductSerial("B00002A")
                                .build();

                requestShipmentList2.add(requestShipment9);

                RequestShipment requestShipment10 = RequestShipment.builder()
                                .observations("no aplica")
                                .purchaseSerial("BB00001")
                                .quantity(13)
                                .supplierProductSerial("B00002B")
                                .build();

                requestShipmentList2.add(requestShipment10);

                RequestShipment requestShipment11 = RequestShipment.builder()
                                .observations("no aplica")
                                .purchaseSerial("BB00001")
                                .quantity(20)
                                .supplierProductSerial("B00003A")
                                .build();

                requestShipmentList2.add(requestShipment11);

                RequestShipment requestShipment12 = RequestShipment.builder()
                                .observations("no aplica")
                                .purchaseSerial("BB00001")
                                .quantity(27)
                                .supplierProductSerial("B00003B")
                                .build();

                requestShipmentList2.add(requestShipment12);

                iShipment.save("SA00001", "luminous", requestShipmentList1, "gjimenez");
                iShipment.save("SB00001", "alcazar", requestShipmentList2, "fcasas");

                // orders mocks

                RequestItem requestItem1 = RequestItem.builder()
                        .productSku("A00001")
                        .discount(0.00)
                        .quantity(2)
                        .observations("")
                        .build();

                RequestItem requestItem2 = RequestItem.builder()
                        .quantity(1)
                        .discount(3.00)
                        .productSku("A00002")
                        .observations("")
                        .build();

                ArrayList<RequestItem> requestItems1 = new ArrayList<>();

                requestItems1.add(requestItem1);
                requestItems1.add(requestItem2);

                RequestOrderSave requestOrderSave1 = RequestOrderSave.builder()
                        .advancedPayment(0.00)
                        .customerAddress("807 IQUIQUE")
                        .customerDepartment("LIMA")
                        .customerProvince("LIMA")
                        .customerDistrict("BREÑA")
                        .customerName("Emilio Gomez")
                        .customerPhone("940544828")
                        .customerReference("")
                        .customerType("Tradicional")
                        .deliveryAddress("807 IQUIQUE")
                        .deliveryAmount(0.00)
                        .managementType("venta")
                        .instagram("")
                        .observations("")
                        .paymentMethod("yape")
                        .paymentReceipt("")
                        .saleChannel("tienda online")
                        .requestItems(requestItems1)
                        .build();

                iOrdering.save(requestOrderSave1,"gjimenez");

                RequestItem requestItem3 = RequestItem.builder()
                        .productSku("A00003")
                        .quantity(3)
                        .discount(0.00)
                        .observations("")
                        .build();

                RequestItem requestItem4 = RequestItem.builder()
                        .productSku("A00001")
                        .quantity(1)
                        .discount(2.00)
                        .observations("")
                        .build();

                ArrayList<RequestItem> requestItems2 = new ArrayList<>();

                requestItems2.add(requestItem3);
                requestItems2.add(requestItem4);

                RequestOrderSave requestOrderSave2 = RequestOrderSave.builder()
                        .advancedPayment(4.00)
                        .customerAddress("AV. JORGE CHAVEZ 420, OFICN LIMA")
                        .customerDepartment("LIMA")
                        .customerProvince("LIMA")
                        .customerDistrict("INDEPENDENCIA")
                        .customerName("Consuelo Rojas")
                        .customerPhone("956701333")
                        .customerReference("")
                        .customerType("Tradicional")
                        .deliveryAddress("AV. JORGE CHAVEZ 420, OFICN LIMA")
                        .deliveryAmount(3.00)
                        .managementType("venta")
                        .instagram("")
                        .observations("")
                        .paymentMethod("plin")
                        .paymentReceipt("")
                        .saleChannel("tienda online")
                        .requestItems(requestItems2)
                        .build();

                iOrdering.save(requestOrderSave2,"gjimenez");

                RequestItem requestItem5 = RequestItem.builder()
                        .productSku("B00001")
                        .discount(0.00)
                        .quantity(1)
                        .observations("")
                        .build();

                RequestItem requestItem6 = RequestItem.builder()
                        .quantity(3)
                        .discount(5.00)
                        .productSku("B00002")
                        .observations("")
                        .build();

                List<RequestItem> requestItems3 = new ArrayList<>();

                requestItems3.add(requestItem5);
                requestItems3.add(requestItem6);

                RequestOrderSave requestOrderSave3 = RequestOrderSave.builder()
                        .advancedPayment(0.00)
                        .customerAddress("AV MARAÑÓN 776")
                        .customerDepartment("LIMA")
                        .customerProvince("LIMA")
                        .customerDistrict("ATE")
                        .customerName("Ulises Trujillo")
                        .customerPhone("944214925")
                        .customerReference("")
                        .customerType("Tradicional")
                        .deliveryAddress("AV MARAÑÓN 776")
                        .deliveryAmount(6.25)
                        .managementType("venta")
                        .instagram("")
                        .observations("")
                        .paymentMethod("efectivo")
                        .paymentReceipt("")
                        .saleChannel("tienda online")
                        .requestItems(requestItems3)
                        .build();

                iOrdering.save(requestOrderSave3,"fcasas");

                RequestItem requestItem7 = RequestItem.builder()
                        .productSku("B00002")
                        .discount(7.00)
                        .quantity(5)
                        .observations("")
                        .build();

                RequestItem requestItem8 = RequestItem.builder()
                        .quantity(2)
                        .discount(0.00)
                        .productSku("B00003")
                        .observations("")
                        .build();

                List<RequestItem> requestItems4 = new ArrayList<>();

                requestItems4.add(requestItem7);
                requestItems4.add(requestItem8);

                RequestOrderSave requestOrderSave4 = RequestOrderSave.builder()
                        .advancedPayment(0.00)
                        .customerAddress("URB. CAPILLA 130")
                        .customerDepartment("LIMA")
                        .customerProvince("LIMA")
                        .customerDistrict("CHORRILLOS")
                        .customerName("Roberto Padilla")
                        .customerPhone("989538516")
                        .customerReference("")
                        .customerType("Tradicional")
                        .deliveryAddress("URB. CAPILLA 130")
                        .deliveryAmount(10.15)
                        .managementType("venta")
                        .instagram("")
                        .observations("URB. LA CAPILLA 130, CALLE SARAGOZA- LA MOLINA")
                        .paymentMethod("efectivo")
                        .paymentReceipt("")
                        .saleChannel("tienda online")
                        .requestItems(requestItems4)
                        .build();

                iOrdering.save(requestOrderSave4,"fcasas");

                // order stock mocks

                RequestOrderStock requestOrderStock1 = RequestOrderStock.builder()
                        .supplierProductSerial("A00001A")
                        .quantity(2)
                        .warehouse("LUMINOUS")
                        .itemId(1L)
                        .build();

                RequestOrderStock requestOrderStock2 = RequestOrderStock.builder()
                        .supplierProductSerial("A00002A")
                        .quantity(1)
                        .warehouse("LUMINOUS")
                        .itemId(2L)
                        .build();

                List<RequestOrderStock> requestOrderStockList1 = new ArrayList<>();
                requestOrderStockList1.add(requestOrderStock1);
                requestOrderStockList1.add(requestOrderStock2);

                iOrderStock.save(1L,requestOrderStockList1,"gjimenez");

                RequestOrderStock requestOrderStock3 = RequestOrderStock.builder()
                        .supplierProductSerial("A00003A")
                        .quantity(2)
                        .warehouse("LUMINOUS")
                        .itemId(3L)
                        .build();

                RequestOrderStock requestOrderStock4 = RequestOrderStock.builder()
                        .supplierProductSerial("A00002A")
                        .quantity(1)
                        .warehouse("LUMINOUS")
                        .itemId(4L)
                        .build();

                List<RequestOrderStock> requestOrderStockList2 = new ArrayList<>();
                requestOrderStockList2.add(requestOrderStock3);
                requestOrderStockList2.add(requestOrderStock4);

                iOrderStock.save(2L,requestOrderStockList2,"gjimenez");

                RequestOrderStock requestOrderStock5 = RequestOrderStock.builder()
                        .supplierProductSerial("B00001A")
                        .quantity(1)
                        .warehouse("ALCAZAR")
                        .itemId(5L)
                        .build();

                RequestOrderStock requestOrderStock6 = RequestOrderStock.builder()
                        .supplierProductSerial("B00002A")
                        .quantity(3)
                        .warehouse("ALCAZAR")
                        .itemId(6L)
                        .build();

                List<RequestOrderStock> requestOrderStockList3 = new ArrayList<>();
                requestOrderStockList3.add(requestOrderStock5);
                requestOrderStockList3.add(requestOrderStock6);

                iOrderStock.save(3L,requestOrderStockList3,"fcasas");

                RequestOrderStock requestOrderStock7 = RequestOrderStock.builder()
                        .supplierProductSerial("B00002A")
                        .quantity(5)
                        .warehouse("ALCAZAR")
                        .itemId(7L)
                        .build();

                RequestOrderStock requestOrderStock8 = RequestOrderStock.builder()
                        .supplierProductSerial("B00003A")
                        .quantity(2)
                        .warehouse("ALCAZAR")
                        .itemId(8L)
                        .build();

                List<RequestOrderStock> requestOrderStockList4 = new ArrayList<>();
                requestOrderStockList4.add(requestOrderStock7);
                requestOrderStockList4.add(requestOrderStock8);

                iOrderStock.save(4L,requestOrderStockList4,"fcasas");
        }

}
