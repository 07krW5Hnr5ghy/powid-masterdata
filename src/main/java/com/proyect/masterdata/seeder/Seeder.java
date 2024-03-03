package com.proyect.masterdata.seeder;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.LocationDTO;
import com.proyect.masterdata.dto.request.*;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.*;
import lombok.RequiredArgsConstructor;
import org.springframework.boot.CommandLineRunner;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Component;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.FileInputStream;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

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
        private final IModule iModule;
        private final ISubscription iSubscription;
        private final IShipment iShipment;
        private final IPurchase iPurchase;
        private final IOrderState iOrderState;
        private final IOrderPaymentState iOrderPaymentState;
        private final ISaleChannel iSaleChannel;
        private final IManagementType iManagementType;
        private final IOrderPaymentMethod iOrderPaymentMethod;
        private final IOrdering iOrdering;
        private final IOrderStock iOrderStock;
        private final IUnitType iUnitType;
        private final IUnit iUnit;
        private final ICourier iCourier;
        private final ICancellationReason iCancellationReason;
        private final ICancelledOrder iCancelledOrder;
        private final IStockReturn iStockReturn;
        private final IShipmentType iShipmentType;
        private final IStockReplenishment iStockReplenishment;
        private final IStockTransfer iStockTransfer;
        private final IOrderItem iOrderItem;
        private final IPaymentGateway iPaymentGateway;
        private final IMembershipState iMembershipState;
        private final IAccess iAccess;
        private final IRole iRole;
        private final IRoleAccess iRoleAccess;
        @Override
        public void run(String... args) throws Exception {

                try{
                        // example one role and one access

                        Access access = accessRepository
                                .save(new Access(1L, "USER_GET", true, new Date(System.currentTimeMillis()),
                                        new Date(System.currentTimeMillis()), "SISTEMA"));

                        Role role = roleRepository.save(new Role(
                                1L, "ADMINISTRATION", true, new Date(System.currentTimeMillis()),
                                new Date(System.currentTimeMillis()), "SISTEMA"));

                        // department, province and district to create system user

                        Department department = departmentRepository
                                .save(new Department(1L, "SISTEMA", true, new Date(System.currentTimeMillis()),
                                        "SISTEMA"));

                        Province province = provinceRepository.save(new Province(1L, "SISTEMA", true,
                                new Date(System.currentTimeMillis()), department.getId(), "SISTEMA", department));

                        District district = districtRepository
                                .save(new District(1L, "SISTEMA", true, new Date(System.currentTimeMillis()),
                                        province.getId(),
                                        province, "SISTEMA"));

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

                        // access
                        iAccess.save("ACCESS_POST","admin1");
                        iAccess.save("BRAND_POST","admin1");
                        iAccess.save("BRAND_DELETE","admin1");
                        iAccess.save("BRAND_GET","admin1");
                        iAccess.save("CANCELLATION_REASON_POST","admin1");
                        iAccess.save("CANCELLATION_REASON_GET","admin1");
                        iAccess.save("CANCELLED_ORDER_POST","admin1");
                        iAccess.save("CANCELLED_ORDER_GET","admin1");
                        iAccess.save("CATEGORY_POST","admin1");
                        iAccess.save("CATEGORY_PUT","admin1");
                        iAccess.save("CATEGORY_DELETE","admin1");
                        iAccess.save("CATEGORY_PRODUCT_POST","admin1");
                        iAccess.save("CATEGORY_PRODUCT_GET","admin1");
                        iAccess.save("CLIENT_GET","admin1");
                        iAccess.save("CLIENT_POST","admin1");
                        iAccess.save("CLIENT_PUT","admin1");
                        iAccess.save("CLIENT_DELETE","admin1");
                        iAccess.save("CLOSING_CHANNEL_POST","admin1");
                        iAccess.save("COLOR_POST","admin1");
                        iAccess.save("COLOR_GET","admin1");
                        iAccess.save("COLOR_DELETE","admin1");
                        iAccess.save("COURIER_POST","admin1");
                        iAccess.save("COURIER_PUT","admin1");
                        iAccess.save("COURIER_GET","admin1");
                        iAccess.save("DEPARTMENT_POST","admin1");
                        iAccess.save("DEPARTMENT_DELETE","admin1");
                        iAccess.save("DISTRICT_POST","admin1");
                        iAccess.save("DISTRICT_DELETE","admin1");
                        iAccess.save("ENTRY_CHANNEL_POST","admin1");
                        iAccess.save("GENERAL_STOCK_GET","admin1");
                        iAccess.save("MEMBERSHIP_GET","admin1");
                        iAccess.save("MODEL_GET","admin1");
                        iAccess.save("MODEL_POST","admin1");
                        iAccess.save("MODEL_DELETE","admin1");
                        iAccess.save("MODULE_GET","admin1");
                        iAccess.save("MODULE_POST","admin1");
                        iAccess.save("MODULE_PUT","admin1");
                        iAccess.save("ONBOARD_GET","admin1");
                        iAccess.save("ORDER_GET","admin1");
                        iAccess.save("ORDER_POST","admin1");
                        iAccess.save("ORDER_PUT","admin1");
                        iAccess.save("ORDER_ITEM_GET","admin1");
                        iAccess.save("ORDER_ITEM_POST","admin1");
                        iAccess.save("ORDER_ITEM_DELETE","admin1");
                        iAccess.save("ORDER_ITEM_PUT","admin1");
                        iAccess.save("ORDER_STOCK_GET","admin1");
                        iAccess.save("ORDER_STOCK_POST","admin1");
                        iAccess.save("ORDER_STOCK_ITEM_GET","admin1");
                        iAccess.save("PAYMENT_GATEWAY_POST","admin1");
                        iAccess.save("ORDER_PAYMENT_METHOD_POST","admin1");
                        iAccess.save("ORDER_PAYMENT_METHOD_GET","admin1");
                        iAccess.save("ORDER_PAYMENT_METHOD_DELETE","admin1");
                        iAccess.save("ORDER_PAYMENT_STATE_GET","admin1");
                        iAccess.save("ORDER_PAYMENT_STATE_POST","admin1");
                        iAccess.save("ORDER_PAYMENT_STATE_DELETE","admin1");
                        iAccess.save("PRODUCT_GET","admin1");
                        iAccess.save("PRODUCT_POST","admin1");
                        iAccess.save("PRODUCT_DELETE","admin1");
                        iAccess.save("PRODUCT_PRICE_POST","admin1");
                        iAccess.save("PROVINCE_POST","admin1");
                        iAccess.save("PROVINCE_DELETE","admin1");
                        iAccess.save("PURCHASE_GET","admin1");
                        iAccess.save("PURCHASE_POST","admin1");
                        iAccess.save("PURCHASE_ITEM_GET","admin1");
                        iAccess.save("ROLE_POST","admin1");
                        iAccess.save("ROLE_GET","admin1");
                        iAccess.save("ROLE_DELETE","admin1");
                        iAccess.save("ROLE_PUT","admin1");
                        iAccess.save("SALE_CHANNEL_POST","admin1");
                        iAccess.save("SALE_CHANNEL_DELETE","admin1");
                        iAccess.save("SHIPMENT_GET","admin1");
                        iAccess.save("SHIPMENT_POST","admin1");
                        iAccess.save("SHIPMENT_ITEM_GET","admin1");
                        iAccess.save("SHIPMENT_TYPE_POST","admin1");
                        iAccess.save("SHIPMENT_TYPE_GET","admin1");
                        iAccess.save("SIZE_GET","admin1");
                        iAccess.save("SIZE_POST","admin1");
                        iAccess.save("SIZE_DELETE","admin1");
                        iAccess.save("SIZE_TYPE_GET","admin1");
                        iAccess.save("SIZE_TYPE_POST","admin1");
                        iAccess.save("SIZE_TYPE_DELETE","admin1");
                        iAccess.save("ORDER_STATE_GET","admin1");
                        iAccess.save("ORDER_STATE_POST","admin1");
                        iAccess.save("ORDER_STATE_DELETE","admin1");
                        iAccess.save("STOCK_REPLENISHMENT_POST","admin1");
                        iAccess.save("STOCK_REPLENISHMENT_GET","admin1");
                        iAccess.save("STOCK_REPLENISHMENT_ITEM_GET","admin1");
                        iAccess.save("STOCK_RETURN_POST","admin1");
                        iAccess.save("STOCK_RETURN_GET","admin1");
                        iAccess.save("STOCK_RETURN_ITEM_GET","admin1");
                        iAccess.save("STOCK_TRANSACTION_GET","admin1");
                        iAccess.save("STOCK_TRANSACTION_ITEM_GET","admin1");
                        iAccess.save("STOCK_TRANSACTION_TYPE_POST","admin1");
                        iAccess.save("STOCK_TRANSACTION_TYPE_DELETE","admin1");
                        iAccess.save("STOCK_TRANSFER_POST","admin1");
                        iAccess.save("STOCK_TRANSFER_GET","admin1");
                        iAccess.save("STOCK_TRANSFER_ITEM_GET","admin1");
                        iAccess.save("STORE_GET","admin1");
                        iAccess.save("STORE_POST","admin1");
                        iAccess.save("STORE_DELETE","admin1");
                        iAccess.save("STORE_TYPE_POST","admin1");
                        iAccess.save("SUBSCRIPTION_POST","admin1");
                        iAccess.save("SUPPLIER_GET","admin1");
                        iAccess.save("SUPPLIER_POST","admin1");
                        iAccess.save("SUPPLIER_DELETE","admin1");
                        iAccess.save("SUPPLIER_PRODUCT_GET","admin1");
                        iAccess.save("SUPPLIER_PRODUCT_POST","admin1");
                        iAccess.save("SUPPLIER_PRODUCT_DELETE","admin1");
                        iAccess.save("UNIT_GET","admin1");
                        iAccess.save("UNIT_POST","admin1");
                        iAccess.save("UNIT_DELETE","admin1");
                        iAccess.save("UNIT_TYPE_GET","admin1");
                        iAccess.save("UNIT_TYPE_POST","admin1");
                        iAccess.save("UNIT_TYPE_DELETE","admin1");
                        iAccess.save("USER_POST","admin1");
                        iAccess.save("USER_DELETE","admin1");
                        iAccess.save("USER_ROLE_POST","admin1");
                        iAccess.save("WAREHOUSE_GET","admin1");
                        iAccess.save("WAREHOUSE_POST","admin1");
                        iAccess.save("WAREHOUSE_STOCK_GET","admin1");
                        // roles
                        iRole.save("BUSINESS","ADMIN1");
                        iRole.save("SALES","admin1");
                        iRole.save("STOCK","admin1");
                        iRole.save("CUSTOMER_SERVICE","admin1");
                        iRole.save("COURIER","ADMIN1");
                        iRole.save("MARKETING","ADMIN1");
                        // roles by access
                        iRoleAccess.save("SALES","BRAND_GET","ADMIN1");
                        iRoleAccess.save("SALES","CANCELLATION_REASON_GET","ADMIN1");
                        iRoleAccess.save("SALES","CANCELLED_ORDER_GET","ADMIN1");
                        iRoleAccess.save("SALES","CANCELLED_ORDER_POST","ADMIN1");
                        iRoleAccess.save("SALES","COURIER_GET","ADMIN1");
                        iRoleAccess.save("SALES","MODEL_GET","ADMIN1");
                        iRoleAccess.save("SALES","ORDER_GET","ADMIN1");
                        iRoleAccess.save("SALES","ORDER_POST","ADMIN1");
                        iRoleAccess.save("SALES","ORDER_PUT","ADMIN1");
                        iRoleAccess.save("SALES","ORDER_ITEM_GET","ADMIN1");
                        iRoleAccess.save("SALES","ORDER_ITEM_POST","ADMIN1");
                        iRoleAccess.save("SALES","ORDER_ITEM_PUT","ADMIN1");
                        iRoleAccess.save("SALES","ORDER_ITEM_DELETE","ADMIN1");
                        iRoleAccess.save("SALES","ORDER_PAYMENT_METHOD_GET","ADMIN1");
                        iRoleAccess.save("SALES","ORDER_PAYMENT_STATE_GET","ADMIN1");
                        iRoleAccess.save("SALES","ORDER_STATE","ADMIN1");
                        iRoleAccess.save("SALES","PRODUCT_GET","ADMIN1");
                        iRoleAccess.save("CUSTOMER_SERVICE","BRAND_GET","ADMIN1");
                        iRoleAccess.save("CUSTOMER_SERVICE","CANCELLATION_REASON_GET","ADMIN1");
                        iRoleAccess.save("CUSTOMER_SERVICE","CANCELLED_ORDER_GET","ADMIN1");
                        iRoleAccess.save("CUSTOMER_SERVICE","CANCELLED_ORDER_POST","ADMIN1");
                        iRoleAccess.save("CUSTOMER_SERVICE","MODEL_GET","ADMIN1");
                        iRoleAccess.save("CUSTOMER_SERVICE","COURIER_GET","ADMIN1");
                        iRoleAccess.save("CUSTOMER_SERVICE","ORDER_GET","ADMIN1");
                        iRoleAccess.save("CUSTOMER_SERVICE","ORDER_PUT","ADMIN1");
                        iRoleAccess.save("CUSTOMER_SERVICE","ORDER_ITEM_GET","ADMIN1");
                        iRoleAccess.save("CUSTOMER_SERVICE","ORDER_ITEM_POST","ADMIN1");
                        iRoleAccess.save("CUSTOMER_SERVICE","ORDER_ITEM_PUT","ADMIN1");
                        iRoleAccess.save("CUSTOMER_SERVICE","ORDER_ITEM_DELETE","ADMIN1");
                        iRoleAccess.save("CUSTOMER_SERVICE","ORDER_PAYMENT_METHOD_GET","ADMIN1");
                        iRoleAccess.save("CUSTOMER_SERVICE","ORDER_PAYMENT_STATE_GET","ADMIN1");
                        iRoleAccess.save("CUSTOMER_SERVICE","ORDER_STATE","ADMIN1");
                        iRoleAccess.save("CUSTOMER_SERVICE","PRODUCT_GET","ADMIN1");
                        iRoleAccess.save("MARKETING","BRAND_GET","ADMIN1");
                        iRoleAccess.save("MARKETING","BRAND_POST","ADMIN1");
                        iRoleAccess.save("MARKETING","BRAND_DELETE","ADMIN1");
                        iRoleAccess.save("MARKETING","CATEGORY_PRODUCT_GET","ADMIN1");
                        iRoleAccess.save("MARKETING","COLOR_GET","ADMIN1");
                        iRoleAccess.save("MARKETING","MODEL_GET","ADMIN1");
                        iRoleAccess.save("MARKETING","MODEL_POST","ADMIN1");
                        iRoleAccess.save("MARKETING","MODEL_DELETE","ADMIN1");
                        iRoleAccess.save("MARKETING","PRODUCT_GET","ADMIN1");
                        iRoleAccess.save("MARKETING","PRODUCT_POST","ADMIN1");
                        iRoleAccess.save("MARKETING","PRODUCT_DELETE","ADMIN1");
                        iRoleAccess.save("MARKETING","PRODUCT_PRICE_POST","ADMIN1");
                        iRoleAccess.save("MARKETING","SIZE_GET","ADMIN1");
                        iRoleAccess.save("MARKETING","SIZE_TYPE_GET","ADMIN1");
                        iRoleAccess.save("MARKETING","UNIT_GET","ADMIN1");
                        iRoleAccess.save("STOCK","BRAND_GET","ADMIN1");
                        iRoleAccess.save("STOCK","COLOR_GET","ADMIN1");
                        iRoleAccess.save("STOCK","GENERAL_STOCK_GET","ADMIN1");
                        iRoleAccess.save("STOCK","MODEL_GET","ADMIN1");
                        iRoleAccess.save("STOCK","ORDER_STOCK_GET","ADMIN1");
                        iRoleAccess.save("STOCK","ORDER_STOCK_POST","ADMIN1");
                        iRoleAccess.save("STOCK","ORDER_STOCK_ITEM_GET","ADMIN1");
                        iRoleAccess.save("STOCK","PURCHASE_GET","ADMIN1");
                        iRoleAccess.save("STOCK","PURCHASE_POST","ADMIN1");
                        iRoleAccess.save("STOCK","PURCHASE_ITEM_GET","ADMIN1");
                        iRoleAccess.save("STOCK","SHIPMENT_GET","ADMIN1");
                        iRoleAccess.save("STOCK","SHIPMENT_POST","ADMIN1");
                        iRoleAccess.save("STOCK","SHIPMENT_ITEM_GET","ADMIN1");
                        iRoleAccess.save("STOCK","SHIPMENT_TYPE_GET","ADMIN1");
                        iRoleAccess.save("STOCK","SIZE_GET","ADMIN1");
                        iRoleAccess.save("STOCK","SIZE_TYPE_GET","ADMIN1");
                        iRoleAccess.save("STOCK","STOCK_REPLENISHMENT_GET","ADMIN1");
                        iRoleAccess.save("STOCK","STOCK_REPLENISHMENT_POST","ADMIN1");
                        iRoleAccess.save("STOCK","STOCK_REPLENISHMENT_ITEM_GET","ADMIN1");
                        iRoleAccess.save("STOCK","STOCK_RETURN_GET","ADMIN1");
                        iRoleAccess.save("STOCK","STOCK_RETURN_POST","ADMIN1");
                        iRoleAccess.save("STOCK","STOCK_RETURN_ITEM_GET","ADMIN1");
                        iRoleAccess.save("STOCK","STOCK_TRANSACTION_GET","ADMIN1");
                        iRoleAccess.save("STOCK","STOCK_TRANSACTION_ITEM_GET","ADMIN1");
                        iRoleAccess.save("STOCK","SUPPLIER_GET","ADMIN1");
                        iRoleAccess.save("STOCK","SUPPLIER_POST","ADMIN1");
                        iRoleAccess.save("STOCK","SUPPLIER_DELETE","ADMIN1");
                        iRoleAccess.save("STOCK","SUPPLIER_PRODUCT_GET","ADMIN1");
                        iRoleAccess.save("STOCK","SUPPLIER_PRODUCT_POST","ADMIN1");
                        iRoleAccess.save("STOCK","SUPPLIER_PRODUCT_DELETE","ADMIN1");
                        iRoleAccess.save("STOCK","UNIT_GET","ADMIN1");
                        iRoleAccess.save("STOCK","WAREHOUSE_GET","ADMIN1");
                        iRoleAccess.save("STOCK","WAREHOUSE_POST","ADMIN1");
                        iRoleAccess.save("STOCK","WAREHOUSE_DELETE","ADMIN1");
                        iRoleAccess.save("COURIER","COURIER_PUT","ADMIN1");
                        iRoleAccess.save("BUSINESS","BRAND_GET","ADMIN1");
                        iRoleAccess.save("BUSINESS","COLOR_GET","ADMIN1");
                        iRoleAccess.save("BUSINESS","CANCELLED_ORDER_GET","ADMIN1");
                        iRoleAccess.save("BUSINESS","COURIER_GET","ADMIN1");
                        iRoleAccess.save("BUSINESS","COURIER_POST","ADMIN1");
                        iRoleAccess.save("BUSINESS","GENERAL_STOCK_GET","ADMIN1");
                        iRoleAccess.save("BUSINESS","MEMBERSHIP_GET","ADMIN1");
                        iRoleAccess.save("BUSINESS","MODEL_GET","ADMIN1");
                        iRoleAccess.save("BUSINESS","ORDER_GET","ADMIN1");
                        iRoleAccess.save("BUSINESS","ORDER_ITEM_GET","ADMIN1");
                        iRoleAccess.save("BUSINESS","ORDER_STOCK_GET","ADMIN1");
                        iRoleAccess.save("BUSINESS","ORDER_STOCK_ITEM_GET","ADMIN1");
                        iRoleAccess.save("BUSINESS","ORDER_PAYMENT_METHOD_GET","ADMIN1");
                        iRoleAccess.save("BUSINESS","ORDER_PAYMENT_STATE_GET","ADMIN1");
                        iRoleAccess.save("BUSINESS","ORDER_STATE_GET","ADMIN1");
                        iRoleAccess.save("BUSINESS","PRODUCT_GET","ADMIN1");
                        iRoleAccess.save("BUSINESS","PURCHASE_GET","ADMIN1");
                        iRoleAccess.save("BUSINESS","PURCHASE_ITEM_GET","ADMIN1");
                        iRoleAccess.save("BUSINESS","ROLE_GET","ADMIN1");
                        iRoleAccess.save("BUSINESS","SHIPMENT_GET","ADMIN1");
                        iRoleAccess.save("BUSINESS","SHIPMENT_ITEM_GET","ADMIN1");
                        iRoleAccess.save("BUSINESS","SIZE_GET","ADMIN1");
                        iRoleAccess.save("BUSINESS","SIZE_TYPE_GET","ADMIN1");
                        iRoleAccess.save("BUSINESS","STOCK_REPLENISHMENT_GET","ADMIN1");
                        iRoleAccess.save("BUSINESS","STOCK_REPLENISHMENT_ITEM_GET","ADMIN1");
                        iRoleAccess.save("BUSINESS","STOCK_RETURN_GET","ADMIN1");
                        iRoleAccess.save("BUSINESS","STOCK_RETURN_ITEM_GET","ADMIN1");
                        iRoleAccess.save("BUSINESS","STOCK_TRANSACTION_GET","ADMIN1");
                        iRoleAccess.save("BUSINESS","STOCK_TRANSACTION_ITEM_GET","ADMIN1");
                        iRoleAccess.save("BUSINESS","STOCK_TRANSFER_GET","ADMIN1");
                        iRoleAccess.save("BUSINESS","STOCK_TRANSFER_ITEM_GET","ADMIN1");
                        iRoleAccess.save("BUSINESS","STORE_GET","ADMIN1");
                        iRoleAccess.save("BUSINESS","STORE_POST","ADMIN1");
                        iRoleAccess.save("BUSINESS","STORE_DELETE","ADMIN1");
                        iRoleAccess.save("BUSINESS","SUPPLIER_GET","ADMIN1");
                        iRoleAccess.save("BUSINESS","UNIT_GET","ADMIN1");
                        iRoleAccess.save("BUSINESS","UNIT_TYPE_GET","ADMIN1");
                        iRoleAccess.save("BUSINESS","USER_GET","ADMIN1");
                        iRoleAccess.save("BUSINESS","USER_POST","ADMIN1");
                        iRoleAccess.save("BUSINESS","WAREHOUSE_GET","ADMIN1");
                        iRoleAccess.save("BUSINESS","WAREHOUSE_STOCK_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","ACCESS_POST","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","BRAND_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","COLOR_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","CANCELLED_ORDER_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","CANCELLATION_REASON_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","CANCELLATION_REASON_POST","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","CATEGORY_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","CATEGORY_POST","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","CATEGORY_DELETE","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","CATEGORY_PUT","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","COURIER_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","COURIER_POST","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","GENERAL_STOCK_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","MEMBERSHIP_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","MODEL_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","ORDER_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","ORDER_ITEM_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","ORDER_STOCK_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","ORDER_STOCK_ITEM_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","ORDER_PAYMENT_METHOD_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","ORDER_PAYMENT_STATE_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","ORDER_STATE_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","PRODUCT_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","PURCHASE_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","PURCHASE_ITEM_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","ROLE_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","SHIPMENT_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","SHIPMENT_ITEM_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","SIZE_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","SIZE_TYPE_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","STOCK_REPLENISHMENT_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","STOCK_REPLENISHMENT_ITEM_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","STOCK_RETURN_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","STOCK_RETURN_ITEM_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","STOCK_TRANSACTION_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","STOCK_TRANSACTION_ITEM_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","STOCK_TRANSFER_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","STOCK_TRANSFER_ITEM_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","STORE_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","STORE_POST","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","STORE_DELETE","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","SUPPLIER_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","UNIT_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","UNIT_TYPE_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","USER_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","USER_POST","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","WAREHOUSE_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRATION","WAREHOUSE_STOCK_GET","ADMIN1");
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
                        iOrderState.save("cancelado","admin1");

                        // payment state
                        iOrderPaymentState.save("por recaudar","admin1");
                        iOrderPaymentState.save("recaudado","admin1");
                        iOrderPaymentState.save("perdida","admin1");

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
                        iOrderPaymentMethod.save("yape","admin1");
                        iOrderPaymentMethod.save("pos","admin1");
                        iOrderPaymentMethod.save("efectivo","admin1");
                        iOrderPaymentMethod.save("link","admin1");
                        iOrderPaymentMethod.save("cambio","admin1");
                        iOrderPaymentMethod.save("plin","admin1");
                        iOrderPaymentMethod.save("plataforma mp/web","admin1");
                        iOrderPaymentMethod.save("bcp","admin1");
                        iOrderPaymentMethod.save("contraentrega","admin1");
                        iOrderPaymentMethod.save("canje","admin1");
                        iOrderPaymentMethod.save("interbank","admin1");
                        iOrderPaymentMethod.save("banco de la nacion","admin1");

                        //unit type
                        iUnitType.save("ropa","admin1");
                        iUnitType.save("calzado","admin1");

                        // unit
                        RequestUnit requestUnit1 = RequestUnit.builder()
                                .name("prenda")
                                .unitType("ropa")
                                .build();

                        iUnit.save(requestUnit1,"admin1");

                        RequestUnit requestUnit2 = RequestUnit.builder()
                                .name("par")
                                .unitType("calzado")
                                .build();

                        iUnit.save(requestUnit2,"admin1");

                        // shipment type
                        iShipmentType.save("embarque","admin1");
                        iShipmentType.save("devolucion","admin1");

                        // cancellation reason
                        iCancellationReason.save("No hay stock","admin1");
                        iCancellationReason.save("Demora en entrega","admin1");
                        iCancellationReason.save("Mala calidad","admin1");
                        iCancellationReason.save("Se le daño el producto - 30 dias","admin1");
                        iCancellationReason.save("Otros motivos","admin1");
                        iCancellationReason.save("Muy caro el envio","admin1");
                        iCancellationReason.save("Zona peligrosa","admin1");
                        iCancellationReason.save("Cliente no confiable para contraentrega","admin1");
                        iCancellationReason.save("Robo por motorizado","admin1");
                        iCancellationReason.save("No le gusto producto","admin1");

                        // payment gateway
                        iPaymentGateway.save("mercado pago","admin1");
                        iPaymentGateway.save("demo","admin1");

                        // membership states
                        iMembershipState.save("activa","admin1");
                        iMembershipState.save("pagada","admin1");
                        iMembershipState.save("expirada","admin1");

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
                        List<MultipartFile> productImages1 = new ArrayList<>();
                        File image1 = new File("C:\\Users\\USUARIO\\Documents\\code\\work\\repositories\\masterdata-java17\\src\\main\\test_pics\\bill.jpg");
                        // uncomment for deployment
                        // File image1 = new File("src/main/test_pics/bill.jpg");
                        FileInputStream imageInputStream1 = new FileInputStream(image1);
                        byte[] imageBytes1 = new byte[(int) image1.length()];
                        imageInputStream1.read(imageBytes1);
                        imageInputStream1.close();
                        MockMultipartFile multipartImage1 = new MockMultipartFile(
                                "file",
                                image1.getName(),
                                "image/jpeg",
                                imageBytes1
                        );
                        productImages1.add(multipartImage1);
                        List<MultipartFile> productImages2 = new ArrayList<>();
                        File image2 = new File("C:\\Users\\USUARIO\\Documents\\code\\work\\repositories\\masterdata-java17\\src\\main\\test_pics\\invoice1.jpg");
                        // uncomment for deployment
                        // File image2 = new File("src/main/test_pics/bill.jpg");
                        FileInputStream imageInputStream2 = new FileInputStream(image2);
                        byte[] imageBytes2 = new byte[(int) image2.length()];
                        imageInputStream2.read(imageBytes2);
                        imageInputStream2.close();
                        MockMultipartFile multipartImage2 = new MockMultipartFile(
                                "file",
                                image2.getName(),
                                "image/jpeg",
                                imageBytes2
                        );
                        productImages2.add(multipartImage2);
                        RequestProductSave product1 = RequestProductSave.builder().build();
                        product1.setCategory("tennis");
                        product1.setColor("negro");
                        product1.setModel("f90");
                        product1.setSize("12");
                        product1.setSku("A00001");
                        product1.setPrice(2.30);
                        product1.setUnit("par");
                        product1.setPictures(productImages1);

                        iProduct.save(product1, "gjimenez");

                        RequestProductSave product2 = RequestProductSave.builder().build();
                        product2.setCategory("botas");
                        product2.setColor("rojo");
                        product2.setModel("m2000");
                        product2.setSize("24");
                        product2.setSku("A00002");
                        product2.setPrice(5.41);
                        product2.setUnit("par");
                        product2.setPictures(productImages2);

                        iProduct.save(product2, "gjimenez");

                        RequestProductSave product3 = RequestProductSave.builder().build();
                        product3.setCategory("tennis");
                        product3.setColor("verde");
                        product3.setModel("mercurial");
                        product3.setSize("24");
                        product3.setSku("A00003");
                        product3.setPrice(3.33);
                        product3.setUnit("par");
                        product3.setPictures(productImages1);

                        iProduct.save(product3, "gjimenez");

                        RequestProductSave product4 = RequestProductSave.builder().build();
                        product4.setCategory("camisetas");
                        product4.setColor("rojo");
                        product4.setModel("indigo");
                        product4.setSize("s");
                        product4.setSku("A00004");
                        product4.setPrice(7.01);
                        product4.setUnit("prenda");
                        product4.setPictures(productImages2);

                        iProduct.save(product4, "gjimenez");

                        RequestProductSave product5 = RequestProductSave.builder().build();
                        product5.setCategory("jeans");
                        product5.setColor("azul");
                        product5.setModel("old navy");
                        product5.setSize("m");
                        product5.setSku("A00005");
                        product5.setPrice(4.76);
                        product5.setUnit("prenda");
                        product5.setPictures(productImages1);

                        iProduct.save(product5, "gjimenez");

                        RequestProductSave product6 = RequestProductSave.builder().build();
                        product6.setCategory("blusas");
                        product6.setColor("amarillo");
                        product6.setModel("ripper");
                        product6.setSize("l");
                        product6.setSku("A00006");
                        product6.setPrice(1.34);
                        product6.setUnit("prenda");
                        product6.setPictures(productImages2);

                        iProduct.save(product6, "gjimenez");

                        RequestProductSave product7 = RequestProductSave.builder().build();
                        product7.setCategory("blusas");
                        product7.setColor("morado");
                        product7.setModel("sweater");
                        product7.setSize("xs");
                        product7.setSku("A00007");
                        product7.setPrice(8.23);
                        product7.setUnit("prenda");
                        product7.setPictures(productImages1);

                        iProduct.save(product7, "gjimenez");

                        RequestProductSave product8 = RequestProductSave.builder().build();
                        product8.setCategory("camisetas");
                        product8.setColor("verde");
                        product8.setModel("kasper");
                        product8.setSize("xm");
                        product8.setSku("A00008");
                        product8.setPrice(6.27);
                        product8.setUnit("prenda");
                        product8.setPictures(productImages2);

                        iProduct.save(product8, "gjimenez");

                        RequestProductSave product9 = RequestProductSave.builder().build();
                        product9.setCategory("blusas");
                        product9.setColor("naranja");
                        product9.setModel("sustra");
                        product9.setSize("xl");
                        product9.setSku("A00009");
                        product9.setPrice(9.05);
                        product9.setUnit("prenda");
                        product9.setPictures(productImages1);

                        iProduct.save(product9, "gjimenez");

                        RequestProductSave product10 = RequestProductSave.builder().build();
                        product10.setCategory("botas");
                        product10.setColor("rojo");
                        product10.setModel("krust");
                        product10.setSize("40");
                        product10.setSku("B00001");
                        product10.setPrice(7.11);
                        product10.setUnit("par");
                        product10.setPictures(productImages2);

                        iProduct.save(product10, "fcasas");

                        RequestProductSave product11 = RequestProductSave.builder().build();
                        product11.setCategory("tennis");
                        product11.setColor("verde");
                        product11.setModel("gist");
                        product11.setSize("32");
                        product11.setSku("B00002");
                        product11.setPrice(4.65);
                        product11.setUnit("par");
                        product11.setPictures(productImages1);

                        iProduct.save(product11, "fcasas");

                        RequestProductSave product12 = RequestProductSave.builder().build();
                        product12.setCategory("tennis");
                        product12.setColor("azul");
                        product12.setModel("thunder");
                        product12.setSize("18");
                        product12.setSku("B00003");
                        product12.setPrice(8.38);
                        product12.setUnit("par");
                        product12.setPictures(productImages2);

                        iProduct.save(product12, "fcasas");

                        RequestProductSave product13 = RequestProductSave.builder().build();
                        product13.setCategory("camisetas");
                        product13.setColor("negro");
                        product13.setModel("yitro");
                        product13.setSize("s");
                        product13.setSku("B00004");
                        product13.setPrice(4.02);
                        product13.setUnit("prenda");
                        product13.setPictures(productImages1);

                        iProduct.save(product13, "fcasas");

                        RequestProductSave product14 = RequestProductSave.builder().build();
                        product14.setCategory("blusas");
                        product14.setColor("morado");
                        product14.setModel("ulcast");
                        product14.setSize("m");
                        product14.setSku("B00005");
                        product14.setPrice(1.99);
                        product14.setUnit("prenda");
                        product14.setPictures(productImages2);

                        iProduct.save(product14, "fcasas");

                        RequestProductSave product15 = RequestProductSave.builder().build();
                        product15.setCategory("jeans");
                        product15.setColor("amarillo");
                        product15.setModel("reinder");
                        product15.setSize("l");
                        product15.setSku("B00006");
                        product15.setPrice(6.37);
                        product15.setUnit("prenda");
                        product15.setPictures(productImages1);

                        iProduct.save(product15, "fcasas");

                        RequestProductSave product16 = RequestProductSave.builder().build();
                        product16.setCategory("camisetas");
                        product16.setColor("rojo");
                        product16.setModel("realt");
                        product16.setSize("xl");
                        product16.setSku("B00007");
                        product16.setPrice(2.97);
                        product16.setUnit("prenda");
                        product16.setPictures(productImages2);

                        iProduct.save(product16, "fcasas");

                        RequestProductSave product17 = RequestProductSave.builder().build();
                        product17.setCategory("blusas");
                        product17.setColor("azul");
                        product17.setModel("brust");
                        product17.setSize("xs");
                        product17.setSku("B00008");
                        product17.setPrice(5.21);
                        product17.setUnit("prenda");
                        product17.setPictures(productImages1);

                        iProduct.save(product17, "fcasas");

                        RequestProductSave product18 = RequestProductSave.builder().build();
                        product18.setCategory("camisetas");
                        product18.setColor("naranja");
                        product18.setModel("frost");
                        product18.setSize("m");
                        product18.setSku("B00009");
                        product18.setPrice(3.53);
                        product18.setUnit("prenda");
                        product18.setPictures(productImages2);

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

                        iSupplierProduct.save(requestSupplierProduct19, "fcasas");

                        RequestSupplierProduct requestSupplierProduct20 = RequestSupplierProduct.builder()
                                .productSku("B00001")
                                .purchasePrice(1.05)
                                .serial("B00001B")
                                .supplierRuc("12345678926")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct20, "fcasas");

                        RequestSupplierProduct requestSupplierProduct21 = RequestSupplierProduct.builder()
                                .productSku("B00002")
                                .purchasePrice(7.20)
                                .serial("B00002A")
                                .supplierRuc("12345678925")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct21, "fcasas");

                        RequestSupplierProduct requestSupplierProduct22 = RequestSupplierProduct.builder()
                                .productSku("B00002")
                                .purchasePrice(5.68)
                                .serial("B00002B")
                                .supplierRuc("12345678926")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct22, "fcasas");

                        RequestSupplierProduct requestSupplierProduct23 = RequestSupplierProduct.builder()
                                .productSku("B00003")
                                .purchasePrice(36.49)
                                .serial("B00003A")
                                .supplierRuc("12345678925")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct23, "fcasas");

                        RequestSupplierProduct requestSupplierProduct24 = RequestSupplierProduct.builder()
                                .productSku("B00003")
                                .purchasePrice(45.27)
                                .serial("B00003B")
                                .supplierRuc("12345678926")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct24, "fcasas");

                        RequestSupplierProduct requestSupplierProduct25 = RequestSupplierProduct.builder()
                                .productSku("B00004")
                                .purchasePrice(22.38)
                                .serial("B00004A")
                                .supplierRuc("12345678925")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct25, "fcasas");

                        RequestSupplierProduct requestSupplierProduct26 = RequestSupplierProduct.builder()
                                .productSku("B00004")
                                .purchasePrice(15.07)
                                .serial("B00004B")
                                .supplierRuc("12345678926")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct26, "fcasas");

                        RequestSupplierProduct requestSupplierProduct27 = RequestSupplierProduct.builder()
                                .productSku("B00005")
                                .purchasePrice(73.02)
                                .serial("B00005A")
                                .supplierRuc("12345678925")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct27, "fcasas");

                        RequestSupplierProduct requestSupplierProduct28 = RequestSupplierProduct.builder()
                                .productSku("B00005")
                                .purchasePrice(82.17)
                                .serial("B00005B")
                                .supplierRuc("12345678926")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct28, "fcasas");

                        RequestSupplierProduct requestSupplierProduct29 = RequestSupplierProduct.builder()
                                .productSku("B00006")
                                .purchasePrice(13.77)
                                .serial("B00006A")
                                .supplierRuc("12345678925")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct29, "fcasas");

                        RequestSupplierProduct requestSupplierProduct30 = RequestSupplierProduct.builder()
                                .productSku("B00006")
                                .purchasePrice(24.93)
                                .serial("B00006B")
                                .supplierRuc("12345678926")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct30, "fcasas");

                        RequestSupplierProduct requestSupplierProduct31 = RequestSupplierProduct.builder()
                                .productSku("B00007")
                                .purchasePrice(64.57)
                                .serial("B00007A")
                                .supplierRuc("12345678925")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct31, "fcasas");

                        RequestSupplierProduct requestSupplierProduct32 = RequestSupplierProduct.builder()
                                .productSku("B00007")
                                .purchasePrice(23.89)
                                .serial("B00007B")
                                .supplierRuc("12345678926")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct32, "fcasas");

                        RequestSupplierProduct requestSupplierProduct33 = RequestSupplierProduct.builder()
                                .productSku("B00008")
                                .purchasePrice(17.94)
                                .serial("B00008A")
                                .supplierRuc("12345678925")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct33, "fcasas");

                        RequestSupplierProduct requestSupplierProduct34 = RequestSupplierProduct.builder()
                                .productSku("B00008")
                                .purchasePrice(33.29)
                                .serial("B00008B")
                                .supplierRuc("12345678926")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct34, "fcasas");

                        RequestSupplierProduct requestSupplierProduct35 = RequestSupplierProduct.builder()
                                .productSku("B00009")
                                .purchasePrice(95.22)
                                .serial("B00009A")
                                .supplierRuc("12345678925")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct35, "fcasas");

                        RequestSupplierProduct requestSupplierProduct36 = RequestSupplierProduct.builder()
                                .productSku("B00009")
                                .purchasePrice(83.19)
                                .serial("B00009B")
                                .supplierRuc("12345678926")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct36, "fcasas");

                        // stock transaction types mocks
                        iStockTransactionType.save("entrada", "admin1");
                        iStockTransactionType.save("salida", "admin1");
                        iStockTransactionType.save("transferencia-entrada", "admin1");
                        iStockTransactionType.save("transferencia-salida", "admin1");
                        iStockTransactionType.save("devolucion-comprador","admin1");
                        iStockTransactionType.save("devolucion-proveedor","admin1");

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

                        // courier
                        RequestCourier requestCourier1 = RequestCourier.builder()
                                .courier("Marvisur")
                                .phoneNumber("123456789")
                                .build();

                        iCourier.save(requestCourier1,"gjimenez");

                        RequestCourier requestCourier2 = RequestCourier.builder()
                                .courier("Rappi")
                                .phoneNumber("111111111")
                                .build();

                        iCourier.save(requestCourier2,"gjimenez");

                        RequestCourier requestCourier3 = RequestCourier.builder()
                                .courier("Indriver")
                                .phoneNumber("222222222")
                                .build();

                        iCourier.save(requestCourier3,"fcasas");

                        RequestCourier requestCourier4 = RequestCourier.builder()
                                .courier("Shalom")
                                .phoneNumber("333333333")
                                .build();

                        iCourier.save(requestCourier4,"fcasas");

                        RequestCourier requestCourier5 = RequestCourier.builder()
                                .courier("Sin courier")
                                .phoneNumber("000000000")
                                .build();

                        iCourier.save(requestCourier5,"admin1");

                        // purchase mocks

                        List<RequestPurchaseItem> requestPurchaseItemList1 = new ArrayList<>();

                        RequestPurchaseItem requestPurchaseItem1 = RequestPurchaseItem.builder()
                                .quantity(15)
                                .supplierProductSerial("A00001A")
                                .unitPrice(3.40)
                                .build();

                        requestPurchaseItemList1.add(requestPurchaseItem1);

                        RequestPurchaseItem requestPurchaseItem2 = RequestPurchaseItem.builder()
                                .quantity(4)
                                .supplierProductSerial("A00001B")
                                .unitPrice(7.8)
                                .build();

                        requestPurchaseItemList1.add(requestPurchaseItem2);

                        RequestPurchaseItem requestPurchaseItem3 = RequestPurchaseItem.builder()
                                .quantity(20)
                                .supplierProductSerial("A00002A")
                                .unitPrice(12.45)
                                .build();

                        requestPurchaseItemList1.add(requestPurchaseItem3);

                        RequestPurchaseItem requestPurchaseItem4 = RequestPurchaseItem.builder()
                                .quantity(25)
                                .supplierProductSerial("A00002B")
                                .unitPrice(20.03)
                                .build();

                        requestPurchaseItemList1.add(requestPurchaseItem4);

                        RequestPurchaseItem requestPurchaseItem5 = RequestPurchaseItem.builder()
                                .quantity(7)
                                .supplierProductSerial("A00003A")
                                .unitPrice(14.76)
                                .build();

                        requestPurchaseItemList1.add(requestPurchaseItem5);

                        RequestPurchaseItem requestPurchaseItem6 = RequestPurchaseItem.builder()
                                .quantity(15)
                                .supplierProductSerial("A00003B")
                                .unitPrice(2.91)
                                .build();

                        requestPurchaseItemList1.add(requestPurchaseItem6);

                        List<RequestPurchaseItem> requestPurchaseItemList2 = new ArrayList<>();

                        RequestPurchaseItem requestPurchaseItem7 = RequestPurchaseItem.builder()
                                .quantity(9)
                                .supplierProductSerial("B00001A")
                                .unitPrice(23.77)
                                .build();

                        requestPurchaseItemList2.add(requestPurchaseItem7);

                        RequestPurchaseItem requestPurchaseItem8 = RequestPurchaseItem.builder()
                                .quantity(15)
                                .supplierProductSerial("B00001B")
                                .unitPrice(13.91)
                                .build();

                        requestPurchaseItemList2.add(requestPurchaseItem8);

                        RequestPurchaseItem requestPurchaseItem9 = RequestPurchaseItem.builder()
                                .quantity(36)
                                .supplierProductSerial("B00002A")
                                .unitPrice(5.19)
                                .build();

                        requestPurchaseItemList2.add(requestPurchaseItem9);

                        RequestPurchaseItem requestPurchaseItem10 = RequestPurchaseItem.builder()
                                .quantity(13)
                                .supplierProductSerial("B00002B")
                                .unitPrice(2.72)
                                .build();

                        requestPurchaseItemList2.add(requestPurchaseItem10);

                        RequestPurchaseItem requestPurchaseItem11 = RequestPurchaseItem.builder()
                                .quantity(20)
                                .supplierProductSerial("B00003B")
                                .unitPrice(17.40)
                                .build();

                        requestPurchaseItemList2.add(requestPurchaseItem11);

                        RequestPurchaseItem requestPurchaseItem12 = RequestPurchaseItem.builder()
                                .quantity(27)
                                .supplierProductSerial("B00003A")
                                .unitPrice(10.99)
                                .build();

                        requestPurchaseItemList2.add(requestPurchaseItem12);

                        iPurchase.save("AA00001", requestPurchaseItemList1, "gjimenez");
                        iPurchase.save("BB00001", requestPurchaseItemList2, "fcasas");

                        // shipments

                        List<RequestShipmentItem> requestShipmentItemList1 = new ArrayList<>();

                        RequestShipmentItem requestShipmentItem1 = RequestShipmentItem.builder()
                                .observations("no aplica")
                                .purchaseSerial("AA00001")
                                .quantity(15)
                                .supplierProductSerial("A00001A")
                                .build();

                        requestShipmentItemList1.add(requestShipmentItem1);

                        RequestShipmentItem requestShipmentItem2 = RequestShipmentItem.builder()
                                .observations("no aplica")
                                .purchaseSerial("AA00001")
                                .quantity(4)
                                .supplierProductSerial("A00001B")
                                .build();

                        requestShipmentItemList1.add(requestShipmentItem2);

                        RequestShipmentItem requestShipmentItem3 = RequestShipmentItem.builder()
                                .observations("no aplica")
                                .purchaseSerial("AA00001")
                                .quantity(20)
                                .supplierProductSerial("A00002A")
                                .build();

                        requestShipmentItemList1.add(requestShipmentItem3);

                        RequestShipmentItem requestShipmentItem4 = RequestShipmentItem.builder()
                                .observations("no aplica")
                                .purchaseSerial("AA00001")
                                .quantity(25)
                                .supplierProductSerial("A00002B")
                                .build();

                        requestShipmentItemList1.add(requestShipmentItem4);

                        RequestShipmentItem requestShipmentItem5 = RequestShipmentItem.builder()
                                .observations("no aplica")
                                .purchaseSerial("AA00001")
                                .quantity(7)
                                .supplierProductSerial("A00003A")
                                .build();

                        requestShipmentItemList1.add(requestShipmentItem5);

                        RequestShipmentItem requestShipmentItem6 = RequestShipmentItem.builder()
                                .observations("no aplica")
                                .purchaseSerial("AA00001")
                                .quantity(15)
                                .supplierProductSerial("A00003B")
                                .build();

                        requestShipmentItemList1.add(requestShipmentItem6);

                        RequestShipment requestShipment1 = RequestShipment.builder()
                                .shipmentType("embarque")
                                .requestShipmentItemList(requestShipmentItemList1)
                                .warehouse("luminous")
                                .purchaseSerial("AA00001")
                                .build();

                        iShipment.save(requestShipment1, "gjimenez");

                        List<RequestShipmentItem> requestShipmentItemList2 = new ArrayList<>();

                        RequestShipmentItem requestShipmentItem7 = RequestShipmentItem.builder()
                                .observations("no aplica")
                                .purchaseSerial("BB00001")
                                .quantity(9)
                                .supplierProductSerial("B00001A")
                                .build();

                        requestShipmentItemList2.add(requestShipmentItem7);

                        RequestShipmentItem requestShipmentItem8 = RequestShipmentItem.builder()
                                .observations("no aplica")
                                .purchaseSerial("BB00001")
                                .quantity(15)
                                .supplierProductSerial("B00001B")
                                .build();

                        requestShipmentItemList2.add(requestShipmentItem8);

                        RequestShipmentItem requestShipmentItem9 = RequestShipmentItem.builder()
                                .observations("no aplica")
                                .purchaseSerial("BB00001")
                                .quantity(36)
                                .supplierProductSerial("B00002A")
                                .build();

                        requestShipmentItemList2.add(requestShipmentItem9);

                        RequestShipmentItem requestShipmentItem10 = RequestShipmentItem.builder()
                                .observations("no aplica")
                                .purchaseSerial("BB00001")
                                .quantity(13)
                                .supplierProductSerial("B00002B")
                                .build();

                        requestShipmentItemList2.add(requestShipmentItem10);

                        RequestShipmentItem requestShipmentItem11 = RequestShipmentItem.builder()
                                .observations("no aplica")
                                .purchaseSerial("BB00001")
                                .quantity(20)
                                .supplierProductSerial("B00003A")
                                .build();

                        requestShipmentItemList2.add(requestShipmentItem11);

                        RequestShipmentItem requestShipmentItem12 = RequestShipmentItem.builder()
                                .observations("no aplica")
                                .purchaseSerial("BB00001")
                                .quantity(27)
                                .supplierProductSerial("B00003B")
                                .build();

                        requestShipmentItemList2.add(requestShipmentItem12);

                        RequestShipment requestShipment2 = RequestShipment.builder()
                                .shipmentType("embarque")
                                .purchaseSerial("BB00001")
                                .requestShipmentItemList(requestShipmentItemList2)
                                .warehouse("alcazar")
                                .build();

                        iShipment.save(requestShipment2, "fcasas");

                        // orders mocks
                        List<MultipartFile> receipts1 = new ArrayList<>();
                        File receipt1 = new File("C:\\Users\\USUARIO\\Documents\\code\\work\\repositories\\masterdata-java17\\src\\main\\test_pics\\bill.jpg");
                        // uncomment for deployment
                        // File receipt1 = new File("src/main/test_pics/bill.jpg");
                        FileInputStream fileInputStream1 = new FileInputStream(receipt1);
                        byte[] fileBytes1 = new byte[(int) receipt1.length()];
                        fileInputStream1.read(fileBytes1);
                        fileInputStream1.close();
                        MockMultipartFile multipartFile1 = new MockMultipartFile(
                                "file",
                                receipt1.getName(),
                                "image/jpeg",
                                fileBytes1
                        );
                        receipts1.add(multipartFile1);

                        RequestOrderItem requestOrderItem1 = RequestOrderItem.builder()
                                .productSku("A00001")
                                .discount(0.00)
                                .quantity(2)
                                .observations("")
                                .build();

                        RequestOrderItem requestOrderItem2 = RequestOrderItem.builder()
                                .quantity(1)
                                .discount(3.00)
                                .productSku("A00002")
                                .observations("")
                                .build();

                        ArrayList<RequestOrderItem> requestOrderItems1 = new ArrayList<>();

                        requestOrderItems1.add(requestOrderItem1);
                        requestOrderItems1.add(requestOrderItem2);

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
                                .receipts(receipts1)
                                .saleChannel("tienda online")
                                .requestOrderItems(requestOrderItems1)
                                .build();

                        iOrdering.save(requestOrderSave1,"gjimenez");

                        List<MultipartFile> receipts2 = new ArrayList<>();
                        File receipt2 = new File("C:\\Users\\USUARIO\\Documents\\code\\work\\repositories\\masterdata-java17\\src\\main\\test_pics\\receiptarticle.jpg");
                        // uncomment for deployment
                        // File receipt2 = new File("src/main/test_pics/receiptarticle.jpg");
                        FileInputStream fileInputStream2 = new FileInputStream(receipt2);
                        byte[] fileBytes2 = new byte[(int) receipt2.length()];
                        fileInputStream2.read(fileBytes2);
                        fileInputStream2.close();
                        MockMultipartFile multipartFile2 = new MockMultipartFile(
                                "file",
                                receipt2.getName(),
                                "image/jpeg",
                                fileBytes2
                        );
                        receipts2.add(multipartFile2);

                        File receipt3 = new File("C:\\Users\\USUARIO\\Documents\\code\\work\\repositories\\masterdata-java17\\src\\main\\test_pics\\invoice1.jpg");
                        // uncomment for deployment
                        // File receipt3 = new File("src/main/test_pics/invoice1.jpg");
                        FileInputStream fileInputStream3 = new FileInputStream(receipt3);
                        byte[] fileBytes3 = new byte[(int) receipt3.length()];
                        fileInputStream3.read(fileBytes3);
                        fileInputStream3.close();
                        MockMultipartFile multipartFile3 = new MockMultipartFile(
                                "file",
                                receipt3.getName(),
                                "image/jpeg",
                                fileBytes3
                        );
                        receipts2.add(multipartFile3);

                        RequestOrderItem requestOrderItem3 = RequestOrderItem.builder()
                                .productSku("A00003")
                                .quantity(3)
                                .discount(0.00)
                                .observations("")
                                .build();

                        RequestOrderItem requestOrderItem4 = RequestOrderItem.builder()
                                .productSku("A00001")
                                .quantity(1)
                                .discount(2.00)
                                .observations("")
                                .build();

                        ArrayList<RequestOrderItem> requestOrderItems2 = new ArrayList<>();

                        requestOrderItems2.add(requestOrderItem3);
                        requestOrderItems2.add(requestOrderItem4);

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
                                .receipts(receipts2)
                                .saleChannel("tienda online")
                                .requestOrderItems(requestOrderItems2)
                                .build();

                        iOrdering.save(requestOrderSave2,"gjimenez");

                        List<MultipartFile> receipts3 = new ArrayList<>();

                        RequestOrderItem requestOrderItem5 = RequestOrderItem.builder()
                                .productSku("B00001")
                                .discount(0.00)
                                .quantity(1)
                                .observations("")
                                .build();

                        RequestOrderItem requestOrderItem6 = RequestOrderItem.builder()
                                .quantity(3)
                                .discount(5.00)
                                .productSku("B00002")
                                .observations("")
                                .build();

                        List<RequestOrderItem> requestOrderItems3 = new ArrayList<>();

                        requestOrderItems3.add(requestOrderItem5);
                        requestOrderItems3.add(requestOrderItem6);

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
                                .receipts(receipts3)
                                .saleChannel("tienda online")
                                .requestOrderItems(requestOrderItems3)
                                .build();

                        iOrdering.save(requestOrderSave3,"fcasas");

                        List<MultipartFile> receipts4 = new ArrayList<>();

                        RequestOrderItem requestOrderItem7 = RequestOrderItem.builder()
                                .productSku("B00002")
                                .discount(7.00)
                                .quantity(5)
                                .observations("")
                                .build();

                        RequestOrderItem requestOrderItem8 = RequestOrderItem.builder()
                                .quantity(2)
                                .discount(0.00)
                                .productSku("B00003")
                                .observations("")
                                .build();

                        List<RequestOrderItem> requestOrderItems4 = new ArrayList<>();

                        requestOrderItems4.add(requestOrderItem7);
                        requestOrderItems4.add(requestOrderItem8);

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
                                .receipts(receipts4)
                                .saleChannel("tienda online")
                                .requestOrderItems(requestOrderItems4)
                                .build();

                        iOrdering.save(requestOrderSave4,"fcasas");

                        RequestOrderItem requestOrderItem9 = RequestOrderItem.builder()
                                .productSku("A00003")
                                .quantity(5)
                                .discount(0.00)
                                .observations("")
                                .build();

                        RequestOrderItem requestOrderItem10 = RequestOrderItem.builder()
                                .productSku("A00001")
                                .quantity(8)
                                .discount(2.00)
                                .observations("")
                                .build();

                        ArrayList<RequestOrderItem> requestOrderItems5 = new ArrayList<>();

                        requestOrderItems5.add(requestOrderItem9);
                        requestOrderItems5.add(requestOrderItem10);

                        RequestOrderSave requestOrderSave5 = RequestOrderSave.builder()
                                .advancedPayment(2.00)
                                .customerAddress("AV. JORGE CHAVEZ 420, OFICN LIMA")
                                .customerDepartment("LIMA")
                                .customerProvince("LIMA")
                                .customerDistrict("INDEPENDENCIA")
                                .customerName("Consuelo Rojas")
                                .customerPhone("956701333")
                                .customerReference("")
                                .customerType("Tradicional")
                                .deliveryAddress("AV. JORGE CHAVEZ 420, OFICN LIMA")
                                .deliveryAmount(4.00)
                                .managementType("venta")
                                .instagram("")
                                .observations("")
                                .paymentMethod("plin")
                                .receipts(receipts2)
                                .saleChannel("tienda online")
                                .requestOrderItems(requestOrderItems5)
                                .build();

                        iOrdering.save(requestOrderSave5,"gjimenez");

                        // order stock mocks

                        RequestOrderStockItem requestOrderStockItem1 = RequestOrderStockItem.builder()
                                .supplierProductSerial("A00001A")
                                .quantity(2)
                                .itemId(1L)
                                .build();

                        RequestOrderStockItem requestOrderStockItem2 = RequestOrderStockItem.builder()
                                .supplierProductSerial("A00002A")
                                .quantity(1)
                                .itemId(2L)
                                .build();

                        List<RequestOrderStockItem> requestOrderStockItemList1 = new ArrayList<>();
                        requestOrderStockItemList1.add(requestOrderStockItem1);
                        requestOrderStockItemList1.add(requestOrderStockItem2);

                        iOrderStock.save(1L,"luminous", requestOrderStockItemList1,"gjimenez");

                        RequestOrderStockItem requestOrderStockItem3 = RequestOrderStockItem.builder()
                                .supplierProductSerial("A00003A")
                                .quantity(2)
                                .itemId(3L)
                                .build();

                        RequestOrderStockItem requestOrderStockItem4 = RequestOrderStockItem.builder()
                                .supplierProductSerial("A00002A")
                                .quantity(1)
                                .itemId(4L)
                                .build();

                        List<RequestOrderStockItem> requestOrderStockItemList2 = new ArrayList<>();
                        requestOrderStockItemList2.add(requestOrderStockItem3);
                        requestOrderStockItemList2.add(requestOrderStockItem4);

                        iOrderStock.save(2L,"luminous", requestOrderStockItemList2,"gjimenez");

                        RequestOrderStockItem requestOrderStockItem5 = RequestOrderStockItem.builder()
                                .supplierProductSerial("B00001A")
                                .quantity(1)
                                .itemId(5L)
                                .build();

                        RequestOrderStockItem requestOrderStockItem6 = RequestOrderStockItem.builder()
                                .supplierProductSerial("B00002A")
                                .quantity(3)
                                .itemId(6L)
                                .build();

                        List<RequestOrderStockItem> requestOrderStockItemList3 = new ArrayList<>();
                        requestOrderStockItemList3.add(requestOrderStockItem5);
                        requestOrderStockItemList3.add(requestOrderStockItem6);

                        iOrderStock.save(3L,"alcazar", requestOrderStockItemList3,"fcasas");

                        RequestOrderStockItem requestOrderStockItem7 = RequestOrderStockItem.builder()
                                .supplierProductSerial("B00002A")
                                .quantity(5)
                                .itemId(7L)
                                .build();

                        RequestOrderStockItem requestOrderStockItem8 = RequestOrderStockItem.builder()
                                .supplierProductSerial("B00003A")
                                .quantity(2)
                                .itemId(8L)
                                .build();

                        List<RequestOrderStockItem> requestOrderStockItemList4 = new ArrayList<>();
                        requestOrderStockItemList4.add(requestOrderStockItem7);
                        requestOrderStockItemList4.add(requestOrderStockItem8);

                        iOrderStock.save(4L, "alcazar",requestOrderStockItemList4,"fcasas");

                        List<MultipartFile> paymentReceipts = new ArrayList<MultipartFile>();
                        List<MultipartFile> courierPictures = new ArrayList<MultipartFile>();

                        RequestOrderUpdate requestOrderUpdate1 = RequestOrderUpdate.builder()
                                .observations("")
                                .orderState("ENTREGADO")
                                .paymentMethod("LINK")
                                .paymentState("RECAUDADO")
                                .saleChannel("tienda online")
                                .receipts(paymentReceipts)
                                .courier("MARVISUR")
                                .pictures(courierPictures)
                                .build();

                        iOrdering.update(1L,requestOrderUpdate1,"gjimenez");

                        RequestCancelledOrder requestCancelledOrder1 = RequestCancelledOrder.builder()
                                .cancellationReason("Demora en entrega")
                                .orderId(2L)
                                .warehouse("luminous")
                                .build();

                        iCancelledOrder.save(requestCancelledOrder1,"gjimenez");

                        // mock courier pictures and change state to delivered
                        List<MultipartFile> courierImages1 = new ArrayList<>();
                        File courierImage1 = new File("C:\\Users\\USUARIO\\Documents\\code\\work\\repositories\\masterdata-java17\\src\\main\\test_pics\\bill.jpg");
                        // uncomment for deployment
                        // File courierImage1 = new File("src/main/test_pics/bill.jpg");
                        FileInputStream courierImageInputStream1 = new FileInputStream(courierImage1);
                        byte[] courierImageBytes1 = new byte[(int) courierImage1.length()];
                        courierImageInputStream1.read(courierImageBytes1);
                        courierImageInputStream1.close();
                        MockMultipartFile multipartCourierImage1 = new MockMultipartFile(
                                "file",
                                courierImage1.getName(),
                                "image/jpeg",
                                courierImageBytes1
                        );
                        courierImages1.add(multipartCourierImage1);

                        RequestCourierOrder requestCourierOrder1 = RequestCourierOrder.builder()
                                .orderPictures(courierImages1)
                                .orderState("ENTREGADO")
                                .paymentMethod("LINK")
                                .build();

                        iCourier.updateOrder(1L,requestCourierOrder1,"gjimenez");

                        // mock register stock return
                        List<RequestStockReturnItem> requestStockReturnItemList = new ArrayList<RequestStockReturnItem>();
                        RequestStockReturnItem requestStockReturnItem1 = RequestStockReturnItem.builder()
                                .observations("unidades dañadas por agua")
                                .quantity(4)
                                .supplierProductSerial("A00001A")
                                .build();
                        requestStockReturnItemList.add(requestStockReturnItem1);
                        RequestStockReturnItem requestStockReturnItem2 = RequestStockReturnItem.builder()
                                .observations("unidades defectuosas")
                                .quantity(2)
                                .supplierProductSerial("A00002A")
                                .build();
                        requestStockReturnItemList.add(requestStockReturnItem2);
                        RequestStockReturnItem requestStockReturnItem3 = RequestStockReturnItem.builder()
                                .observations("mercancia incorrecta")
                                .quantity(3)
                                .supplierProductSerial("A00003A")
                                .build();
                        requestStockReturnItemList.add(requestStockReturnItem3);
                        iStockReturn.save("AA00001",requestStockReturnItemList,"gjimenez");
                        // update order to lack of stock state
                        RequestOrderUpdate requestOrderUpdate3 = RequestOrderUpdate.builder()
                                .pictures(new ArrayList<>())
                                .receipts(receipts2)
                                .saleChannel("tienda online")
                                .courier("SIN COURIER")
                                .paymentState("POR RECAUDAR")
                                .paymentMethod("plin")
                                .orderState("NO HAY STOCK")
                                .observations("falta de stock para cumplir con el pedido")
                                .build();
                        iOrdering.update(5L,requestOrderUpdate3,"gjimenez");
                        // mock stock replenishment
                        List<RequestStockReplenishmentItem> requestStockReplenishmentItemList1 = new ArrayList<RequestStockReplenishmentItem>();
                        RequestStockReplenishmentItem requestStockReplenishmentItem1 =  RequestStockReplenishmentItem.builder()
                                .productSku("A00001")
                                .quantity(8)
                                .build();
                        RequestStockReplenishmentItem requestStockReplenishmentItem2 = RequestStockReplenishmentItem.builder()
                                .productSku("A00003")
                                .quantity(5)
                                .build();
                        requestStockReplenishmentItemList1.add(requestStockReplenishmentItem1);
                        requestStockReplenishmentItemList1.add(requestStockReplenishmentItem2);
                        iStockReplenishment.save(5L,requestStockReplenishmentItemList1,"gjimenez");
                        // mock stock transfer
                        List<RequestStockTransferItem> requestStockTransferItemList1 = new ArrayList<>();
                        requestStockTransferItemList1.add(RequestStockTransferItem.builder()
                                .quantity(15)
                                .supplierProductSerial("A00003B")
                                .build());
                        requestStockTransferItemList1.add(RequestStockTransferItem.builder()
                                .quantity(12)
                                .supplierProductSerial("A00002B")
                                .build());
                        RequestStockTransfer requestStockTransfer1 = RequestStockTransfer.builder()
                                .originWarehouse("luminous")
                                .destinationWarehouse("oikas")
                                .requestStockTransferItemList(requestStockTransferItemList1)
                                .build();

                        iStockTransfer.save(requestStockTransfer1,"gjimenez");
                        // add order item mock
                        RequestOrderItem requestOrderItemAdd = RequestOrderItem.builder()
                                .productSku("B00003")
                                .discount(0.00)
                                .observations("")
                                .quantity(2)
                                .build();
                        iOrderItem.add(3L,requestOrderItemAdd,"fcasas");
                        // delete order item mock
                        iOrderItem.delete(4L,"B00002","fcasas");
                        // update order item mock
                        RequestOrderItem requestOrderItemUpdate = RequestOrderItem.builder()
                                .productSku("B00001")
                                .discount(5.00)
                                .quantity(3)
                                .observations("se adicionan dos unidades al pedido")
                                .build();
                        iOrderItem.update(3L,requestOrderItemUpdate,"fcasas");
                }catch (RuntimeException e){
                        e.printStackTrace();
                        throw new RuntimeException(e.getMessage());
                }
        }

}
