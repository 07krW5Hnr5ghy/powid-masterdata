package com.proyect.masterdata.seeder;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.CountryDTO;
import com.proyect.masterdata.dto.LocationDTO;
import com.proyect.masterdata.dto.request.*;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.*;
import lombok.RequiredArgsConstructor;
import org.springframework.boot.CommandLineRunner;
import org.springframework.core.io.Resource;
import org.springframework.core.io.ResourceLoader;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Component;
import org.springframework.util.StreamUtils;
import org.springframework.web.multipart.MultipartFile;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

@Component
@RequiredArgsConstructor
public class Seeder implements CommandLineRunner {
        // repositories
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
        // services
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
        private final IPurchaseType iPurchaseType;
        private final IStockReplenishment iStockReplenishment;
        private final IStockTransfer iStockTransfer;
        private final IOrderItem iOrderItem;
        private final IPaymentGateway iPaymentGateway;
        private final IMembershipState iMembershipState;
        private final IAccess iAccess;
        private final IRole iRole;
        private final IRoleAccess iRoleAccess;
        private final IUserRole iUserRole;
        private final IUser iUser;
        private final IStore iStore;
        private final IPurchaseDocument iPurchaseDocument;
        private final ICountry iCountry;
        private final ISupplierType iSupplierType;
        private final ICustomerType iCustomerType;
        private final IOrderReturnType iOrderReturnType;
        private final IOrderReturn iOrderReturn;
        private final ResourceLoader resourceLoader;
        private final IAuditEvent iAuditEvent;
        private final ICustomer iCustomer;
        private final IDiscount iDiscount;
        private final IDeliveryPoint iDeliveryPoint;
        @Override
        public void run(String... args) throws Exception {

                try{
                        // example one role and one access

                        Access access = accessRepository
                                .save(new Access(1L, "USER_GET", true, new Date(System.currentTimeMillis()),
                                        new Date(System.currentTimeMillis()), "SISTEMA"));

                        Role role = roleRepository.save(new Role(
                                1L, "ADMINISTRACION", true, new Date(System.currentTimeMillis()),
                                new Date(System.currentTimeMillis()), "SISTEMA"));

                        // department, province and district to create system user

                        Department department = departmentRepository
                                .save(new Department(1L, "SISTEMA", true, new Date(System.currentTimeMillis()),new Date(System.currentTimeMillis()),
                                        "SISTEMA"));

                        Province province = provinceRepository.save(new Province(1L, "SISTEMA", true,
                                new Date(System.currentTimeMillis()), department.getId(), "SISTEMA", department));

                        District district = districtRepository
                                .save(new District(1L, "SISTEMA", true, new Date(System.currentTimeMillis()),new Date(System.currentTimeMillis()),
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
                                new UserRole(1L, adminUser.getId(), role.getId(), adminUser.getUsername(),true,
                                        new Date(System.currentTimeMillis()),new Date(System.currentTimeMillis()),role,adminUser));

                        roleAccessRepository.save(
                                new RoleAccess(1L, role.getId(), access.getId(), "SISTEMA",
                                        new Date(System.currentTimeMillis()),new Date(System.currentTimeMillis()),true,role,access));

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

                        User business1 = userRepository
                                .save(new User(3L, "GJIMENEZ", "GONZALO", "JIMENEZ", "12345678910", "gj@gmail.com",
                                        "CRA 123", "M", "123456789", passwordEncoder.encode("123abc+"), true,
                                        new Date(System.currentTimeMillis()),
                                        new Date(System.currentTimeMillis()), district.getId(), client1.getId(),
                                        "ADMIN1", district, client1));

                        User business2 = userRepository
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

                        iDepartment.save("NO APLICA","ADMIN1");

                        List<LocationDTO> listProvince = iJsonFileReader.filterProvince();

                        for (LocationDTO locationProvince : listProvince) {
                                iProvince.save(locationProvince.getProvince(), "ADMIN1", locationProvince.getDepartment());
                        }

                        iProvince.save("NO APLICA","ADMIN1","NO APLICA");

                        List<LocationDTO> listDistrict = iJsonFileReader.filterDistrict();

                        for (LocationDTO locationDistrict : listDistrict) {
                                iDistrict.save(locationDistrict.getDistrict(), "ADMIN1", locationDistrict.getProvince());
                        }

                        iDistrict.save("NO APLICA","ADMIN1","NO APLICA");

                        // mock countries

                        List<CountryDTO> listCountry = iJsonFileReader.filterCountry();

                        for(CountryDTO country : listCountry){
                                iCountry.save(country.getValue(),"ADMIN1");
                        }

                        // audit
                        iAuditEvent.save("ACTIVATE_ACCESS","ADMIN1");
                        iAuditEvent.save("ACTIVATE_BRAND","ADMIN1");
                        iAuditEvent.save("ACTIVATE_CANCELLATION_REASON","ADMIN1");
                        iAuditEvent.save("ACTIVATE_CATEGORY","ADMIN1");
                        iAuditEvent.save("ACTIVATE_CATEGORY_PRODUCT","ADMIN1");
                        iAuditEvent.save("ACTIVATE_CLIENT","ADMIN1");
                        iAuditEvent.save("ACTIVATE_CLOSING_CHANNEL","ADMIN1");
                        iAuditEvent.save("ACTIVATE_COLOR","ADMIN1");
                        iAuditEvent.save("ACTIVATE_COURIER","ADMIN1");
                        iAuditEvent.save("ACTIVATE_CUSTOMER_TYPE","ADMIN1");
                        iAuditEvent.save("ACTIVATE_DEPARTMENT","ADMIN1");
                        iAuditEvent.save("ACTIVATE_DISTRICT","ADMIN1");
                        iAuditEvent.save("ACTIVATE_DEMO_ACCOUNT","ADMIN1");
                        iAuditEvent.save("ACTIVATE_ENTRY_CHANNEL","ADMIN1");
                        iAuditEvent.save("ACTIVATE_MANAGEMENT_TYPE","ADMIN1");
                        iAuditEvent.save("ACTIVATE_MEMBERSHIP_STATE","ADMIN1");
                        iAuditEvent.save("ACTIVATE_MODEL","ADMIN1");
                        iAuditEvent.save("ACTIVATE_MODULE","ADMIN1");
                        iAuditEvent.save("ACTIVATE_ORDER_PAYMENT_METHOD","ADMIN1");
                        iAuditEvent.save("ACTIVATE_ORDER_PAYMENT_STATE","ADMIN1");
                        iAuditEvent.save("ACTIVATE_ORDER_RETURN_ITEM","ADMIN1");
                        iAuditEvent.save("ACTIVATE_ORDER_RETURN_TYPE","ADMIN1");
                        iAuditEvent.save("ACTIVATE_ORDER_STATE","ADMIN1");
                        iAuditEvent.save("ACTIVATE_ORDER_STOCK_ITEM","ADMIN1");
                        iAuditEvent.save("ACTIVATE_PAYMENT_GATEWAY","ADMIN1");
                        iAuditEvent.save("ACTIVATE_PRODUCT","ADMIN1");
                        iAuditEvent.save("ACTIVATE_PURCHASE_DOCUMENT","ADMIN1");
                        iAuditEvent.save("ACTIVATE_PURCHASE_ITEM","ADMIN1");
                        iAuditEvent.save("ACTIVATE_PROVINCE","ADMIN1");
                        iAuditEvent.save("ACTIVATE_ROLE","ADMIN1");
                        iAuditEvent.save("ACTIVATE_ROLE_ACCESS","ADMIN1");
                        iAuditEvent.save("ACTIVATE_SALE_CHANNEL","ADMIN1");
                        iAuditEvent.save("ACTIVATE_PURCHASE_TYPE","ADMIN1");
                        iAuditEvent.save("ACTIVATE_SIZE","ADMIN1");
                        iAuditEvent.save("ACTIVATE_SIZE_TYPE","ADMIN1");
                        iAuditEvent.save("ACTIVATE_STOCK_REPLENISHMENT_ITEM","ADMIN1");
                        iAuditEvent.save("ACTIVATE_STOCK_TRANSACTION_TYPE","ADMIN1");
                        iAuditEvent.save("ACTIVATE_STORE","ADMIN1");
                        iAuditEvent.save("ACTIVATE_STORE_TYPE","ADMIN1");
                        iAuditEvent.save("ACTIVATE_SUBSCRIPTION","ADMIN1");
                        iAuditEvent.save("ACTIVATE_SUPPLIER","ADMIN1");
                        iAuditEvent.save("ACTIVATE_SUPPLIER_PRODUCT","ADMIN1");
                        iAuditEvent.save("ACTIVATE_SUPPLIER_TYPE","ADMIN1");
                        iAuditEvent.save("ACTIVATE_UNIT","ADMIN1");
                        iAuditEvent.save("ACTIVATE_UNIT_TYPE","ADMIN1");
                        iAuditEvent.save("ACTIVATE_USER","ADMIN1");
                        iAuditEvent.save("ACTIVATE_USER_ROLE","ADMIN1");
                        iAuditEvent.save("ACTIVATE_WAREHOUSE","ADMIN1");
                        iAuditEvent.save("ADD_ACCESS","ADMIN1");
                        iAuditEvent.save("ADD_BRAND","ADMIN1");
                        iAuditEvent.save("ADD_CANCELLATION_REASON","ADMIN1");
                        iAuditEvent.save("ADD_CANCELLED_ORDER","ADMIN1");
                        iAuditEvent.save("ADD_CATEGORY","ADMIN1");
                        iAuditEvent.save("ADD_CATEGORY_PRODUCT","ADMIN1");
                        iAuditEvent.save("ADD_CLOSING_CHANNEL","ADMIN1");
                        iAuditEvent.save("ADD_COLOR","ADMIN1");
                        iAuditEvent.save("ADD_COUNTRY","ADMIN1");
                        iAuditEvent.save("ADD_COURIER","ADMIN1");
                        iAuditEvent.save("ADD_COURIER_PICTURE","ADMIN1");
                        iAuditEvent.save("ADD_CUSTOMER","ADMIN1");
                        iAuditEvent.save("ADD_CUSTOMER_TYPE","ADMIN1");
                        iAuditEvent.save("ADD_DELIVERY_POINT","ADMIN1");
                        iAuditEvent.save("ADD_DEPARTMENT","ADMIN1");
                        iAuditEvent.save("ADD_DISCOUNT","ADMIN1");
                        iAuditEvent.save("ADD_DISTRICT","ADMIN1");
                        iAuditEvent.save("ADD_ENTRY_CHANNEL","ADMIN1");
                        iAuditEvent.save("ADD_GENERAL_STOCK","ADMIN1");
                        iAuditEvent.save("ADD_MANAGEMENT_TYPE","ADMIN1");
                        iAuditEvent.save("ADD_MEMBERSHIP","ADMIN1");
                        iAuditEvent.save("ADD_MEMBERSHIP_PAYMENT","ADMIN1");
                        iAuditEvent.save("ADD_MEMBERSHIP_STATE","ADMIN1");
                        iAuditEvent.save("ADD_MERCADO_PAGO_PAYMENT","ADMIN1");
                        iAuditEvent.save("ADD_MODEL","ADMIN1");
                        iAuditEvent.save("ADD_MODULE","ADMIN1");
                        iAuditEvent.save("ADD_ORDER","ADMIN1");
                        iAuditEvent.save("ADD_ORDER_ITEM","ADMIN1");
                        iAuditEvent.save("ADD_ORDER_PAYMENT_METHOD","ADMIN1");
                        iAuditEvent.save("ADD_ORDER_PAYMENT_RECEIPT","ADMIN1");
                        iAuditEvent.save("ADD_ORDER_PAYMENT_STATE","ADMIN1");
                        iAuditEvent.save("ADD_ORDER_RETURN","ADMIN1");
                        iAuditEvent.save("ADD_ORDER_RETURN_ITEM","ADMIN1");
                        iAuditEvent.save("ADD_ORDER_RETURN_EXCEL","ADMIN1");
                        iAuditEvent.save("ADD_ORDER_RETURN_TYPE","ADMIN1");
                        iAuditEvent.save("ADD_ORDER_STATE","ADMIN1");
                        iAuditEvent.save("ADD_ORDER_STOCK","ADMIN1");
                        iAuditEvent.save("ADD_ORDER_STOCK_EXCEL","ADMIN1");
                        iAuditEvent.save("ADD_ORDER_STOCK_ITEM","ADMIN1");
                        iAuditEvent.save("ADD_PAYMENT_GATEWAY","ADMIN1");
                        iAuditEvent.save("ADD_PRODUCT","ADMIN1");
                        iAuditEvent.save("ADD_PRODUCT_EXCEL","ADMIN1");
                        iAuditEvent.save("ADD_PRODUCT_PICTURE","ADMIN1");
                        iAuditEvent.save("ADD_PRODUCT_PRICE","ADMIN1");
                        iAuditEvent.save("ADD_PROVINCE","ADMIN1");
                        iAuditEvent.save("ADD_PURCHASE","ADMIN1");
                        iAuditEvent.save("ADD_PURCHASE_DOCUMENT","ADMIN1");
                        iAuditEvent.save("ADD_PURCHASE_EXCEL","ADMIN1");
                        iAuditEvent.save("ADD_PURCHASE_ITEM","ADMIN1");
                        iAuditEvent.save("ADD_ROLE","ADMIN1");
                        iAuditEvent.save("ADD_ROLE_ACCESS","ADMIN1");
                        iAuditEvent.save("ADD_SALE_CHANNEL","ADMIN1");
                        iAuditEvent.save("ADD_PURCHASE_TYPE","ADMIN1");
                        iAuditEvent.save("ADD_SIZE","ADMIN1");
                        iAuditEvent.save("ADD_SIZE_TYPE","ADMIN1");
                        iAuditEvent.save("ADD_STOCK_REPLENISHMENT_EXCEL","ADMIN1");
                        iAuditEvent.save("ADD_STOCK_RETURN_EXCEL","ADMIN1");
                        iAuditEvent.save("ADD_STOCK_TRANSFER_EXCEL","ADMIN1");
                        iAuditEvent.save("ADD_STOCK_REPLENISHMENT","ADMIN1");
                        iAuditEvent.save("ADD_STOCK_REPLENISHMENT_ITEM","ADMIN1");
                        iAuditEvent.save("ADD_STOCK_RETURN","ADMIN1");
                        iAuditEvent.save("ADD_STOCK_RETURN_ITEM","ADMIN1");
                        iAuditEvent.save("ADD_STOCK_TRANSACTION","ADMIN1");
                        iAuditEvent.save("ADD_STOCK_TRANSACTION_ITEM","ADMIN1");
                        iAuditEvent.save("ADD_STOCK_TRANSACTION_TYPE","ADMIN1");
                        iAuditEvent.save("ADD_STOCK_TRANSFER","ADMIN1");
                        iAuditEvent.save("ADD_STOCK_TRANSFER_ITEM","ADMIN1");
                        iAuditEvent.save("ADD_STORE","ADMIN1");
                        iAuditEvent.save("ADD_STORE_TYPE","ADMIN1");
                        iAuditEvent.save("ADD_SUBSCRIPTION","ADMIN1");
                        iAuditEvent.save("ADD_SUPPLIER","ADMIN1");
                        iAuditEvent.save("ADD_SUPPLIER_PRODUCT","ADMIN1");
                        iAuditEvent.save("ADD_SUPPLIER_PRODUCT_EXCEL","ADMIN1");
                        iAuditEvent.save("ADD_SUPPLIER_TYPE","ADMIN1");
                        iAuditEvent.save("ADD_UNIT","ADMIN1");
                        iAuditEvent.save("ADD_UNIT_TYPE","ADMIN1");
                        iAuditEvent.save("ADD_USER","ADMIN1");
                        iAuditEvent.save("ADD_USER_ROLE","ADMIN1");
                        iAuditEvent.save("ADD_WAREHOUSE","ADMIN1");
                        iAuditEvent.save("ADD_WAREHOUSE_STOCK","ADMIN1");
                        iAuditEvent.save("DELETE_ACCESS","ADMIN1");
                        iAuditEvent.save("DELETE_BRAND","ADMIN1");
                        iAuditEvent.save("DELETE_CANCELLATION_REASON","ADMIN1");
                        iAuditEvent.save("DELETE_CATEGORY","ADMIN1");
                        iAuditEvent.save("DELETE_CATEGORY_PRODUCT","ADMIN1");
                        iAuditEvent.save("DELETE_CLIENT","ADMIN1");
                        iAuditEvent.save("DELETE_CLOSING_CHANNEL","ADMIN1");
                        iAuditEvent.save("DELETE_COLOR","ADMIN1");
                        iAuditEvent.save("DELETE_COURIER","ADMIN1");
                        iAuditEvent.save("DELETE_CUSTOMER_TYPE","ADMIN1");
                        iAuditEvent.save("DELETE_DEPARTMENT","ADMIN1");
                        iAuditEvent.save("DELETE_DISTRICT","ADMIN1");
                        iAuditEvent.save("DELETE_ENTRY_CHANNEL","ADMIN1");
                        iAuditEvent.save("DELETE_GENERAL_STOCK","ADMIN1");
                        iAuditEvent.save("DELETE_MANAGEMENT_TYPE","ADMIN1");
                        iAuditEvent.save("DELETE_MEMBERSHIP","ADMIN1");
                        iAuditEvent.save("DELETE_MEMBERSHIP_STATE","ADMIN1");
                        iAuditEvent.save("DELETE_MODEL","ADMIN1");
                        iAuditEvent.save("DELETE_MODULE","ADMIN1");
                        iAuditEvent.save("DELETE_ORDER_ITEM","ADMIN1");
                        iAuditEvent.save("DELETE_ORDER_PAYMENT_METHOD","ADMIN1");
                        iAuditEvent.save("DELETE_ORDER_PAYMENT_STATE","ADMIN1");
                        iAuditEvent.save("DELETE_ORDER_RETURN_ITEM","ADMIN1");
                        iAuditEvent.save("DELETE_ORDER_RETURN_TYPE","ADMIN1");
                        iAuditEvent.save("DELETE_ORDER_STATE","ADMIN1");
                        iAuditEvent.save("DELETE_ORDER_STOCK_ITEM","ADMIN1");
                        iAuditEvent.save("DELETE_PAYMENT_GATEWAY","ADMIN1");
                        iAuditEvent.save("DELETE_PRODUCT","ADMIN1");
                        iAuditEvent.save("DELETE_PRODUCT_PRICE","ADMIN1");
                        iAuditEvent.save("DELETE_PROVINCE","ADMIN1");
                        iAuditEvent.save("DELETE_PURCHASE_DOCUMENT","ADMIN1");
                        iAuditEvent.save("DELETE_PURCHASE_ITEM","ADMIN1");
                        iAuditEvent.save("DELETE_ROLE","ADMIN1");
                        iAuditEvent.save("DELETE_ROLE_ACCESS","ADMIN1");
                        iAuditEvent.save("DELETE_SALE_CHANNEL","ADMIN1");
                        iAuditEvent.save("DELETE_PURCHASE_TYPE","ADMIN1");
                        iAuditEvent.save("DELETE_SIZE","ADMIN1");
                        iAuditEvent.save("DELETE_SIZE_TYPE","ADMIN1");
                        iAuditEvent.save("DELETE_STOCK_REPLENISHMENT_ITEM","ADMIN1");
                        iAuditEvent.save("DELETE_STOCK_TRANSACTION_TYPE","ADMIN1");
                        iAuditEvent.save("DELETE_STORE","ADMIN1");
                        iAuditEvent.save("DELETE_STORE_TYPE","ADMIN1");
                        iAuditEvent.save("DELETE_SUBSCRIPTION","ADMIN1");
                        iAuditEvent.save("DELETE_SUPPLIER","ADMIN1");
                        iAuditEvent.save("DELETE_SUPPLIER_PRODUCT","ADMIN1");
                        iAuditEvent.save("DELETE_SUPPLIER_TYPE","ADMIN1");
                        iAuditEvent.save("DELETE_UNIT","ADMIN1");
                        iAuditEvent.save("DELETE_UNIT_TYPE","ADMIN1");
                        iAuditEvent.save("DELETE_USER","ADMIN1");
                        iAuditEvent.save("DELETE_USER_ROLE","ADMIN1");
                        iAuditEvent.save("DELETE_WAREHOUSE","ADMIN1");
                        iAuditEvent.save("DELETE_WAREHOUSE_STOCK","ADMIN1");
                        iAuditEvent.save("LOG_IN","ADMIN1");
                        iAuditEvent.save("LOG_OUT","ADMIN1");
                        iAuditEvent.save("REGISTER_CLIENT","ADMIN1");
                        iAuditEvent.save("SEND_MERCADO_PAGO_PAYMENT","ADMIN1");
                        iAuditEvent.save("SEND_SUBSCRIPTION_PAYMENT","ADMIN1");
                        iAuditEvent.save("UPDATE_CATEGORY","ADMIN1");
                        iAuditEvent.save("UPDATE_CATEGORY_PRODUCT","ADMIN1");
                        iAuditEvent.save("UPDATE_CLIENT","ADMIN1");
                        iAuditEvent.save("UPDATE_COURIER_ORDER","ADMIN1");
                        iAuditEvent.save("UPDATE_MODULE","ADMIN1");
                        iAuditEvent.save("UPDATE_ORDER","ADMIN1");
                        iAuditEvent.save("UPDATE_ORDER_ITEM","ADMIN1");
                        iAuditEvent.save("UPDATE_ORDER_RETURN_ITEM","ADMIN1");
                        iAuditEvent.save("UPDATE_ORDER_STOCK_ITEM","ADMIN1");
                        iAuditEvent.save("UPDATE_PRODUCT","ADMIN1");
                        iAuditEvent.save("UPDATE_STOCK_REPLENISHMENT_ITEM","ADMIN1");
                        iAuditEvent.save("UPDATE_STORE","ADMIN1");
                        iAuditEvent.save("UPDATE_USER","ADMIN1");
                        // supplier types
                        iSupplierType.save("INTERNO","ADMIN1");
                        iSupplierType.save("DISTRIBUIDOR","ADMIN1");

                        // access
                        iAccess.save("ACCESS_POST","ADMIN1");
                        iAccess.save("ACCESS_DELETE","ADMIN1");
                        iAccess.save("ACCESS_GET","ADMIN1");
                        iAccess.save("ACCESS_PUT","ADMIN1");
                        iAccess.save("BRAND_POST","ADMIN1");
                        iAccess.save("BRAND_DELETE","ADMIN1");
                        iAccess.save("BRAND_GET","ADMIN1");
                        iAccess.save("BRAND_PUT","ADMIN1");
                        iAccess.save("CANCELLATION_REASON_POST","ADMIN1");
                        iAccess.save("CANCELLATION_REASON_GET","ADMIN1");
                        iAccess.save("CANCELLATION_REASON_PUT","ADMIN1");
                        iAccess.save("CANCELLED_ORDER_POST","ADMIN1");
                        iAccess.save("CANCELLED_ORDER_GET","ADMIN1");
                        iAccess.save("CATEGORY_GET","ADMIN1");
                        iAccess.save("CATEGORY_POST","ADMIN1");
                        iAccess.save("CATEGORY_PUT","ADMIN1");
                        iAccess.save("CATEGORY_DELETE","ADMIN1");
                        iAccess.save("CATEGORY_PRODUCT_POST","ADMIN1");
                        iAccess.save("CATEGORY_PRODUCT_GET","ADMIN1");
                        iAccess.save("CLIENT_GET","ADMIN1");
                        iAccess.save("CLIENT_POST","ADMIN1");
                        iAccess.save("CLIENT_PUT","ADMIN1");
                        iAccess.save("CLIENT_DELETE","ADMIN1");
                        iAccess.save("CLOSING_CHANNEL_POST","ADMIN1");
                        iAccess.save("COLOR_POST","ADMIN1");
                        iAccess.save("COLOR_GET","ADMIN1");
                        iAccess.save("COLOR_DELETE","ADMIN1");
                        iAccess.save("COURIER_POST","ADMIN1");
                        iAccess.save("COURIER_PUT","ADMIN1");
                        iAccess.save("COURIER_GET","ADMIN1");
                        iAccess.save("DEPARTMENT_GET","ADMIN1");
                        iAccess.save("DEPARTMENT_POST","ADMIN1");
                        iAccess.save("DEPARTMENT_DELETE","ADMIN1");
                        iAccess.save("DISTRICT_POST","ADMIN1");
                        iAccess.save("DISTRICT_DELETE","ADMIN1");
                        iAccess.save("ENTRY_CHANNEL_POST","ADMIN1");
                        iAccess.save("GENERAL_STOCK_GET","ADMIN1");
                        iAccess.save("MEMBERSHIP_GET","ADMIN1");
                        iAccess.save("MEMBERSHIP_PAYMENT_GET","ADMIN1");
                        iAccess.save("MODEL_GET","ADMIN1");
                        iAccess.save("MODEL_POST","ADMIN1");
                        iAccess.save("MODEL_DELETE","ADMIN1");
                        iAccess.save("MODULE_GET","ADMIN1");
                        iAccess.save("MODULE_POST","ADMIN1");
                        iAccess.save("MODULE_PUT","ADMIN1");
                        iAccess.save("MODULE_DELETE","ADMIN1");
                        iAccess.save("ONBOARD_GET","ADMIN1");
                        iAccess.save("ORDER_GET","ADMIN1");
                        iAccess.save("ORDER_POST","ADMIN1");
                        iAccess.save("ORDER_PUT","ADMIN1");
                        iAccess.save("ORDER_ITEM_GET","ADMIN1");
                        iAccess.save("ORDER_ITEM_POST","ADMIN1");
                        iAccess.save("ORDER_ITEM_DELETE","ADMIN1");
                        iAccess.save("ORDER_ITEM_PUT","ADMIN1");
                        iAccess.save("ORDER_STOCK_GET","ADMIN1");
                        iAccess.save("ORDER_STOCK_POST","ADMIN1");
                        iAccess.save("ORDER_STOCK_ITEM_GET","ADMIN1");
                        iAccess.save("ORDER_PAYMENT_METHOD_POST","ADMIN1");
                        iAccess.save("ORDER_PAYMENT_METHOD_GET","ADMIN1");
                        iAccess.save("ORDER_PAYMENT_METHOD_DELETE","ADMIN1");
                        iAccess.save("ORDER_PAYMENT_STATE_GET","ADMIN1");
                        iAccess.save("ORDER_PAYMENT_STATE_POST","ADMIN1");
                        iAccess.save("ORDER_PAYMENT_STATE_DELETE","ADMIN1");
                        iAccess.save("ORDER_STATE_GET","ADMIN1");
                        iAccess.save("ORDER_STATE_POST","ADMIN1");
                        iAccess.save("ORDER_STATE_DELETE","ADMIN1");
                        iAccess.save("PAYMENT_GATEWAY_POST","ADMIN1");
                        iAccess.save("PRODUCT_GET","ADMIN1");
                        iAccess.save("PRODUCT_POST","ADMIN1");
                        iAccess.save("PRODUCT_DELETE","ADMIN1");
                        iAccess.save("PRODUCT_PRICE_POST","ADMIN1");
                        iAccess.save("PROVINCE_GET","ADMIN1");
                        iAccess.save("PROVINCE_POST","ADMIN1");
                        iAccess.save("PROVINCE_DELETE","ADMIN1");
                        iAccess.save("PURCHASE_GET","ADMIN1");
                        iAccess.save("PURCHASE_POST","ADMIN1");
                        iAccess.save("PURCHASE_DOCUMENT_POST","ADMIN1");
                        iAccess.save("PURCHASE_DOCUMENT_DELETE","ADMIN1");
                        iAccess.save("PURCHASE_DOCUMENT_GET","ADMIN1");
                        iAccess.save("PURCHASE_ITEM_GET","ADMIN1");
                        iAccess.save("ROLE_POST","ADMIN1");
                        iAccess.save("ROLE_GET","ADMIN1");
                        iAccess.save("ROLE_DELETE","ADMIN1");
                        iAccess.save("ROLE_PUT","ADMIN1");
                        iAccess.save("ROLE_ACCESS_GET","ADMIN1");
                        iAccess.save("ROLE_ACCESS_POST","ADMIN1");
                        iAccess.save("ROLE_ACCESS_PUT","ADMIN1");
                        iAccess.save("ROLE_ACCESS_DELETE","ADMIN1");
                        iAccess.save("SALE_CHANNEL_POST","ADMIN1");
                        iAccess.save("SALE_CHANNEL_DELETE","ADMIN1");
                        iAccess.save("PURCHASE_TYPE_POST","ADMIN1");
                        iAccess.save("PURCHASE_TYPE_GET","ADMIN1");
                        iAccess.save("SIZE_GET","ADMIN1");
                        iAccess.save("SIZE_POST","ADMIN1");
                        iAccess.save("SIZE_DELETE","ADMIN1");
                        iAccess.save("SIZE_TYPE_GET","ADMIN1");
                        iAccess.save("SIZE_TYPE_POST","ADMIN1");
                        iAccess.save("SIZE_TYPE_DELETE","ADMIN1");
                        iAccess.save("STOCK_REPLENISHMENT_POST","ADMIN1");
                        iAccess.save("STOCK_REPLENISHMENT_GET","ADMIN1");
                        iAccess.save("STOCK_REPLENISHMENT_ITEM_GET","ADMIN1");
                        iAccess.save("STOCK_RETURN_POST","ADMIN1");
                        iAccess.save("STOCK_RETURN_GET","ADMIN1");
                        iAccess.save("STOCK_RETURN_ITEM_GET","ADMIN1");
                        iAccess.save("STOCK_TRANSACTION_GET","ADMIN1");
                        iAccess.save("STOCK_TRANSACTION_ITEM_GET","ADMIN1");
                        iAccess.save("STOCK_TRANSACTION_TYPE_GET","ADMIN1");
                        iAccess.save("STOCK_TRANSACTION_TYPE_POST","ADMIN1");
                        iAccess.save("STOCK_TRANSACTION_TYPE_DELETE","ADMIN1");
                        iAccess.save("STOCK_TRANSFER_POST","ADMIN1");
                        iAccess.save("STOCK_TRANSFER_GET","ADMIN1");
                        iAccess.save("STOCK_TRANSFER_ITEM_GET","ADMIN1");
                        iAccess.save("STORE_GET","ADMIN1");
                        iAccess.save("STORE_POST","ADMIN1");
                        iAccess.save("STORE_PUT","ADMIN1");
                        iAccess.save("STORE_DELETE","ADMIN1");
                        iAccess.save("STORE_TYPE_POST","ADMIN1");
                        iAccess.save("SUBSCRIPTION_GET","ADMIN1");
                        iAccess.save("SUBSCRIPTION_POST","ADMIN1");
                        iAccess.save("SUBSCRIPTION_PAYMENT_POST","ADMIN1");
                        iAccess.save("SUPPLIER_GET","ADMIN1");
                        iAccess.save("SUPPLIER_POST","ADMIN1");
                        iAccess.save("SUPPLIER_DELETE","ADMIN1");
                        iAccess.save("SUPPLIER_PRODUCT_GET","ADMIN1");
                        iAccess.save("SUPPLIER_PRODUCT_POST","ADMIN1");
                        iAccess.save("SUPPLIER_PRODUCT_DELETE","ADMIN1");
                        iAccess.save("UNIT_GET","ADMIN1");
                        iAccess.save("UNIT_POST","ADMIN1");
                        iAccess.save("UNIT_DELETE","ADMIN1");
                        iAccess.save("UNIT_TYPE_GET","ADMIN1");
                        iAccess.save("UNIT_TYPE_POST","ADMIN1");
                        iAccess.save("UNIT_TYPE_DELETE","ADMIN1");
                        iAccess.save("USER_POST","ADMIN1");
                        iAccess.save("USER_PUT","ADMIN1");
                        iAccess.save("USER_DELETE","ADMIN1");
                        iAccess.save("USER_ROLE_POST","ADMIN1");
                        iAccess.save("WAREHOUSE_GET","ADMIN1");
                        iAccess.save("WAREHOUSE_POST","ADMIN1");
                        iAccess.save("WAREHOUSE_STOCK_GET","ADMIN1");
                        // roles
                        iRole.save("NEGOCIO","ADMIN1");
                        iRole.save("VENTAS","ADMIN1");
                        iRole.save("STOCK","ADMIN1");
                        iRole.save("SERVICIO_CLIENTE","ADMIN1");
                        iRole.save("COURIER","ADMIN1");
                        iRole.save("MARKETING","ADMIN1");
                        // roles by access
                        iRoleAccess.save("VENTAS","BRAND_GET","ADMIN1");
                        iRoleAccess.save("VENTAS","CANCELLATION_REASON_GET","ADMIN1");
                        iRoleAccess.save("VENTAS","CANCELLED_ORDER_GET","ADMIN1");
                        iRoleAccess.save("VENTAS","CANCELLED_ORDER_POST","ADMIN1");
                        iRoleAccess.save("VENTAS","COURIER_GET","ADMIN1");
                        iRoleAccess.save("VENTAS","MODEL_GET","ADMIN1");
                        iRoleAccess.save("VENTAS","ORDER_GET","ADMIN1");
                        iRoleAccess.save("VENTAS","ORDER_POST","ADMIN1");
                        iRoleAccess.save("VENTAS","ORDER_PUT","ADMIN1");
                        iRoleAccess.save("VENTAS","ORDER_ITEM_GET","ADMIN1");
                        iRoleAccess.save("VENTAS","ORDER_ITEM_POST","ADMIN1");
                        iRoleAccess.save("VENTAS","ORDER_ITEM_PUT","ADMIN1");
                        iRoleAccess.save("VENTAS","ORDER_ITEM_DELETE","ADMIN1");
                        iRoleAccess.save("VENTAS","ORDER_PAYMENT_METHOD_GET","ADMIN1");
                        iRoleAccess.save("VENTAS","ORDER_PAYMENT_STATE_GET","ADMIN1");
                        iRoleAccess.save("VENTAS","ORDER_STATE_GET","ADMIN1");
                        iRoleAccess.save("VENTAS","PRODUCT_GET","ADMIN1");
                        iRoleAccess.save("VENTAS","STORE_GET","ADMIN1");
                        iRoleAccess.save("SERVICIO_CLIENTE","BRAND_GET","ADMIN1");
                        iRoleAccess.save("SERVICIO_CLIENTE","CANCELLATION_REASON_GET","ADMIN1");
                        iRoleAccess.save("SERVICIO_CLIENTE","CANCELLED_ORDER_GET","ADMIN1");
                        iRoleAccess.save("SERVICIO_CLIENTE","CANCELLED_ORDER_POST","ADMIN1");
                        iRoleAccess.save("SERVICIO_CLIENTE","MODEL_GET","ADMIN1");
                        iRoleAccess.save("SERVICIO_CLIENTE","COURIER_GET","ADMIN1");
                        iRoleAccess.save("SERVICIO_CLIENTE","ORDER_GET","ADMIN1");
                        iRoleAccess.save("SERVICIO_CLIENTE","ORDER_PUT","ADMIN1");
                        iRoleAccess.save("SERVICIO_CLIENTE","ORDER_ITEM_GET","ADMIN1");
                        iRoleAccess.save("SERVICIO_CLIENTE","ORDER_ITEM_POST","ADMIN1");
                        iRoleAccess.save("SERVICIO_CLIENTE","ORDER_ITEM_PUT","ADMIN1");
                        iRoleAccess.save("SERVICIO_CLIENTE","ORDER_ITEM_DELETE","ADMIN1");
                        iRoleAccess.save("SERVICIO_CLIENTE","ORDER_PAYMENT_METHOD_GET","ADMIN1");
                        iRoleAccess.save("SERVICIO_CLIENTE","ORDER_PAYMENT_STATE_GET","ADMIN1");
                        iRoleAccess.save("SERVICIO_CLIENTE","ORDER_STATE_GET","ADMIN1");
                        iRoleAccess.save("SERVICIO_CLIENTE","PRODUCT_GET","ADMIN1");
                        iRoleAccess.save("SERVICIO_CLIENTE","STORE_GET","ADMIN1");
                        iRoleAccess.save("MARKETING","BRAND_GET","ADMIN1");
                        iRoleAccess.save("MARKETING","BRAND_POST","ADMIN1");
                        iRoleAccess.save("MARKETING","BRAND_DELETE","ADMIN1");
                        iRoleAccess.save("MARKETING","BRAND_PUT","ADMIN1");
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
                        iRoleAccess.save("MARKETING","UNIT_TYPE_GET","ADMIN1");
                        iRoleAccess.save("STOCK","BRAND_GET","ADMIN1");
                        iRoleAccess.save("STOCK","COLOR_GET","ADMIN1");
                        iRoleAccess.save("STOCK","GENERAL_STOCK_GET","ADMIN1");
                        iRoleAccess.save("STOCK","MODEL_GET","ADMIN1");
                        iRoleAccess.save("STOCK","ORDER_GET","ADMIN1");
                        iRoleAccess.save("STOCK","ORDER_ITEM_GET","ADMIN1");
                        iRoleAccess.save("STOCK","ORDER_STOCK_GET","ADMIN1");
                        iRoleAccess.save("STOCK","ORDER_STOCK_POST","ADMIN1");
                        iRoleAccess.save("STOCK","ORDER_STOCK_ITEM_GET","ADMIN1");
                        iRoleAccess.save("STOCK","PURCHASE_GET","ADMIN1");
                        iRoleAccess.save("STOCK","PURCHASE_POST","ADMIN1");
                        iRoleAccess.save("STOCK","PURCHASE_ITEM_GET","ADMIN1");
                        iRoleAccess.save("STOCK","PURCHASE_TYPE_GET","ADMIN1");
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
                        iRoleAccess.save("STOCK","UNIT_TYPE_GET","ADMIN1");
                        iRoleAccess.save("STOCK","WAREHOUSE_GET","ADMIN1");
                        iRoleAccess.save("STOCK","WAREHOUSE_POST","ADMIN1");
                        iRoleAccess.save("COURIER","COURIER_PUT","ADMIN1");
                        iRoleAccess.save("NEGOCIO","BRAND_GET","ADMIN1");
                        iRoleAccess.save("NEGOCIO","CLIENT_GET","ADMIN1");
                        iRoleAccess.save("NEGOCIO","COLOR_GET","ADMIN1");
                        iRoleAccess.save("NEGOCIO","CANCELLED_ORDER_GET","ADMIN1");
                        iRoleAccess.save("NEGOCIO","COURIER_GET","ADMIN1");
                        iRoleAccess.save("NEGOCIO","COURIER_POST","ADMIN1");
                        iRoleAccess.save("NEGOCIO","GENERAL_STOCK_GET","ADMIN1");
                        iRoleAccess.save("NEGOCIO","MEMBERSHIP_GET","ADMIN1");
                        iRoleAccess.save("NEGOCIO","MEMBERSHIP_PAYMENT_GET","ADMIN1");
                        iRoleAccess.save("NEGOCIO","MODEL_GET","ADMIN1");
                        iRoleAccess.save("NEGOCIO","ORDER_GET","ADMIN1");
                        iRoleAccess.save("NEGOCIO","ORDER_ITEM_GET","ADMIN1");
                        iRoleAccess.save("NEGOCIO","ORDER_STOCK_GET","ADMIN1");
                        iRoleAccess.save("NEGOCIO","ORDER_STOCK_ITEM_GET","ADMIN1");
                        iRoleAccess.save("NEGOCIO","ORDER_PAYMENT_METHOD_GET","ADMIN1");
                        iRoleAccess.save("NEGOCIO","ORDER_PAYMENT_STATE_GET","ADMIN1");
                        iRoleAccess.save("NEGOCIO","ORDER_STATE_GET","ADMIN1");
                        iRoleAccess.save("NEGOCIO","PRODUCT_GET","ADMIN1");
                        iRoleAccess.save("NEGOCIO","PURCHASE_GET","ADMIN1");
                        iRoleAccess.save("NEGOCIO","PURCHASE_ITEM_GET","ADMIN1");
                        iRoleAccess.save("NEGOCIO","ROLE_GET","ADMIN1");
                        iRoleAccess.save("NEGOCIO","SIZE_GET","ADMIN1");
                        iRoleAccess.save("NEGOCIO","SIZE_TYPE_GET","ADMIN1");
                        iRoleAccess.save("NEGOCIO","STOCK_REPLENISHMENT_GET","ADMIN1");
                        iRoleAccess.save("NEGOCIO","STOCK_REPLENISHMENT_ITEM_GET","ADMIN1");
                        iRoleAccess.save("NEGOCIO","STOCK_RETURN_GET","ADMIN1");
                        iRoleAccess.save("NEGOCIO","STOCK_RETURN_ITEM_GET","ADMIN1");
                        iRoleAccess.save("NEGOCIO","STOCK_TRANSACTION_GET","ADMIN1");
                        iRoleAccess.save("NEGOCIO","STOCK_TRANSACTION_ITEM_GET","ADMIN1");
                        iRoleAccess.save("NEGOCIO","STOCK_TRANSFER_GET","ADMIN1");
                        iRoleAccess.save("NEGOCIO","STOCK_TRANSFER_ITEM_GET","ADMIN1");
                        iRoleAccess.save("NEGOCIO","STORE_GET","ADMIN1");
                        iRoleAccess.save("NEGOCIO","STORE_PUT","ADMIN1");
                        iRoleAccess.save("NEGOCIO","STORE_POST","ADMIN1");
                        iRoleAccess.save("NEGOCIO","STORE_DELETE","ADMIN1");
                        iRoleAccess.save("NEGOCIO","SUBSCRIPTION_GET","ADMIN1");
                        iRoleAccess.save("NEGOCIO","SUBSCRIPTION_PAYMENT_POST","ADMIN1");
                        iRoleAccess.save("NEGOCIO","SUPPLIER_GET","ADMIN1");
                        iRoleAccess.save("NEGOCIO","UNIT_GET","ADMIN1");
                        iRoleAccess.save("NEGOCIO","UNIT_TYPE_GET","ADMIN1");
                        iRoleAccess.save("NEGOCIO","USER_GET","ADMIN1");
                        iRoleAccess.save("NEGOCIO","USER_POST","ADMIN1");
                        iRoleAccess.save("NEGOCIO","WAREHOUSE_GET","ADMIN1");
                        iRoleAccess.save("NEGOCIO","WAREHOUSE_STOCK_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","ACCESS_POST","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","ACCESS_DELETE","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","ACCESS_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","ACCESS_PUT","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","BRAND_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","CANCELLED_ORDER_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","CANCELLATION_REASON_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","CANCELLATION_REASON_POST","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","CANCELLATION_REASON_PUT","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","CATEGORY_POST","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","CATEGORY_DELETE","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","CATEGORY_PUT","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","CLIENT_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","CLIENT_POST","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","CLIENT_DELETE","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","CLOSING_CHANNEL_POST","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","COLOR_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","COLOR_POST","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","COLOR_DELETE","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","COURIER_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","COURIER_POST","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","DEPARTMENT_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","DEPARTMENT_POST","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","DEPARTMENT_DELETE","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","DISTRICT_POST","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","DISTRICT_DELETE","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","ENTRY_CHANNEL_POST","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","GENERAL_STOCK_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","MEMBERSHIP_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","MEMBERSHIP_PAYMENT_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","MODEL_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","MODULE_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","MODULE_POST","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","MODULE_DELETE","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","ONBOARD_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","ORDER_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","ORDER_ITEM_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","ORDER_STOCK_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","ORDER_STOCK_ITEM_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","ORDER_PAYMENT_METHOD_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","ORDER_PAYMENT_METHOD_POST","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","ORDER_PAYMENT_METHOD_DELETE","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","ORDER_PAYMENT_STATE_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","ORDER_PAYMENT_STATE_POST","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","ORDER_PAYMENT_STATE_DELETE","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","ORDER_STATE_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","ORDER_STATE_POST","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","ORDER_STATE_DELETE","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","PAYMENT_GATEWAY_POST","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","PRODUCT_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","PROVINCE_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","PROVINCE_POST","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","PROVINCE_DELETE","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","PURCHASE_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","PURCHASE_ITEM_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","PURCHASE_DOCUMENT_POST","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","PURCHASE_DOCUMENT_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","PURCHASE_DOCUMENT_DELETE","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","ROLE_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","ROLE_POST","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","ROLE_PUT","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","ROLE_DELETE","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","ROLE_ACCESS_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","ROLE_ACCESS_POST","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","ROLE_ACCESS_PUT","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","ROLE_ACCESS_DELETE","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","SALE_CHANNEL_DELETE","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","SALE_CHANNEL_POST","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","PURCHASE_TYPE_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","PURCHASE_TYPE_POST","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","SIZE_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","SIZE_POST","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","SIZE_DELETE","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","SIZE_TYPE_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","SIZE_TYPE_POST","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","SIZE_TYPE_DELETE","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","STOCK_REPLENISHMENT_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","STOCK_REPLENISHMENT_ITEM_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","STOCK_RETURN_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","STOCK_RETURN_ITEM_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","STOCK_TRANSACTION_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","STOCK_TRANSACTION_ITEM_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","STOCK_TRANSACTION_TYPE_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","STOCK_TRANSACTION_TYPE_POST","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","STOCK_TRANSFER_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","STOCK_TRANSFER_ITEM_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","STORE_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","SUBSCRIPTION_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","SUBSCRIPTION_POST","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","SUPPLIER_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","UNIT_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","UNIT_POST","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","UNIT_DELETE","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","UNIT_TYPE_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","UNIT_TYPE_POST","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","UNIT_TYPE_DELETE","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","USER_POST","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","USER_PUT","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","USER_DELETE","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","USER_ROLE_POST","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","WAREHOUSE_GET","ADMIN1");
                        iRoleAccess.save("ADMINISTRACION","WAREHOUSE_STOCK_GET","ADMIN1");
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

                        // mock category products
                        iCategoryProduct.save("camisetas", "camisetas","ropa", "admin1");
                        iCategoryProduct.save("jeans", "jeans","ropa", "admin1");
                        iCategoryProduct.save("tennis", "tennis","calzado", "admin1");
                        iCategoryProduct.save("botas", "botas","calzado", "admin1");
                        iCategoryProduct.save("blusas", "blusas", "ropa","admin1");
                        iCategoryProduct.save("bisuteria", "adornos", "accesorios","admin1");

                        // mock size
                        iSize.save("s", "ropa", "admin1");
                        iSize.save("m", "ropa", "admin1");
                        iSize.save("l", "ropa", "admin1");
                        iSize.save("xs", "ropa", "admin1");
                        iSize.save("xm", "ropa", "admin1");
                        iSize.save("xl", "ropa", "admin1");

                        iSize.save("12", "calzado", "admin1");
                        iSize.save("18", "calzado", "admin1");
                        iSize.save("28", "calzado", "admin1");
                        iSize.save("32","calzado", "admin1");
                        iSize.save("24",  "calzado", "admin1");
                        iSize.save("40", "calzado", "admin1");

                        iSize.save("accesorios", "accesorios", "admin1");

                        // order state
                        iOrderState.save("pendiente","#f2433d","admin1");
                        iOrderState.save("entregado", "#52c41a","admin1");
                        iOrderState.save("preparado","#00FF00", "admin1");
                        iOrderState.save("pendiente de stock","#faad14","admin1");
                        iOrderState.save("pagado","#FFA500","admin1");
                        iOrderState.save("reservado","#2f54eb", "admin1");
                        iOrderState.save("fallido","#f5222d","admin1");
                        iOrderState.save("por recoger","#1890ff","admin1");
                        iOrderState.save("no hay stock","#d9d9d9","admin1");
                        iOrderState.save("llamar","#722ed1","admin1");
                        iOrderState.save("devolucion","#ad8b00", "admin1");
                        iOrderState.save("agendado","#13c2c2","admin1");
                        iOrderState.save("en ruta","#004d80","admin1");
                        iOrderState.save("llamado","#008080","admin1");
                        iOrderState.save("cancelado","#f5222d","admin1");

                        // payment state
                        iOrderPaymentState.save("por recaudar","admin1");
                        iOrderPaymentState.save("recaudado","admin1");
                        iOrderPaymentState.save("perdida","admin1");

                        // sale channel
                        iSaleChannel.save("web","admin1");
                        iSaleChannel.save("ripley","admin1");

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
                        iUnitType.save("accessorio","admin1");

                        // unit
                        RequestUnit requestUnit1 = RequestUnit.builder()
                                .name("und")
                                .unitType("ropa")
                                .build();

                        iUnit.save(requestUnit1,"admin1");

                        RequestUnit requestUnit2 = RequestUnit.builder()
                                .name("par")
                                .unitType("calzado")
                                .build();

                        iUnit.save(requestUnit2,"admin1");

                        RequestUnit requestUnit3 = RequestUnit.builder()
                                .name("accesorio")
                                        .unitType("accessorio")
                                                .build();

                        iUnit.save(requestUnit3,"admin1");

                        // purchase type
                        iPurchaseType.save("compra","admin1");
                        iPurchaseType.save("devolucion","admin1");
                        iPurchaseType.save("restockaje","admin1");

                        // order return type
                        iOrderReturnType.save("rechazo","admin1");
                        iOrderReturnType.save("cambio","admin1");

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
                        // purchase documents
                        iPurchaseDocument.save("factura","admin1");
                        iPurchaseDocument.save("boleta","admin1");
                        iPurchaseDocument.save("recibo","admin1");
                        iPurchaseDocument.save("sin comprobante","admin1");
                        // customer types
                        iCustomerType.save("tradicional","admin1");
                        iCustomerType.save("mayorista","admin1");
                        // discounts
                        iDiscount.save("monto","admin1");
                        iDiscount.save("porcentaje","admin1");
                        iDiscount.save("no aplica","admin1");
                        // delivery points
                        iDeliveryPoint.save("lima","admin1");
                        iDeliveryPoint.save("punto scharf","admin1");
                        iDeliveryPoint.save("provincia","admin1");
                        iDeliveryPoint.save("recojo en tienda","admin1");
                        // mock users
                        iUserRole.save(business1.getUsername(), "NEGOCIO",business1.getUsername());
                        iUserRole.save(business2.getUsername(),"NEGOCIO",business2.getUsername());

                        RequestUser businessSales1 = RequestUser.builder()
                                .user("CROJAS")
                                .dni("12345678912")
                                .email("cj@gmail.com")
                                .name("CAMILO")
                                .surname("ROJAS")
                                .mobile("223456789")
                                .gender("M")
                                .address(business1.getAddress())
                                .password("abc123+")
                                .district(business1.getDistrict().getName())
                                .tokenUser(business1.getUsername())
                                .roleName("VENTAS")
                                .build();

                        iUser.save(businessSales1);

                        RequestUser businessSales2 = RequestUser.builder()
                                .user("MAPARICIO")
                                .name("MARIO")
                                .surname("APARICIO")
                                .dni("12345678913")
                                .email("mp@gmail.com")
                                .mobile("222456789")
                                .gender("M")
                                .address(business2.getAddress())
                                .password("abc123+")
                                .district(business2.getDistrict().getName())
                                .tokenUser(business2.getUsername())
                                .roleName("VENTAS")
                                .build();

                        iUser.save(businessSales2);

                        RequestUser businessStock1 = RequestUser.builder()
                                .user("AYEPES")
                                .name("AMANDA")
                                .surname("YEPES")
                                .dni("12345678914")
                                .email("ay@gmail.com")
                                .mobile("222256789")
                                .gender("F")
                                .address(business1.getAddress())
                                .password("abc123+")
                                .district(business1.getDistrict().getName())
                                .tokenUser(business1.getUsername())
                                .roleName("STOCK")
                                .build();

                        iUser.save(businessStock1);

                        RequestUser businessStock2 = RequestUser.builder()
                                .user("NTORRES")
                                .name("NICOLE")
                                .surname("TORRES")
                                .dni("12345678915")
                                .email("nt@gmail.com")
                                .mobile("222226789")
                                .gender("F")
                                .address(business2.getAddress())
                                .password("abc123+")
                                .district(business2.getDistrict().getName())
                                .tokenUser(business2.getUsername())
                                .roleName("STOCK")
                                .build();

                        iUser.save(businessStock2);

                        RequestUser businessMarketing1 = RequestUser.builder()
                                .user("OPEREZ")
                                .name("OMAR")
                                .surname("PEREZ")
                                .dni("12345678916")
                                .email("op@gmail.com")
                                .mobile("222222789")
                                .gender("M")
                                .address(business1.getAddress())
                                .password("abc123+")
                                .district(business1.getDistrict().getName())
                                .tokenUser(business1.getUsername())
                                .roleName("MARKETING")
                                .build();

                        iUser.save(businessMarketing1);

                        RequestUser businessMarketing2 = RequestUser.builder()
                                .user("JORTIZ")
                                .name("JASON")
                                .surname("ORTIZ")
                                .dni("12345678917")
                                .email("jo@gmail.com")
                                .mobile("222222289")
                                .gender("M")
                                .address(business2.getAddress())
                                .password("abc123+")
                                .district(business2.getDistrict().getName())
                                .tokenUser(business2.getUsername())
                                .roleName("MARKETING")
                                .build();

                        iUser.save(businessMarketing2);

                        RequestUser businessCustomerService1 = RequestUser.builder()
                                .user("ICONTRERAS")
                                .name("ISAAC")
                                .surname("CONTRERAS")
                                .dni("12345678918")
                                .email("ic@gmail.com")
                                .mobile("222222229")
                                .gender("M")
                                .address(business1.getAddress())
                                .password("abc123+")
                                .district(business1.getDistrict().getName())
                                .tokenUser(business1.getUsername())
                                .roleName("SERVICIO_CLIENTE")
                                .build();

                        iUser.save(businessCustomerService1);

                        RequestUser businessCustomerService2 = RequestUser.builder()
                                .user("VMENDEZ")
                                .name("VANESSA")
                                .surname("MENDEZ")
                                .dni("12345678919")
                                .email("vm@gmail.com")
                                .mobile("222222222")
                                .gender("F")
                                .address(business2.getAddress())
                                .password("abc123+")
                                .district(business2.getDistrict().getName())
                                .tokenUser(business2.getUsername())
                                .roleName("SERVICIO_CLIENTE")
                                .build();

                        iUser.save(businessCustomerService2);

                        RequestUser businessCourier1 = RequestUser.builder()
                                .user("MSALAS")
                                .name("MARCO")
                                .surname("SALAS")
                                .dni("12345678920")
                                .email("mc@gmail.com")
                                .mobile("322222222")
                                .gender("M")
                                .address(business1.getAddress())
                                .password("abc123+")
                                .district(business1.getDistrict().getName())
                                .tokenUser(business1.getUsername())
                                .roleName("COURIER")
                                .build();

                        iUser.save(businessCourier1);

                        RequestUser businessCourier2 = RequestUser.builder()
                                .user("GTRUJILLO")
                                .name("GERMAN")
                                .surname("TRUJILLO")
                                .dni("12345678921")
                                .email("gt@gmail.com")
                                .mobile("332222222")
                                .gender("M")
                                .address(business2.getAddress())
                                .password("abc123+")
                                .district(business2.getDistrict().getName())
                                .tokenUser(business2.getUsername())
                                .roleName("COURIER")
                                .build();

                        iUser.save(businessCourier2);

                        // mock stores
                        RequestStoreSave requestStoreSave1 = RequestStoreSave.builder()
                                .url("https://store1.com")
                                .storeType("WOOCOMMERCE")
                                .name("store 1")
                                .build();

                        iStore.save(requestStoreSave1,business1.getUsername());

                        RequestStoreSave requestStoreSave2 = RequestStoreSave.builder()
                                .url("https://store2.com")
                                .storeType("PRESTASHOP")
                                .name("store 2")
                                .build();

                        iStore.save(requestStoreSave2,business2.getUsername());

                        // mock brands
                        iBrand.save("nike", "OPEREZ");
                        iBrand.save("levis", "OPEREZ");
                        iBrand.save("gap", "OPEREZ");
                        iBrand.save("adidas", "JORTIZ");
                        iBrand.save("kenzo", "JORTIZ");
                        iBrand.save("lacoste", "JORTIZ");

                        // mock models
                        iModel.save("f90", "nike", "OPEREZ");
                        iModel.save("m2000", "nike", "OPEREZ");
                        iModel.save("mercurial", "nike", "OPEREZ");
                        iModel.save("indigo", "levis", "OPEREZ");
                        iModel.save("old navy", "levis", "OPEREZ");
                        iModel.save("ripper", "levis", "OPEREZ");
                        iModel.save("sweater", "gap", "OPEREZ");
                        iModel.save("kasper", "gap", "OPEREZ");
                        iModel.save("sustra", "gap", "OPEREZ");
                        iModel.save("krust", "adidas", "JORTIZ");
                        iModel.save("gist", "adidas", "JORTIZ");
                        iModel.save("thunder", "adidas", "JORTIZ");
                        iModel.save("yitro", "kenzo", "JORTIZ");
                        iModel.save("ulcast", "kenzo", "JORTIZ");
                        iModel.save("reinder", "kenzo", "JORTIZ");
                        iModel.save("realt", "lacoste", "JORTIZ");
                        iModel.save("brust", "lacoste", "JORTIZ");
                        iModel.save("frost", "lacoste", "JORTIZ");
                        // mock products
                        List<MultipartFile> productImages1 = new ArrayList<>();
                        Resource resource1 = resourceLoader.getResource("classpath:static/pictures/bill.jpg");
                        MockMultipartFile multipartImage1 = new MockMultipartFile(
                                "bill.jpg",
                                "bill.jpg",
                                "image/jpeg",
                                StreamUtils.copyToByteArray(resource1.getInputStream())
                        );
                        productImages1.add(multipartImage1);
                        List<MultipartFile> productImages2 = new ArrayList<>();
                        Resource resource2 = resourceLoader.getResource("classpath:static/pictures/invoice1.jpg");
                        MockMultipartFile multipartImage2 = new MockMultipartFile(
                                "invoice.jpg",
                                "invoice.jpg",
                                "image/jpeg",
                                StreamUtils.copyToByteArray(resource2.getInputStream())
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

                        iProduct.save(product1,productImages1, "OPEREZ");

                        RequestProductSave product2 = RequestProductSave.builder().build();
                        product2.setCategory("botas");
                        product2.setColor("rojo");
                        product2.setModel("m2000");
                        product2.setSize("24");
                        product2.setSku("A00002");
                        product2.setPrice(5.41);
                        product2.setUnit("par");

                        iProduct.save(product2,productImages2, "OPEREZ");

                        RequestProductSave product3 = RequestProductSave.builder().build();
                        product3.setCategory("tennis");
                        product3.setColor("verde");
                        product3.setModel("mercurial");
                        product3.setSize("24");
                        product3.setSku("A00003");
                        product3.setPrice(3.33);
                        product3.setUnit("par");

                        iProduct.save(product3,productImages1, "OPEREZ");

                        RequestProductSave product4 = RequestProductSave.builder().build();
                        product4.setCategory("camisetas");
                        product4.setColor("rojo");
                        product4.setModel("indigo");
                        product4.setSize("s");
                        product4.setSku("A00004");
                        product4.setPrice(7.01);
                        product4.setUnit("und");

                        iProduct.save(product4,productImages2, "OPEREZ");

                        RequestProductSave product5 = RequestProductSave.builder().build();
                        product5.setCategory("jeans");
                        product5.setColor("azul");
                        product5.setModel("old navy");
                        product5.setSize("m");
                        product5.setSku("A00005");
                        product5.setPrice(4.76);
                        product5.setUnit("und");

                        iProduct.save(product5, productImages1,"OPEREZ");

                        RequestProductSave product6 = RequestProductSave.builder().build();
                        product6.setCategory("blusas");
                        product6.setColor("amarillo");
                        product6.setModel("ripper");
                        product6.setSize("l");
                        product6.setSku("A00006");
                        product6.setPrice(1.34);
                        product6.setUnit("und");

                        iProduct.save(product6, productImages2,"OPEREZ");

                        RequestProductSave product7 = RequestProductSave.builder().build();
                        product7.setCategory("blusas");
                        product7.setColor("morado");
                        product7.setModel("sweater");
                        product7.setSize("xs");
                        product7.setSku("A00007");
                        product7.setPrice(8.23);
                        product7.setUnit("und");

                        iProduct.save(product7,productImages1, "OPEREZ");

                        RequestProductSave product8 = RequestProductSave.builder().build();
                        product8.setCategory("camisetas");
                        product8.setColor("verde");
                        product8.setModel("kasper");
                        product8.setSize("xm");
                        product8.setSku("A00008");
                        product8.setPrice(6.27);
                        product8.setUnit("und");

                        iProduct.save(product8,productImages2, "OPEREZ");

                        RequestProductSave product9 = RequestProductSave.builder().build();
                        product9.setCategory("blusas");
                        product9.setColor("naranja");
                        product9.setModel("sustra");
                        product9.setSize("xl");
                        product9.setSku("A00009");
                        product9.setPrice(9.05);
                        product9.setUnit("und");

                        iProduct.save(product9,productImages1, "OPEREZ");

                        RequestProductSave product10 = RequestProductSave.builder().build();
                        product10.setCategory("botas");
                        product10.setColor("rojo");
                        product10.setModel("krust");
                        product10.setSize("40");
                        product10.setSku("B00001");
                        product10.setPrice(7.11);
                        product10.setUnit("par");

                        iProduct.save(product10,productImages2, "JORTIZ");

                        RequestProductSave product11 = RequestProductSave.builder().build();
                        product11.setCategory("tennis");
                        product11.setColor("verde");
                        product11.setModel("gist");
                        product11.setSize("32");
                        product11.setSku("B00002");
                        product11.setPrice(4.65);
                        product11.setUnit("par");

                        iProduct.save(product11,productImages1, "JORTIZ");

                        RequestProductSave product12 = RequestProductSave.builder().build();
                        product12.setCategory("tennis");
                        product12.setColor("azul");
                        product12.setModel("thunder");
                        product12.setSize("18");
                        product12.setSku("B00003");
                        product12.setPrice(8.38);
                        product12.setUnit("par");

                        iProduct.save(product12,productImages2, "JORTIZ");

                        RequestProductSave product13 = RequestProductSave.builder().build();
                        product13.setCategory("camisetas");
                        product13.setColor("negro");
                        product13.setModel("yitro");
                        product13.setSize("s");
                        product13.setSku("B00004");
                        product13.setPrice(4.02);
                        product13.setUnit("und");

                        iProduct.save(product13, productImages1,"JORTIZ");

                        RequestProductSave product14 = RequestProductSave.builder().build();
                        product14.setCategory("blusas");
                        product14.setColor("morado");
                        product14.setModel("ulcast");
                        product14.setSize("m");
                        product14.setSku("B00005");
                        product14.setPrice(1.99);
                        product14.setUnit("und");

                        iProduct.save(product14,productImages2, "JORTIZ");

                        RequestProductSave product15 = RequestProductSave.builder().build();
                        product15.setCategory("jeans");
                        product15.setColor("amarillo");
                        product15.setModel("reinder");
                        product15.setSize("l");
                        product15.setSku("B00006");
                        product15.setPrice(6.37);
                        product15.setUnit("und");

                        iProduct.save(product15,productImages1, "JORTIZ");

                        RequestProductSave product16 = RequestProductSave.builder().build();
                        product16.setCategory("camisetas");
                        product16.setColor("rojo");
                        product16.setModel("realt");
                        product16.setSize("xl");
                        product16.setSku("B00007");
                        product16.setPrice(2.97);
                        product16.setUnit("und");

                        iProduct.save(product16,productImages2, "JORTIZ");

                        RequestProductSave product17 = RequestProductSave.builder().build();
                        product17.setCategory("blusas");
                        product17.setColor("azul");
                        product17.setModel("brust");
                        product17.setSize("xs");
                        product17.setSku("B00008");
                        product17.setPrice(5.21);
                        product17.setUnit("und");

                        iProduct.save(product17,productImages1, "JORTIZ");

                        RequestProductSave product18 = RequestProductSave.builder().build();
                        product18.setCategory("camisetas");
                        product18.setColor("naranja");
                        product18.setModel("frost");
                        product18.setSize("m");
                        product18.setSku("B00009");
                        product18.setPrice(3.53);
                        product18.setUnit("und");

                        iProduct.save(product18,productImages2, "JORTIZ");

                        // mocks suppliers

                        RequestSupplier supplier1 = RequestSupplier.builder().build();
                        supplier1.setBusinessName("burgenvillia .corp");
                        supplier1.setRuc("12345678922");
                        supplier1.setCountry("PERÚ");
                        supplier1.setDistrict("RIMAC");
                        supplier1.setSupplierType("INTERNO");
                        supplier1.setEmail("bg@gmail.com");
                        supplier1.setLocation("Lima, Street 123");
                        supplier1.setPhone("323456789");

                        iSupplier.save(supplier1, "AYEPES");

                        RequestSupplier supplier2 = RequestSupplier.builder().build();
                        supplier2.setBusinessName("coltran ltd");
                        supplier2.setRuc("12345678924");
                        supplier2.setCountry("INDIA");
                        supplier2.setDistrict("NO APLICA");
                        supplier2.setSupplierType("DISTRIBUIDOR");
                        supplier2.setEmail("coltran@gmail.com");
                        supplier2.setLocation("Mumbai, Av 345");
                        supplier2.setPhone("333456789");

                        iSupplier.save(supplier2, "AYEPES");

                        RequestSupplier supplier3 = RequestSupplier.builder().build();
                        supplier3.setBusinessName("xincheng ptd");
                        supplier3.setRuc("12345678925");
                        supplier3.setCountry("China");
                        supplier3.setDistrict("NO APLICA");
                        supplier3.setSupplierType("DISTRIBUIDOR");
                        supplier3.setEmail("xincheng@gmail.com");
                        supplier3.setLocation("Shanghai, st 777");
                        supplier3.setPhone("343456789");

                        iSupplier.save(supplier3, "NTORRES");

                        RequestSupplier supplier4 = RequestSupplier.builder().build();
                        supplier4.setBusinessName("tejidos sa");
                        supplier4.setRuc("12345678926");
                        supplier4.setCountry("España");
                        supplier4.setDistrict("NO APLICA");
                        supplier4.setSupplierType("DISTRIBUIDOR");
                        supplier4.setEmail("tejidos@gmail.com");
                        supplier4.setLocation("Valencia, tranv 843");
                        supplier4.setPhone("353456789");

                        iSupplier.save(supplier4, "NTORRES");

                        // mock supplier products

                        RequestSupplierProduct requestSupplierProduct1 = RequestSupplierProduct.builder()
                                .product("A00001")
                                .purchasePrice(5.24)
                                .serial("A00001A")
                                .supplier("12345678922")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct1, "AYEPES");

                        RequestSupplierProduct requestSupplierProduct2 = RequestSupplierProduct.builder()
                                .product("A00001")
                                .purchasePrice(2.10)
                                .serial("A00001B")
                                .supplier("12345678924")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct2, "AYEPES");

                        RequestSupplierProduct requestSupplierProduct3 = RequestSupplierProduct.builder()
                                .product("A00002")
                                .purchasePrice(10.47)
                                .serial("A00002A")
                                .supplier("12345678922")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct3, "AYEPES");

                        RequestSupplierProduct requestSupplierProduct4 = RequestSupplierProduct.builder()
                                .product("A00002")
                                .purchasePrice(13.09)
                                .serial("A00002B")
                                .supplier("12345678924")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct4, "AYEPES");

                        RequestSupplierProduct requestSupplierProduct5 = RequestSupplierProduct.builder()
                                .product("A00003")
                                .purchasePrice(20.15)
                                .serial("A00003A")
                                .supplier("12345678922")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct5, "AYEPES");

                        RequestSupplierProduct requestSupplierProduct6 = RequestSupplierProduct.builder()
                                .product("A00003")
                                .purchasePrice(17.45)
                                .serial("A00003B")
                                .supplier("12345678924")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct6, "AYEPES");

                        RequestSupplierProduct requestSupplierProduct7 = RequestSupplierProduct.builder()
                                .product("A00004")
                                .purchasePrice(23.76)
                                .serial("A00004A")
                                .supplier("12345678922")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct7, "AYEPES");

                        RequestSupplierProduct requestSupplierProduct8 = RequestSupplierProduct.builder()
                                .product("A00004")
                                .purchasePrice(35.02)
                                .serial("A00004B")
                                .supplier("12345678924")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct8, "AYEPES");

                        RequestSupplierProduct requestSupplierProduct9 = RequestSupplierProduct.builder()
                                .product("A00005")
                                .purchasePrice(7.90)
                                .serial("A00005A")
                                .supplier("12345678922")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct9, "AYEPES");

                        RequestSupplierProduct requestSupplierProduct10 = RequestSupplierProduct.builder()
                                .product("A00005")
                                .purchasePrice(3.22)
                                .serial("A00005B")
                                .supplier("12345678924")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct10, "AYEPES");

                        RequestSupplierProduct requestSupplierProduct11 = RequestSupplierProduct.builder()
                                .product("A00006")
                                .purchasePrice(5.34)
                                .serial("A00006A")
                                .supplier("12345678922")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct11, "AYEPES");

                        RequestSupplierProduct requestSupplierProduct12 = RequestSupplierProduct.builder()
                                .product("A00006")
                                .purchasePrice(2.66)
                                .serial("A00006B")
                                .supplier("12345678924")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct12, "AYEPES");

                        RequestSupplierProduct requestSupplierProduct13 = RequestSupplierProduct.builder()
                                .product("A00007")
                                .purchasePrice(4.50)
                                .serial("A00007A")
                                .supplier("12345678922")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct13, "AYEPES");

                        RequestSupplierProduct requestSupplierProduct14 = RequestSupplierProduct.builder()
                                .product("A00007")
                                .purchasePrice(11.37)
                                .serial("A00007B")
                                .supplier("12345678924")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct14, "AYEPES");

                        RequestSupplierProduct requestSupplierProduct15 = RequestSupplierProduct.builder()
                                .product("A00008")
                                .purchasePrice(9.11)
                                .serial("A00008A")
                                .supplier("12345678922")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct15, "AYEPES");

                        RequestSupplierProduct requestSupplierProduct16 = RequestSupplierProduct.builder()
                                .product("A00008")
                                .purchasePrice(2.73)
                                .serial("A00008B")
                                .supplier("12345678924")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct16, "AYEPES");

                        RequestSupplierProduct requestSupplierProduct17 = RequestSupplierProduct.builder()
                                .product("A00009")
                                .purchasePrice(6.41)
                                .serial("A00009A")
                                .supplier("12345678922")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct17, "AYEPES");

                        RequestSupplierProduct requestSupplierProduct18 = RequestSupplierProduct.builder()
                                .product("A00009")
                                .purchasePrice(12.30)
                                .serial("A00009B")
                                .supplier("12345678924")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct18, "AYEPES");

                        RequestSupplierProduct requestSupplierProduct19 = RequestSupplierProduct.builder()
                                .product("B00001")
                                .purchasePrice(3.01)
                                .serial("B00001A")
                                .supplier("12345678925")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct19, "NTORRES");

                        RequestSupplierProduct requestSupplierProduct20 = RequestSupplierProduct.builder()
                                .product("B00001")
                                .purchasePrice(1.05)
                                .serial("B00001B")
                                .supplier("12345678926")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct20, "NTORRES");

                        RequestSupplierProduct requestSupplierProduct21 = RequestSupplierProduct.builder()
                                .product("B00002")
                                .purchasePrice(7.20)
                                .serial("B00002A")
                                .supplier("12345678925")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct21, "NTORRES");

                        RequestSupplierProduct requestSupplierProduct22 = RequestSupplierProduct.builder()
                                .product("B00002")
                                .purchasePrice(5.68)
                                .serial("B00002B")
                                .supplier("12345678926")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct22, "NTORRES");

                        RequestSupplierProduct requestSupplierProduct23 = RequestSupplierProduct.builder()
                                .product("B00003")
                                .purchasePrice(36.49)
                                .serial("B00003A")
                                .supplier("12345678925")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct23, "NTORRES");

                        RequestSupplierProduct requestSupplierProduct24 = RequestSupplierProduct.builder()
                                .product("B00003")
                                .purchasePrice(45.27)
                                .serial("B00003B")
                                .supplier("12345678926")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct24, "NTORRES");

                        RequestSupplierProduct requestSupplierProduct25 = RequestSupplierProduct.builder()
                                .product("B00004")
                                .purchasePrice(22.38)
                                .serial("B00004A")
                                .supplier("12345678925")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct25, "NTORRES");

                        RequestSupplierProduct requestSupplierProduct26 = RequestSupplierProduct.builder()
                                .product("B00004")
                                .purchasePrice(15.07)
                                .serial("B00004B")
                                .supplier("12345678926")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct26, "NTORRES");

                        RequestSupplierProduct requestSupplierProduct27 = RequestSupplierProduct.builder()
                                .product("B00005")
                                .purchasePrice(73.02)
                                .serial("B00005A")
                                .supplier("12345678925")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct27, "NTORRES");

                        RequestSupplierProduct requestSupplierProduct28 = RequestSupplierProduct.builder()
                                .product("B00005")
                                .purchasePrice(82.17)
                                .serial("B00005B")
                                .supplier("12345678926")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct28, "NTORRES");

                        RequestSupplierProduct requestSupplierProduct29 = RequestSupplierProduct.builder()
                                .product("B00006")
                                .purchasePrice(13.77)
                                .serial("B00006A")
                                .supplier("12345678925")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct29, "NTORRES");

                        RequestSupplierProduct requestSupplierProduct30 = RequestSupplierProduct.builder()
                                .product("B00006")
                                .purchasePrice(24.93)
                                .serial("B00006B")
                                .supplier("12345678926")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct30, "NTORRES");

                        RequestSupplierProduct requestSupplierProduct31 = RequestSupplierProduct.builder()
                                .product("B00007")
                                .purchasePrice(64.57)
                                .serial("B00007A")
                                .supplier("12345678925")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct31, "NTORRES");

                        RequestSupplierProduct requestSupplierProduct32 = RequestSupplierProduct.builder()
                                .product("B00007")
                                .purchasePrice(23.89)
                                .serial("B00007B")
                                .supplier("12345678926")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct32, "NTORRES");

                        RequestSupplierProduct requestSupplierProduct33 = RequestSupplierProduct.builder()
                                .product("B00008")
                                .purchasePrice(17.94)
                                .serial("B00008A")
                                .supplier("12345678925")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct33, "NTORRES");

                        RequestSupplierProduct requestSupplierProduct34 = RequestSupplierProduct.builder()
                                .product("B00008")
                                .purchasePrice(33.29)
                                .serial("B00008B")
                                .supplier("12345678926")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct34, "NTORRES");

                        RequestSupplierProduct requestSupplierProduct35 = RequestSupplierProduct.builder()
                                .product("B00009")
                                .purchasePrice(95.22)
                                .serial("B00009A")
                                .supplier("12345678925")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct35, "NTORRES");

                        RequestSupplierProduct requestSupplierProduct36 = RequestSupplierProduct.builder()
                                .product("B00009")
                                .purchasePrice(83.19)
                                .serial("B00009B")
                                .supplier("12345678926")
                                .build();

                        iSupplierProduct.save(requestSupplierProduct36, "NTORRES");

                        // stock transaction types mocks
                        iStockTransactionType.save("compra", "admin1");
                        iStockTransactionType.save("pedido", "admin1");
                        iStockTransactionType.save("transferencia-entrada", "admin1");
                        iStockTransactionType.save("transferencia-salida", "admin1");
                        iStockTransactionType.save("devolucion-comprador","admin1");
                        iStockTransactionType.save("devolucion-proveedor","admin1");

                        // warehouse mocks
                        RequestWarehouse warehouse1 = RequestWarehouse.builder()
                                .contact("ROBERTO FRANCO")
                                .phone("145235782")
                                .address("Cusco calle 123")
                                .reference("NO APLICA")
                                .name("luminous")
                                .build();

                        iWarehouse.save(warehouse1, "AYEPES");

                        RequestWarehouse warehouse2 = RequestWarehouse.builder()
                                .contact("MARIA PERDOMO")
                                .phone("190203455")
                                .address("Lima Avenida 234")
                                .reference("NO APLICA")
                                .name("oikas")
                                .build();

                        iWarehouse.save(warehouse2, "AYEPES");

                        RequestWarehouse warehouse3 = RequestWarehouse.builder()
                                .contact("ERNESTO MENDEZ")
                                .phone("359834031")
                                .address("Arequipa Calle 765")
                                .reference("NO APLICA")
                                .name("villalobos")
                                .build();

                        iWarehouse.save(warehouse3, "NTORRES");

                        RequestWarehouse warehouse4 = RequestWarehouse.builder()
                                .contact("OMAR RESTREPO")
                                .phone("899123054")
                                .address("Nazca calle 89")
                                .reference("NO APLICA")
                                .name("alcazar")
                                .build();

                        iWarehouse.save(warehouse4, "NTORRES");

                        // courier
                        RequestCourier requestCourier1 = RequestCourier.builder()
                                .courier("Marvisur")
                                .phone("123456789")
                                .build();

                        iCourier.save(requestCourier1,"gjimenez");

                        RequestCourier requestCourier2 = RequestCourier.builder()
                                .courier("Rappi")
                                .phone("111111111")
                                .build();

                        iCourier.save(requestCourier2,"gjimenez");

                        RequestCourier requestCourier3 = RequestCourier.builder()
                                .courier("Indriver")
                                .phone("222222222")
                                .build();

                        iCourier.save(requestCourier3,"fcasas");

                        RequestCourier requestCourier4 = RequestCourier.builder()
                                .courier("Shalom")
                                .phone("333333333")
                                .build();

                        iCourier.save(requestCourier4,"fcasas");

                        RequestCourier requestCourier5 = RequestCourier.builder()
                                .courier("Sin courier")
                                .phone("000000000")
                                .build();

                        iCourier.save(requestCourier5,"admin1");

                        // purchases

                        List<RequestPurchaseItem> requestPurchaseItemList1 = new ArrayList<>();

                        RequestPurchaseItem requestPurchaseItem1 = RequestPurchaseItem.builder()
                                .observations("no aplica")
                                .quantity(15)
                                .supplierProduct("A00001A")
                                .build();

                        requestPurchaseItemList1.add(requestPurchaseItem1);

                        RequestPurchaseItem requestPurchaseItem2 = RequestPurchaseItem.builder()
                                .observations("no aplica")
                                .quantity(4)
                                .supplierProduct("A00002A")
                                .build();

                        requestPurchaseItemList1.add(requestPurchaseItem2);

                        RequestPurchaseItem requestPurchaseItem3 = RequestPurchaseItem.builder()
                                .observations("no aplica")
                                .quantity(20)
                                .supplierProduct("A00003A")
                                .build();

                        requestPurchaseItemList1.add(requestPurchaseItem3);

                        RequestPurchaseItem requestPurchaseItem4 = RequestPurchaseItem.builder()
                                .observations("no aplica")
                                .quantity(25)
                                .supplierProduct("A00004A")
                                .build();

                        requestPurchaseItemList1.add(requestPurchaseItem4);

                        RequestPurchaseItem requestPurchaseItem5 = RequestPurchaseItem.builder()
                                .observations("no aplica")
                                .quantity(7)
                                .supplierProduct("A00005A")
                                .build();

                        requestPurchaseItemList1.add(requestPurchaseItem5);

                        RequestPurchaseItem requestPurchaseItem6 = RequestPurchaseItem.builder()
                                .observations("no aplica")
                                .quantity(15)
                                .supplierProduct("A00006A")
                                .build();

                        requestPurchaseItemList1.add(requestPurchaseItem6);

                        RequestPurchase requestPurchase1 = RequestPurchase.builder()
                                .serial("SA00001")
                                .purchaseType("compra")
                                .purchaseDocument("FACTURA")
                                .requestPurchaseItemList(requestPurchaseItemList1)
                                .warehouse("luminous")
                                .supplier("12345678922")
                                .build();

                        iPurchase.save(requestPurchase1, "AYEPES");

                        List<RequestPurchaseItem> requestPurchaseItemList2 = new ArrayList<>();

                        RequestPurchaseItem requestPurchaseItem7 = RequestPurchaseItem.builder()
                                .observations("no aplica")
                                .quantity(9)
                                .supplierProduct("B00001A")
                                .build();

                        requestPurchaseItemList2.add(requestPurchaseItem7);

                        RequestPurchaseItem requestPurchaseItem8 = RequestPurchaseItem.builder()
                                .observations("no aplica")
                                .quantity(15)
                                .supplierProduct("B00002A")
                                .build();

                        requestPurchaseItemList2.add(requestPurchaseItem8);

                        RequestPurchaseItem requestPurchaseItem9 = RequestPurchaseItem.builder()
                                .observations("no aplica")
                                .quantity(36)
                                .supplierProduct("B00003A")
                                .build();

                        requestPurchaseItemList2.add(requestPurchaseItem9);

                        RequestPurchaseItem requestPurchaseItem10 = RequestPurchaseItem.builder()
                                .observations("no aplica")
                                .quantity(13)
                                .supplierProduct("B00004A")
                                .build();

                        requestPurchaseItemList2.add(requestPurchaseItem10);

                        RequestPurchaseItem requestPurchaseItem11 = RequestPurchaseItem.builder()
                                .observations("no aplica")
                                .quantity(20)
                                .supplierProduct("B00005A")
                                .build();

                        requestPurchaseItemList2.add(requestPurchaseItem11);

                        RequestPurchaseItem requestPurchaseItem12 = RequestPurchaseItem.builder()
                                .observations("no aplica")
                                .quantity(27)
                                .supplierProduct("B00006A")
                                .build();

                        requestPurchaseItemList2.add(requestPurchaseItem12);

                        RequestPurchase requestPurchase2 = RequestPurchase.builder()
                                .purchaseType("compra")
                                .serial("SB00001")
                                .requestPurchaseItemList(requestPurchaseItemList2)
                                .warehouse("alcazar")
                                .purchaseDocument("FACTURA")
                                .supplier("12345678925")
                                .build();

                        iPurchase.save(requestPurchase2, "NTORRES");

                        // orders mocks
                        MultipartFile[] receipts1 = new MultipartFile[1];
                        Resource resource3 = resourceLoader.getResource("classpath:static/pictures/receipt.jpg");
                        MockMultipartFile multipartFile1 = new MockMultipartFile(
                                "receipt.jpg",
                                "receipt.jpg",
                                "image/jpeg",
                                StreamUtils.copyToByteArray(resource3.getInputStream())
                        );
                        receipts1[0] = multipartFile1;

                        RequestOrderItem requestOrderItem1 = RequestOrderItem.builder()
                                .product("A00001")
                                .discount("no aplica")
                                .discountAmount(0.00)
                                .quantity(2)
                                .observations("")
                                .build();

                        RequestOrderItem requestOrderItem2 = RequestOrderItem.builder()
                                .quantity(1)
                                .discount("porcentaje")
                                .discountAmount(3.00)
                                .product("A00002")
                                .observations("")
                                .build();

                        ArrayList<RequestOrderItem> requestOrderItems1 = new ArrayList<>();

                        requestOrderItems1.add(requestOrderItem1);
                        requestOrderItems1.add(requestOrderItem2);

                        RequestCustomer requestCustomer1 = RequestCustomer.builder()
                                .name("Emilio Gomez")
                                .phone("940544828")
                                .address("807 IQUIQUE")
                                .customerType("Tradicional")
                                .instagram("")
                                .district("BREÑA")
                                .tokenUser("CROJAS")
                                .reference("")
                                .dni("NO APLICA")
                                .build();

                        iCustomer.save(requestCustomer1);

                        RequestOrderSave requestOrderSave1 = RequestOrderSave.builder()
                                .advancedPayment(0.00)
                                .deliveryAddress("807 IQUIQUE")
                                .deliveryAmount(0.00)
                                .managementType("venta")
                                .observations("")
                                .paymentMethod("yape")
                                .saleChannel("web")
                                .requestOrderItems(requestOrderItems1)
                                .storeName("store 1")
                                .closingChannel("whatsapp")
                                .deliveryPoint("lima")
                                .discountAmount(0.00)
                                .discount("NO APLICA")
                                .phone("940544828")
                                .build();

                        iOrdering.save(requestOrderSave1,receipts1,"CROJAS");

                        MultipartFile[] receipts2 = new MultipartFile[2];
                        Resource resource4 = resourceLoader.getResource("classpath:static/pictures/receiptarticle.jpg");
                        MockMultipartFile multipartFile2 = new MockMultipartFile(
                                "receiptarticle.jpg",
                                "receiptarticle.jpg",
                                "image/jpeg",
                                StreamUtils.copyToByteArray(resource4.getInputStream())
                        );
                        receipts2[0] = multipartFile2;
                        Resource resource5 = resourceLoader.getResource("classpath:static/pictures/invoice1.jpg");
                        MockMultipartFile multipartFile3 = new MockMultipartFile(
                                "invoice1.jpg",
                                "invoice1.jpg",
                                "image/jpeg",
                                StreamUtils.copyToByteArray(resource5.getInputStream())
                        );
                        receipts2[1] = multipartFile3;

                        RequestOrderItem requestOrderItem3 = RequestOrderItem.builder()
                                .product("A00003")
                                .quantity(3)
                                .discount("no aplica")
                                .discountAmount(0.00)
                                .observations("")
                                .build();

                        RequestOrderItem requestOrderItem4 = RequestOrderItem.builder()
                                .product("A00001")
                                .quantity(1)
                                .discount("porcentaje")
                                .discountAmount(2.00)
                                .observations("")
                                .build();

                        ArrayList<RequestOrderItem> requestOrderItems2 = new ArrayList<>();

                        requestOrderItems2.add(requestOrderItem3);
                        requestOrderItems2.add(requestOrderItem4);

                        RequestCustomer requestCustomer2 = RequestCustomer.builder()
                                .name("Consuelo Rojas")
                                .phone("956701333")
                                .address("AV. JORGE CHAVEZ 420, OFICN LIMA")
                                .customerType("Tradicional")
                                .instagram("")
                                .district("INDEPENDENCIA")
                                .tokenUser("CROJAS")
                                .dni("NO APLICA")
                                .reference("")
                                .build();

                        iCustomer.save(requestCustomer2);

                        RequestOrderSave requestOrderSave2 = RequestOrderSave.builder()
                                .advancedPayment(4.00)
                                .deliveryAddress("AV. JORGE CHAVEZ 420, OFICN LIMA")
                                .deliveryAmount(3.00)
                                .managementType("venta")
                                .observations("")
                                .paymentMethod("plin")
                                .saleChannel("web")
                                .requestOrderItems(requestOrderItems2)
                                .storeName("store 1")
                                .closingChannel("facebook")
                                .phone("956701333")
                                .deliveryPoint("provincia")
                                .discountAmount(2.00)
                                .discount("PORCENTAJE")
                                .build();

                        iOrdering.save(requestOrderSave2,receipts2,"CROJAS");

                        MultipartFile[] receipts3 = new MultipartFile[0];

                        RequestOrderItem requestOrderItem5 = RequestOrderItem.builder()
                                .product("B00001")
                                .discount("no aplica")
                                .discountAmount(0.00)
                                .quantity(1)
                                .observations("")
                                .build();

                        RequestOrderItem requestOrderItem6 = RequestOrderItem.builder()
                                .quantity(3)
                                .discount("porcentaje")
                                .discountAmount(5.00)
                                .product("B00002")
                                .observations("")
                                .build();

                        List<RequestOrderItem> requestOrderItems3 = new ArrayList<>();

                        requestOrderItems3.add(requestOrderItem5);
                        requestOrderItems3.add(requestOrderItem6);

                        RequestCustomer requestCustomer3 = RequestCustomer.builder()
                                .name("Ulises Trujillo")
                                .phone("944214925")
                                .address("AV MARAÑÓN 776")
                                .customerType("Tradicional")
                                .instagram("")
                                .district("ATE")
                                .tokenUser("MAPARICIO")
                                .dni("NO APLICA")
                                .reference("")
                                .build();

                        iCustomer.save(requestCustomer3);

                        RequestOrderSave requestOrderSave3 = RequestOrderSave.builder()
                                .advancedPayment(0.00)
                                .deliveryAmount(6.25)
                                .managementType("venta")
                                .observations("")
                                .paymentMethod("efectivo")
                                .saleChannel("web")
                                .requestOrderItems(requestOrderItems3)
                                .storeName("store 2")
                                .closingChannel("twitter")
                                .deliveryAddress("AV MARAÑÓN 776")
                                .phone("944214925")
                                .deliveryPoint("punto scharf")
                                .discountAmount(5.00)
                                .discount("MONTO")
                                .build();

                        iOrdering.save(requestOrderSave3,receipts3,"MAPARICIO");

                        MultipartFile[] receipts4 = new MultipartFile[0];

                        RequestOrderItem requestOrderItem7 = RequestOrderItem.builder()
                                .product("B00002")
                                .discount("porcentaje")
                                .discountAmount(7.00)
                                .quantity(5)
                                .observations("")
                                .build();

                        RequestOrderItem requestOrderItem8 = RequestOrderItem.builder()
                                .quantity(2)
                                .discount("no aplica")
                                .discountAmount(0.00)
                                .product("B00003")
                                .observations("")
                                .build();

                        List<RequestOrderItem> requestOrderItems4 = new ArrayList<>();

                        requestOrderItems4.add(requestOrderItem7);
                        requestOrderItems4.add(requestOrderItem8);

                        RequestCustomer requestCustomer4 = RequestCustomer.builder()
                                .name("Roberto Padilla")
                                .phone("989538516")
                                .address("URB. CAPILLA 130")
                                .customerType("Tradicional")
                                .instagram("")
                                .district("CHORRILLOS")
                                .tokenUser("MAPARICIO")
                                .dni("NO APLICA")
                                .reference("")
                                .build();

                        iCustomer.save(requestCustomer4);

                        RequestOrderSave requestOrderSave4 = RequestOrderSave.builder()
                                .advancedPayment(0.00)
                                .deliveryAddress("URB. CAPILLA 130")
                                .deliveryAmount(10.15)
                                .managementType("venta")
                                .observations("URB. LA CAPILLA 130, CALLE SARAGOZA- LA MOLINA")
                                .paymentMethod("efectivo")
                                .saleChannel("web")
                                .requestOrderItems(requestOrderItems4)
                                .storeName("store 2")
                                .closingChannel("web")
                                .phone("989538516")
                                .deliveryPoint("recojo en tienda")
                                .discountAmount(5.00)
                                .discount("PORCENTAJE")
                                .build();

                        iOrdering.save(requestOrderSave4,receipts4,"MAPARICIO");

                        RequestOrderItem requestOrderItem9 = RequestOrderItem.builder()
                                .product("A00003")
                                .quantity(5)
                                .discount("no aplica")
                                .discountAmount(0.00)
                                .observations("")
                                .build();

                        RequestOrderItem requestOrderItem10 = RequestOrderItem.builder()
                                .product("A00001")
                                .quantity(8)
                                .discount("porcentaje")
                                .discountAmount(2.00)
                                .observations("")
                                .build();

                        ArrayList<RequestOrderItem> requestOrderItems5 = new ArrayList<>();

                        requestOrderItems5.add(requestOrderItem9);
                        requestOrderItems5.add(requestOrderItem10);

                        RequestOrderSave requestOrderSave5 = RequestOrderSave.builder()
                                .advancedPayment(2.00)
                                .deliveryAddress("AV. JORGE CHAVEZ 420, OFICN LIMA")
                                .deliveryAmount(4.00)
                                .managementType("venta")
                                .observations("")
                                .paymentMethod("plin")
                                .saleChannel("web")
                                .requestOrderItems(requestOrderItems5)
                                .storeName("store 1")
                                .closingChannel("instagram")
                                .phone("956701333")
                                .deliveryPoint("lima")
                                .discountAmount(0.00)
                                .discount("NO APLICA")
                                .build();

                        iOrdering.save(requestOrderSave5,receipts2,"CROJAS");

                        // order stock mocks

                        RequestOrderStockItem requestOrderStockItem1 = RequestOrderStockItem.builder()
                                .supplierProduct("A00001A")
                                .quantity(2)
                                .product("A00001")
                                .build();

                        RequestOrderStockItem requestOrderStockItem2 = RequestOrderStockItem.builder()
                                .supplierProduct("A00002A")
                                .quantity(1)
                                .product("A00002")
                                .build();

                        List<RequestOrderStockItem> requestOrderStockItemList1 = new ArrayList<>();
                        requestOrderStockItemList1.add(requestOrderStockItem1);
                        requestOrderStockItemList1.add(requestOrderStockItem2);

                        iOrderStock.save(1L,"luminous", requestOrderStockItemList1,"AYEPES");

                        RequestOrderStockItem requestOrderStockItem3 = RequestOrderStockItem.builder()
                                .supplierProduct("A00003A")
                                .quantity(2)
                                .product("A00003")
                                .build();

                        RequestOrderStockItem requestOrderStockItem4 = RequestOrderStockItem.builder()
                                .supplierProduct("A00001A")
                                .quantity(1)
                                .product("A00001")
                                .build();

                        List<RequestOrderStockItem> requestOrderStockItemList2 = new ArrayList<>();
                        requestOrderStockItemList2.add(requestOrderStockItem3);
                        requestOrderStockItemList2.add(requestOrderStockItem4);

                        iOrderStock.save(2L,"luminous", requestOrderStockItemList2,"AYEPES");

                        RequestOrderStockItem requestOrderStockItem5 = RequestOrderStockItem.builder()
                                .supplierProduct("B00001A")
                                .quantity(1)
                                .product("B00001")
                                .build();

                        RequestOrderStockItem requestOrderStockItem6 = RequestOrderStockItem.builder()
                                .supplierProduct("B00002A")
                                .quantity(3)
                                .product("B00002")
                                .build();

                        List<RequestOrderStockItem> requestOrderStockItemList3 = new ArrayList<>();
                        requestOrderStockItemList3.add(requestOrderStockItem5);
                        requestOrderStockItemList3.add(requestOrderStockItem6);

                        iOrderStock.save(3L,"alcazar", requestOrderStockItemList3,"NTORRES");

                        RequestOrderStockItem requestOrderStockItem7 = RequestOrderStockItem.builder()
                                .supplierProduct("B00002A")
                                .quantity(5)
                                .product("B00002")
                                .build();

                        RequestOrderStockItem requestOrderStockItem8 = RequestOrderStockItem.builder()
                                .supplierProduct("B00003A")
                                .quantity(2)
                                .product("B00003")
                                .build();

                        List<RequestOrderStockItem> requestOrderStockItemList4 = new ArrayList<>();
                        requestOrderStockItemList4.add(requestOrderStockItem7);
                        requestOrderStockItemList4.add(requestOrderStockItem8);

                        iOrderStock.save(4L, "alcazar",requestOrderStockItemList4,"NTORRES");

                        MultipartFile[] paymentReceipts = new MultipartFile[0];
                        MultipartFile[] courierPictures = new MultipartFile[0];

                        RequestOrderUpdate requestOrderUpdate1 = RequestOrderUpdate.builder()
                                .observations("")
                                .orderState("ENTREGADO")
                                .paymentMethod("LINK")
                                .paymentState("RECAUDADO")
                                .saleChannel("tienda online")
                                .courier("MARVISUR")
                                .build();

                        iOrdering.update(1L,requestOrderUpdate1,paymentReceipts,courierPictures,"ICONTRERAS");

                        RequestCancelledOrder requestCancelledOrder1 = RequestCancelledOrder.builder()
                                .cancellationReason("Demora en entrega")
                                .orderId(2L)
                                .warehouse("luminous")
                                .build();

                        iCancelledOrder.save(requestCancelledOrder1,"ICONTRERAS");

                        // mock courier pictures and change state to delivered
                        MultipartFile[] courierImages1 = new MultipartFile[1];
                        Resource resource6 = resourceLoader.getResource("classpath:static/pictures/bill.jpg");
                        MockMultipartFile multipartCourierImage1 = new MockMultipartFile(
                                "bill.jpg",
                                "bill.jpg",
                                "image/jpeg",
                                StreamUtils.copyToByteArray(resource6.getInputStream())
                        );
                        courierImages1[0] = multipartCourierImage1;

                        RequestCourierOrder requestCourierOrder1 = RequestCourierOrder.builder()
                                .orderPictures(courierImages1)
                                .orderState("ENTREGADO")
                                .paymentMethod("LINK")
                                .build();

                        iCourier.updateOrder(1L,requestCourierOrder1,"MSALAS");

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
                        RequestStockReturn requestStockReturn1 = RequestStockReturn.builder()
                                .serial("SR1AA00001")
                                .purchaseSerial("SA00001")
                                .warehouse("luminous")
                                .tokenUser("AYEPES")
                                .requestStockReturnItemList(requestStockReturnItemList)
                                .build();
                        iStockReturn.save(requestStockReturn1);
                        // update order to lack of stock state
                        RequestOrderUpdate requestOrderUpdate3 = RequestOrderUpdate.builder()
                                .saleChannel("tienda online")
                                .courier("SIN COURIER")
                                .paymentState("POR RECAUDAR")
                                .paymentMethod("plin")
                                .orderState("NO HAY STOCK")
                                .observations("falta de stock para cumplir con el pedido")
                                .build();
                        iOrdering.update(5L,requestOrderUpdate3,receipts2,new MultipartFile[0],"ICONTRERAS");
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
                        iStockReplenishment.save(5L,requestStockReplenishmentItemList1,"AYEPES");
                        // mock stock transfer
                        List<RequestStockTransferItem> requestStockTransferItemList1 = new ArrayList<>();
                        requestStockTransferItemList1.add(RequestStockTransferItem.builder()
                                .quantity(15)
                                .supplierProductSerial("A00003A")
                                .build());
                        requestStockTransferItemList1.add(RequestStockTransferItem.builder()
                                .quantity(4)
                                .supplierProductSerial("A00002A")
                                .build());
                        RequestStockTransfer requestStockTransfer1 = RequestStockTransfer.builder()
                                .serial("ST00001")
                                .originWarehouse("luminous")
                                .destinationWarehouse("oikas")
                                .requestStockTransferItemList(requestStockTransferItemList1)
                                .build();

                        iStockTransfer.save(requestStockTransfer1,"AYEPES");
                        // add order item mock
                        RequestOrderItem requestOrderItemAdd = RequestOrderItem.builder()
                                .product("B00003")
                                .discount("porcentaje")
                                .discountAmount(0.00)
                                .observations("")
                                .quantity(2)
                                .build();
                        iOrderItem.add(3L,requestOrderItemAdd,"VMENDEZ");
                        // delete order item mock
                        iOrderItem.delete(4L,"B00002","VMENDEZ");
                        // update order item mock
                        RequestOrderItem requestOrderItemUpdate = RequestOrderItem.builder()
                                .product("B00001")
                                .discount("porcentaje")
                                .discountAmount(5.00)
                                .quantity(3)
                                .observations("se adicionan dos unidades al pedido")
                                .build();
                        iOrderItem.update(3L,requestOrderItemUpdate,"VMENDEZ");

                        List<RequestOrderReturnItem> requestOrderReturnItemList = new ArrayList<>();

                        RequestOrderReturnItem requestOrderReturnItem1 = RequestOrderReturnItem.builder()
                                .productSku("A00001")
                                .orderReturnType("rechazo")
                                .quantity(1)
                                .supplierProductSerial("A00001A")
                                .build();

                        requestOrderReturnItemList.add(requestOrderReturnItem1);

                        RequestOrderReturnItem requestOrderReturnItem2 = RequestOrderReturnItem.builder()
                                .productSku("A00002")
                                .orderReturnType("rechazo")
                                .supplierProductSerial("A00002A")
                                .quantity(1)
                                .build();
                        requestOrderReturnItemList.add(requestOrderReturnItem2);

                        iOrderReturn.save(1L,requestOrderReturnItemList,"AYEPES");
                }catch (RuntimeException e){
                        e.printStackTrace();
                        throw new RuntimeException(e.getMessage());
                }
        }

}
