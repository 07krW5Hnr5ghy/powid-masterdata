package com.proyect.masterdata.seeder;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.CountryDTO;
import com.proyect.masterdata.dto.LocationDTO;
import com.proyect.masterdata.dto.request.*;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.*;
import lombok.RequiredArgsConstructor;
import org.bouncycastle.cert.ocsp.Req;
import org.springframework.boot.CommandLineRunner;
import org.springframework.core.io.Resource;
import org.springframework.core.io.ResourceLoader;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Component;
import org.springframework.util.StreamUtils;
import org.springframework.web.multipart.MultipartFile;

import java.time.OffsetDateTime;
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
        private final ISubCategoryProduct iSubCategoryProduct;
        private final IPurchasePaymentType iPurchasePaymentType;
        private final IDeliveryStatus iDeliveryStatus;
        private final IDeliveryManifestStatus iDeliveryManifestStatus;
        @Override
        public void run(String... args) throws Exception {

                try{
                        // department, province and district to create system user
                        Department department = departmentRepository.save(Department.builder()
                                        .name("SISTEMA")
                                        .status(true)
                                        .registrationDate(OffsetDateTime.now())
                                        .updateDate(OffsetDateTime.now())
                                .build());

                        Province province = provinceRepository.save(Province.builder()
                                        .name("SISTEMA")
                                        .status(true)
                                        .department(department)
                                        .departmentId(department.getId())
                                        .registrationDate(OffsetDateTime.now())
                                        .updateDate(OffsetDateTime.now())
                                .build());

                        District district = districtRepository.save(District.builder()
                                        .name("SISTEMA")
                                        .status(true)
                                        .registrationDate(OffsetDateTime.now())
                                        .updateDate(OffsetDateTime.now())
                                .build());

                        Client systemClient = clientRepository.save(Client.builder()
                                        .name("SISTEMA")
                                        .surname("SISTEMA")
                                        .ruc("SISTEMA")
                                        .dni("SISTEMA")
                                        .business("SISTEMA")
                                        .mobile("SISTEMA")
                                        .address("SISTEMA")
                                        .email("SISTEMA")
                                        .status(true)
                                        .district(district)
                                        .districtId(district.getId())
                                        .registrationDate(OffsetDateTime.now())
                                        .updateDate(OffsetDateTime.now())
                                .build());

                        User adminUser = userRepository.save(User.builder()
                                        .username("JROMERO")
                                        .name("JUAN")
                                        .surname("ROMERO")
                                        .dni("00000000")
                                        .email("jca@gmail.com")
                                        .address("cr 12 h 34")
                                        .gender("M")
                                        .district(district)
                                        .districtId(district.getId())
                                        .client(systemClient)
                                        .clientId(systemClient.getId())
                                        .mobile("00000000")
                                        .password(passwordEncoder.encode("n>53F-8W5L7Dw+"))
                                        .status(true)
                                        .registrationDate(OffsetDateTime.now())
                                        .updateDate(OffsetDateTime.now())
                                .build());

                        // example one role and one access
                        Access access = accessRepository.save(Access.builder()
                                .name("USER_GET")
                                .status(true)
                                .registrationDate(OffsetDateTime.now())
                                .updateDate(OffsetDateTime.now())
                                .user(adminUser)
                                .userId(adminUser.getId())
                                .build());

                        Role role = roleRepository.save(Role.builder()
                                .user(adminUser)
                                .userId(adminUser.getId())
                                .name("ADMINISTRACION")
                                .status(true)
                                .registrationDate(OffsetDateTime.now())
                                .updateDate(OffsetDateTime.now())
                                .build());

                        userRoleRepository.save(UserRole.builder()
                                        .user(adminUser)
                                        .userId(adminUser.getId())
                                        .role(role)
                                        .roleId(role.getId())
                                        .status(true)
                                        .registrationDate(OffsetDateTime.now())
                                        .updateDate(OffsetDateTime.now())
                                .build());

                        roleAccessRepository.save(RoleAccess.builder()
                                        .role(role)
                                        .access(access)
                                        .user(adminUser)
                                        .registrationDate(OffsetDateTime.now())
                                        .updateDate(OffsetDateTime.now())
                                        .status(true)
                                        .roleId(role.getId())
                                        .accessId(access.getId())
                                .build());

                        // user for register new users
                        User registerUser = userRepository.save(User.builder()
                                        .username("REGISTER")
                                        .name("REGISTER")
                                        .surname("REGISTER")
                                        .dni("REGISTER")
                                        .email("REGISTER")
                                        .address("REGISTER")
                                        .gender("REGISTER")
                                        .mobile("00000000")
                                        .district(district)
                                        .districtId(district.getId())
                                        .password(passwordEncoder.encode("321abc+"))
                                        .status(true)
                                        .registrationDate(OffsetDateTime.now())
                                        .updateDate(OffsetDateTime.now())
                                        .client(systemClient)
                                        .clientId(systemClient.getId())
                                .build());

                        // mock departments peru
                        List<LocationDTO> listDepartment = iJsonFileReader.filterDepartment();

                        for (LocationDTO locationDepartment : listDepartment) {
                                iDepartment.save(locationDepartment.getDepartment(), adminUser.getUsername());
                        }

                        iDepartment.save("NO APLICA",adminUser.getUsername());

                        List<LocationDTO> listProvince = iJsonFileReader.filterProvince();

                        for (LocationDTO locationProvince : listProvince) {
                                iProvince.save(locationProvince.getProvince(), adminUser.getUsername(), locationProvince.getDepartment());
                        }

                        iProvince.save("NO APLICA",adminUser.getUsername(),"NO APLICA");

                        List<LocationDTO> listDistrict = iJsonFileReader.filterDistrict();

                        for (LocationDTO locationDistrict : listDistrict) {
                                iDistrict.save(locationDistrict.getDistrict(), adminUser.getUsername(), locationDistrict.getProvince());
                        }

                        iDistrict.save("NO APLICA",adminUser.getUsername(),"NO APLICA");

                        District districtB = districtRepository.findByNameAndStatusTrue("BREÑA");

                        Client client1 = clientRepository.save(Client.builder()
                                        .name("JOEL")
                                        .surname("COILA OSNAYO")
                                        .ruc("20609605601")
                                        .dni("1111111")
                                        .business("CORPORACION ARANNI S.A.C")
                                        .mobile("947424006")
                                        .address("Iquique 807 - breña")
                                        .email("joel@aranni.com.pe")
                                        .registrationDate(OffsetDateTime.now())
                                        .updateDate(OffsetDateTime.now())
                                        .status(true)
                                        .district(districtB)
                                        .districtId(districtB.getId())
                                .build());

                        // mock countries

                        List<CountryDTO> listCountry = iJsonFileReader.filterCountry();

                        for(CountryDTO country : listCountry){
                                iCountry.save(country.getValue(),adminUser.getUsername());
                        }

                        // audit
                        iAuditEvent.save("ACTIVATE_ACCESS",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_BRAND",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_CANCELLATION_REASON",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_CATEGORY",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_CATEGORY_PRODUCT",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_CLIENT",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_CLOSING_CHANNEL",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_COLOR",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_COURIER",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_CUSTOMER_TYPE",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_DELIVERY_STATUS",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_DELIVERY_MANIFEST_STATUS",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_DEPARTMENT",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_DISTRICT",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_DEMO_ACCOUNT",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_ENTRY_CHANNEL",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_MANAGEMENT_TYPE",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_MEMBERSHIP_STATE",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_MODEL",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_MODULE",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_ORDER_PAYMENT_METHOD",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_ORDER_PAYMENT_STATE",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_ORDER_RETURN_ITEM",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_ORDER_RETURN_TYPE",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_ORDER_STATE",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_ORDER_STOCK_ITEM",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_PAYMENT_GATEWAY",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_PRODUCT",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_PURCHASE_DOCUMENT",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_PURCHASE_ITEM",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_PROVINCE",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_ROLE",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_ROLE_ACCESS",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_SALE_CHANNEL",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_PURCHASE_TYPE",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_PURCHASE_PAYMENT_TYPE",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_SIZE",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_SIZE_TYPE",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_STOCK_REPLENISHMENT_ITEM",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_STOCK_TRANSACTION_TYPE",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_STORE",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_STORE_TYPE",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_SUBSCRIPTION",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_SUB_CATEGORY_PRODUCT",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_SUPPLIER",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_SUPPLIER_PRODUCT",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_SUPPLIER_TYPE",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_UNIT",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_UNIT_TYPE",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_USER",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_USER_ROLE",adminUser.getUsername());
                        iAuditEvent.save("ACTIVATE_WAREHOUSE",adminUser.getUsername());
                        iAuditEvent.save("ADD_ACCESS",adminUser.getUsername());
                        iAuditEvent.save("ADD_BRAND",adminUser.getUsername());
                        iAuditEvent.save("ADD_CANCELLATION_REASON",adminUser.getUsername());
                        iAuditEvent.save("ADD_CANCELLED_ORDER",adminUser.getUsername());
                        iAuditEvent.save("ADD_CATEGORY",adminUser.getUsername());
                        iAuditEvent.save("ADD_CATEGORY_PRODUCT",adminUser.getUsername());
                        iAuditEvent.save("ADD_CLOSING_CHANNEL",adminUser.getUsername());
                        iAuditEvent.save("ADD_COLOR",adminUser.getUsername());
                        iAuditEvent.save("ADD_COUNTRY",adminUser.getUsername());
                        iAuditEvent.save("ADD_COURIER",adminUser.getUsername());
                        iAuditEvent.save("ADD_COURIER_PICTURE",adminUser.getUsername());
                        iAuditEvent.save("ADD_CUSTOMER",adminUser.getUsername());
                        iAuditEvent.save("ADD_CUSTOMER_TYPE",adminUser.getUsername());
                        iAuditEvent.save("ADD_DELIVERY_POINT",adminUser.getUsername());
                        iAuditEvent.save("ADD_DELIVERY_MANIFEST",adminUser.getUsername());
                        iAuditEvent.save("ADD_DELIVERY_MANIFEST_ITEM",adminUser.getUsername());
                        iAuditEvent.save("ADD_DELIVERY_STATUS",adminUser.getUsername());
                        iAuditEvent.save("ADD_DELIVERY_MANIFEST_STATUS",adminUser.getUsername());
                        iAuditEvent.save("ADD_DEPARTMENT",adminUser.getUsername());
                        iAuditEvent.save("ADD_DISCOUNT",adminUser.getUsername());
                        iAuditEvent.save("ADD_DISTRICT",adminUser.getUsername());
                        iAuditEvent.save("ADD_ENTRY_CHANNEL",adminUser.getUsername());
                        iAuditEvent.save("ADD_GENERAL_STOCK",adminUser.getUsername());
                        iAuditEvent.save("ADD_MANAGEMENT_TYPE",adminUser.getUsername());
                        iAuditEvent.save("ADD_MEMBERSHIP",adminUser.getUsername());
                        iAuditEvent.save("ADD_MEMBERSHIP_PAYMENT",adminUser.getUsername());
                        iAuditEvent.save("ADD_MEMBERSHIP_STATE",adminUser.getUsername());
                        iAuditEvent.save("ADD_MERCADO_PAGO_PAYMENT",adminUser.getUsername());
                        iAuditEvent.save("ADD_MODEL",adminUser.getUsername());
                        iAuditEvent.save("ADD_MODULE",adminUser.getUsername());
                        iAuditEvent.save("ADD_ORDER",adminUser.getUsername());
                        iAuditEvent.save("ADD_ORDER_ITEM",adminUser.getUsername());
                        iAuditEvent.save("ADD_ORDER_PAYMENT_METHOD",adminUser.getUsername());
                        iAuditEvent.save("ADD_ORDER_PAYMENT_RECEIPT",adminUser.getUsername());
                        iAuditEvent.save("ADD_ORDER_PAYMENT_STATE",adminUser.getUsername());
                        iAuditEvent.save("ADD_ORDER_RETURN",adminUser.getUsername());
                        iAuditEvent.save("ADD_ORDER_RETURN_ITEM",adminUser.getUsername());
                        iAuditEvent.save("ADD_ORDER_RETURN_EXCEL",adminUser.getUsername());
                        iAuditEvent.save("ADD_ORDER_RETURN_TYPE",adminUser.getUsername());
                        iAuditEvent.save("ADD_ORDER_STATE",adminUser.getUsername());
                        iAuditEvent.save("ADD_ORDER_STOCK",adminUser.getUsername());
                        iAuditEvent.save("ADD_ORDER_STOCK_EXCEL",adminUser.getUsername());
                        iAuditEvent.save("ADD_ORDER_STOCK_ITEM",adminUser.getUsername());
                        iAuditEvent.save("ADD_PAYMENT_GATEWAY",adminUser.getUsername());
                        iAuditEvent.save("ADD_PRODUCT",adminUser.getUsername());
                        iAuditEvent.save("ADD_PRODUCT_EXCEL",adminUser.getUsername());
                        iAuditEvent.save("ADD_PRODUCT_PICTURE",adminUser.getUsername());
                        iAuditEvent.save("ADD_PRODUCT_PRICE",adminUser.getUsername());
                        iAuditEvent.save("ADD_PROVINCE",adminUser.getUsername());
                        iAuditEvent.save("ADD_PURCHASE",adminUser.getUsername());
                        iAuditEvent.save("ADD_PURCHASE_DOCUMENT",adminUser.getUsername());
                        iAuditEvent.save("ADD_PURCHASE_EXCEL",adminUser.getUsername());
                        iAuditEvent.save("ADD_PURCHASE_ITEM",adminUser.getUsername());
                        iAuditEvent.save("ADD_PURCHASE_PAYMENT_TYPE",adminUser.getUsername());
                        iAuditEvent.save("ADD_ROLE",adminUser.getUsername());
                        iAuditEvent.save("ADD_ROLE_ACCESS",adminUser.getUsername());
                        iAuditEvent.save("ADD_SALE_CHANNEL",adminUser.getUsername());
                        iAuditEvent.save("ADD_PURCHASE_TYPE",adminUser.getUsername());
                        iAuditEvent.save("ADD_SIZE",adminUser.getUsername());
                        iAuditEvent.save("ADD_SIZE_TYPE",adminUser.getUsername());
                        iAuditEvent.save("ADD_STOCK_REPLENISHMENT_EXCEL",adminUser.getUsername());
                        iAuditEvent.save("ADD_STOCK_RETURN_EXCEL",adminUser.getUsername());
                        iAuditEvent.save("ADD_STOCK_TRANSFER_EXCEL",adminUser.getUsername());
                        iAuditEvent.save("ADD_STOCK_REPLENISHMENT",adminUser.getUsername());
                        iAuditEvent.save("ADD_STOCK_REPLENISHMENT_ITEM",adminUser.getUsername());
                        iAuditEvent.save("ADD_STOCK_RETURN",adminUser.getUsername());
                        iAuditEvent.save("ADD_STOCK_RETURN_ITEM",adminUser.getUsername());
                        iAuditEvent.save("ADD_STOCK_TRANSACTION",adminUser.getUsername());
                        iAuditEvent.save("ADD_STOCK_TRANSACTION_ITEM",adminUser.getUsername());
                        iAuditEvent.save("ADD_STOCK_TRANSACTION_TYPE",adminUser.getUsername());
                        iAuditEvent.save("ADD_STOCK_TRANSFER",adminUser.getUsername());
                        iAuditEvent.save("ADD_STOCK_TRANSFER_ITEM",adminUser.getUsername());
                        iAuditEvent.save("ADD_STORE",adminUser.getUsername());
                        iAuditEvent.save("ADD_STORE_TYPE",adminUser.getUsername());
                        iAuditEvent.save("ADD_SUBSCRIPTION",adminUser.getUsername());
                        iAuditEvent.save("ADD_SUB_CATEGORY_PRODUCT",adminUser.getUsername());
                        iAuditEvent.save("ADD_SUPPLIER",adminUser.getUsername());
                        iAuditEvent.save("ADD_SUPPLIER_PRODUCT",adminUser.getUsername());
                        iAuditEvent.save("ADD_SUPPLIER_PRODUCT_EXCEL",adminUser.getUsername());
                        iAuditEvent.save("ADD_SUPPLIER_TYPE",adminUser.getUsername());
                        iAuditEvent.save("ADD_UNIT",adminUser.getUsername());
                        iAuditEvent.save("ADD_UNIT_TYPE",adminUser.getUsername());
                        iAuditEvent.save("ADD_USER",adminUser.getUsername());
                        iAuditEvent.save("ADD_USER_ROLE",adminUser.getUsername());
                        iAuditEvent.save("ADD_WAREHOUSE",adminUser.getUsername());
                        iAuditEvent.save("ADD_WAREHOUSE_STOCK",adminUser.getUsername());
                        iAuditEvent.save("DELETE_ACCESS",adminUser.getUsername());
                        iAuditEvent.save("DELETE_BRAND",adminUser.getUsername());
                        iAuditEvent.save("DELETE_CANCELLATION_REASON",adminUser.getUsername());
                        iAuditEvent.save("DELETE_CATEGORY",adminUser.getUsername());
                        iAuditEvent.save("DELETE_CATEGORY_PRODUCT",adminUser.getUsername());
                        iAuditEvent.save("DELETE_CLIENT",adminUser.getUsername());
                        iAuditEvent.save("DELETE_CLOSING_CHANNEL",adminUser.getUsername());
                        iAuditEvent.save("DELETE_COLOR",adminUser.getUsername());
                        iAuditEvent.save("DELETE_COURIER",adminUser.getUsername());
                        iAuditEvent.save("DELETE_CUSTOMER_TYPE",adminUser.getUsername());
                        iAuditEvent.save("DELETE_DELIVERY_MANIFEST",adminUser.getUsername());
                        iAuditEvent.save("DELETE_DELIVERY_STATUS",adminUser.getUsername());
                        iAuditEvent.save("DELETE_DELIVERY_MANIFEST_STATUS",adminUser.getUsername());
                        iAuditEvent.save("DELETE_DEPARTMENT",adminUser.getUsername());
                        iAuditEvent.save("DELETE_DISTRICT",adminUser.getUsername());
                        iAuditEvent.save("DELETE_ENTRY_CHANNEL",adminUser.getUsername());
                        iAuditEvent.save("DELETE_GENERAL_STOCK",adminUser.getUsername());
                        iAuditEvent.save("DELETE_MANAGEMENT_TYPE",adminUser.getUsername());
                        iAuditEvent.save("DELETE_MEMBERSHIP",adminUser.getUsername());
                        iAuditEvent.save("DELETE_MEMBERSHIP_STATE",adminUser.getUsername());
                        iAuditEvent.save("DELETE_MODEL",adminUser.getUsername());
                        iAuditEvent.save("DELETE_MODULE",adminUser.getUsername());
                        iAuditEvent.save("DELETE_ORDER_ITEM",adminUser.getUsername());
                        iAuditEvent.save("DELETE_ORDER_PAYMENT_METHOD",adminUser.getUsername());
                        iAuditEvent.save("DELETE_ORDER_PAYMENT_STATE",adminUser.getUsername());
                        iAuditEvent.save("DELETE_ORDER_RETURN_ITEM",adminUser.getUsername());
                        iAuditEvent.save("DELETE_ORDER_RETURN_TYPE",adminUser.getUsername());
                        iAuditEvent.save("DELETE_ORDER_STATE",adminUser.getUsername());
                        iAuditEvent.save("DELETE_ORDER_STOCK_ITEM",adminUser.getUsername());
                        iAuditEvent.save("DELETE_PAYMENT_GATEWAY",adminUser.getUsername());
                        iAuditEvent.save("DELETE_PRODUCT",adminUser.getUsername());
                        iAuditEvent.save("DELETE_PRODUCT_PRICE",adminUser.getUsername());
                        iAuditEvent.save("DELETE_PROVINCE",adminUser.getUsername());
                        iAuditEvent.save("DELETE_PURCHASE_DOCUMENT",adminUser.getUsername());
                        iAuditEvent.save("DELETE_PURCHASE_ITEM",adminUser.getUsername());
                        iAuditEvent.save("DELETE_ROLE",adminUser.getUsername());
                        iAuditEvent.save("DELETE_ROLE_ACCESS",adminUser.getUsername());
                        iAuditEvent.save("DELETE_SALE_CHANNEL",adminUser.getUsername());
                        iAuditEvent.save("DELETE_PURCHASE_TYPE",adminUser.getUsername());
                        iAuditEvent.save("DELETE_PURCHASE_PAYMENT_TYPE",adminUser.getUsername());
                        iAuditEvent.save("DELETE_SIZE",adminUser.getUsername());
                        iAuditEvent.save("DELETE_SIZE_TYPE",adminUser.getUsername());
                        iAuditEvent.save("DELETE_STOCK_REPLENISHMENT_ITEM",adminUser.getUsername());
                        iAuditEvent.save("DELETE_STOCK_TRANSACTION_TYPE",adminUser.getUsername());
                        iAuditEvent.save("DELETE_STORE",adminUser.getUsername());
                        iAuditEvent.save("DELETE_STORE_TYPE",adminUser.getUsername());
                        iAuditEvent.save("DELETE_SUBSCRIPTION",adminUser.getUsername());
                        iAuditEvent.save("DELETE_SUB_CATEGORY_PRODUCT",adminUser.getUsername());
                        iAuditEvent.save("DELETE_SUPPLIER",adminUser.getUsername());
                        iAuditEvent.save("DELETE_SUPPLIER_PRODUCT",adminUser.getUsername());
                        iAuditEvent.save("DELETE_SUPPLIER_TYPE",adminUser.getUsername());
                        iAuditEvent.save("DELETE_UNIT",adminUser.getUsername());
                        iAuditEvent.save("DELETE_UNIT_TYPE",adminUser.getUsername());
                        iAuditEvent.save("DELETE_USER",adminUser.getUsername());
                        iAuditEvent.save("DELETE_USER_ROLE",adminUser.getUsername());
                        iAuditEvent.save("DELETE_WAREHOUSE",adminUser.getUsername());
                        iAuditEvent.save("DELETE_WAREHOUSE_STOCK",adminUser.getUsername());
                        iAuditEvent.save("LOG_IN",adminUser.getUsername());
                        iAuditEvent.save("LOG_OUT",adminUser.getUsername());
                        iAuditEvent.save("REGISTER_CLIENT",adminUser.getUsername());
                        iAuditEvent.save("SEND_MERCADO_PAGO_PAYMENT",adminUser.getUsername());
                        iAuditEvent.save("SEND_SUBSCRIPTION_PAYMENT",adminUser.getUsername());
                        iAuditEvent.save("UPDATE_CATEGORY",adminUser.getUsername());
                        iAuditEvent.save("UPDATE_CATEGORY_PRODUCT",adminUser.getUsername());
                        iAuditEvent.save("UPDATE_CLIENT",adminUser.getUsername());
                        iAuditEvent.save("UPDATE_COURIER_ORDER",adminUser.getUsername());
                        iAuditEvent.save("UPDATE_DELIVERY_MANIFEST_ITEM",adminUser.getUsername());
                        iAuditEvent.save("UPDATE_MODULE",adminUser.getUsername());
                        iAuditEvent.save("UPDATE_ORDER",adminUser.getUsername());
                        iAuditEvent.save("UPDATE_ORDER_ITEM",adminUser.getUsername());
                        iAuditEvent.save("UPDATE_ORDER_RETURN_ITEM",adminUser.getUsername());
                        iAuditEvent.save("UPDATE_ORDER_STOCK_ITEM",adminUser.getUsername());
                        iAuditEvent.save("UPDATE_PRODUCT",adminUser.getUsername());
                        iAuditEvent.save("UPDATE_STOCK_REPLENISHMENT_ITEM",adminUser.getUsername());
                        iAuditEvent.save("UPDATE_STORE",adminUser.getUsername());
                        iAuditEvent.save("UPDATE_USER",adminUser.getUsername());
                        // supplier types
                        iSupplierType.save("INTERNO",adminUser.getUsername());
                        iSupplierType.save("DISTRIBUIDOR",adminUser.getUsername());

                        // access
                        iAccess.save("ACCESS_POST",adminUser.getUsername());
                        iAccess.save("ACCESS_DELETE",adminUser.getUsername());
                        iAccess.save("ACCESS_GET",adminUser.getUsername());
                        iAccess.save("ACCESS_PUT",adminUser.getUsername());
                        iAccess.save("BRAND_POST",adminUser.getUsername());
                        iAccess.save("BRAND_DELETE",adminUser.getUsername());
                        iAccess.save("BRAND_GET",adminUser.getUsername());
                        iAccess.save("BRAND_PUT",adminUser.getUsername());
                        iAccess.save("CANCELLATION_REASON_POST",adminUser.getUsername());
                        iAccess.save("CANCELLATION_REASON_GET",adminUser.getUsername());
                        iAccess.save("CANCELLATION_REASON_PUT",adminUser.getUsername());
                        iAccess.save("CANCELLED_ORDER_POST",adminUser.getUsername());
                        iAccess.save("CANCELLED_ORDER_GET",adminUser.getUsername());
                        iAccess.save("CATEGORY_GET",adminUser.getUsername());
                        iAccess.save("CATEGORY_POST",adminUser.getUsername());
                        iAccess.save("CATEGORY_PUT",adminUser.getUsername());
                        iAccess.save("CATEGORY_DELETE",adminUser.getUsername());
                        iAccess.save("CATEGORY_PRODUCT_POST",adminUser.getUsername());
                        iAccess.save("CATEGORY_PRODUCT_GET",adminUser.getUsername());
                        iAccess.save("CLIENT_GET",adminUser.getUsername());
                        iAccess.save("CLIENT_POST",adminUser.getUsername());
                        iAccess.save("CLIENT_PUT",adminUser.getUsername());
                        iAccess.save("CLIENT_DELETE",adminUser.getUsername());
                        iAccess.save("CLOSING_CHANNEL_POST",adminUser.getUsername());
                        iAccess.save("COLOR_POST",adminUser.getUsername());
                        iAccess.save("COLOR_GET",adminUser.getUsername());
                        iAccess.save("COLOR_DELETE",adminUser.getUsername());
                        iAccess.save("COURIER_POST",adminUser.getUsername());
                        iAccess.save("COURIER_PUT",adminUser.getUsername());
                        iAccess.save("COURIER_GET",adminUser.getUsername());
                        iAccess.save("DELIVERY_STATUS_GET",adminUser.getUsername());
                        iAccess.save("DELIVERY_STATUS_POST",adminUser.getUsername());
                        iAccess.save("DELIVERY_STATUS_DELETE",adminUser.getUsername());
                        iAccess.save("DELIVERY_STATUS_PUT",adminUser.getUsername());
                        iAccess.save("DELIVERY_MANIFEST_STATUS_GET",adminUser.getUsername());
                        iAccess.save("DELIVERY_MANIFEST_STATUS_POST",adminUser.getUsername());
                        iAccess.save("DELIVERY_MANIFEST_STATUS_DELETE",adminUser.getUsername());
                        iAccess.save("DELIVERY_MANIFEST_STATUS_PUT",adminUser.getUsername());
                        iAccess.save("DELIVERY_MANIFEST_ITEM_GET",adminUser.getUsername());
                        iAccess.save("DELIVERY_MANIFEST_ITEM_PUT",adminUser.getUsername());
                        iAccess.save("DEPARTMENT_GET",adminUser.getUsername());
                        iAccess.save("DEPARTMENT_POST",adminUser.getUsername());
                        iAccess.save("DEPARTMENT_DELETE",adminUser.getUsername());
                        iAccess.save("DISTRICT_POST",adminUser.getUsername());
                        iAccess.save("DISTRICT_DELETE",adminUser.getUsername());
                        iAccess.save("ENTRY_CHANNEL_POST",adminUser.getUsername());
                        iAccess.save("GENERAL_STOCK_GET",adminUser.getUsername());
                        iAccess.save("MEMBERSHIP_GET",adminUser.getUsername());
                        iAccess.save("MEMBERSHIP_PAYMENT_GET",adminUser.getUsername());
                        iAccess.save("MODEL_GET",adminUser.getUsername());
                        iAccess.save("MODEL_POST",adminUser.getUsername());
                        iAccess.save("MODEL_DELETE",adminUser.getUsername());
                        iAccess.save("MODULE_GET",adminUser.getUsername());
                        iAccess.save("MODULE_POST",adminUser.getUsername());
                        iAccess.save("MODULE_PUT",adminUser.getUsername());
                        iAccess.save("MODULE_DELETE",adminUser.getUsername());
                        iAccess.save("ONBOARD_GET",adminUser.getUsername());
                        iAccess.save("ORDER_GET",adminUser.getUsername());
                        iAccess.save("ORDER_POST",adminUser.getUsername());
                        iAccess.save("ORDER_PUT",adminUser.getUsername());
                        iAccess.save("ORDER_ITEM_GET",adminUser.getUsername());
                        iAccess.save("ORDER_ITEM_POST",adminUser.getUsername());
                        iAccess.save("ORDER_ITEM_DELETE",adminUser.getUsername());
                        iAccess.save("ORDER_ITEM_PUT",adminUser.getUsername());
                        iAccess.save("ORDER_STOCK_GET",adminUser.getUsername());
                        iAccess.save("ORDER_STOCK_POST",adminUser.getUsername());
                        iAccess.save("ORDER_STOCK_ITEM_GET",adminUser.getUsername());
                        iAccess.save("ORDER_PAYMENT_METHOD_POST",adminUser.getUsername());
                        iAccess.save("ORDER_PAYMENT_METHOD_GET",adminUser.getUsername());
                        iAccess.save("ORDER_PAYMENT_METHOD_DELETE",adminUser.getUsername());
                        iAccess.save("ORDER_PAYMENT_STATE_GET",adminUser.getUsername());
                        iAccess.save("ORDER_PAYMENT_STATE_POST",adminUser.getUsername());
                        iAccess.save("ORDER_PAYMENT_STATE_DELETE",adminUser.getUsername());
                        iAccess.save("ORDER_STATE_GET",adminUser.getUsername());
                        iAccess.save("ORDER_STATE_POST",adminUser.getUsername());
                        iAccess.save("ORDER_STATE_DELETE",adminUser.getUsername());
                        iAccess.save("PAYMENT_GATEWAY_POST",adminUser.getUsername());
                        iAccess.save("PRODUCT_GET",adminUser.getUsername());
                        iAccess.save("PRODUCT_POST",adminUser.getUsername());
                        iAccess.save("PRODUCT_DELETE",adminUser.getUsername());
                        iAccess.save("PRODUCT_PRICE_POST",adminUser.getUsername());
                        iAccess.save("PROVINCE_GET",adminUser.getUsername());
                        iAccess.save("PROVINCE_POST",adminUser.getUsername());
                        iAccess.save("PROVINCE_DELETE",adminUser.getUsername());
                        iAccess.save("PURCHASE_GET",adminUser.getUsername());
                        iAccess.save("PURCHASE_POST",adminUser.getUsername());
                        iAccess.save("PURCHASE_DOCUMENT_POST",adminUser.getUsername());
                        iAccess.save("PURCHASE_DOCUMENT_DELETE",adminUser.getUsername());
                        iAccess.save("PURCHASE_DOCUMENT_GET",adminUser.getUsername());
                        iAccess.save("PURCHASE_ITEM_GET",adminUser.getUsername());
                        iAccess.save("ROLE_POST",adminUser.getUsername());
                        iAccess.save("ROLE_GET",adminUser.getUsername());
                        iAccess.save("ROLE_DELETE",adminUser.getUsername());
                        iAccess.save("ROLE_PUT",adminUser.getUsername());
                        iAccess.save("ROLE_ACCESS_GET",adminUser.getUsername());
                        iAccess.save("ROLE_ACCESS_POST",adminUser.getUsername());
                        iAccess.save("ROLE_ACCESS_PUT",adminUser.getUsername());
                        iAccess.save("ROLE_ACCESS_DELETE",adminUser.getUsername());
                        iAccess.save("SALE_CHANNEL_POST",adminUser.getUsername());
                        iAccess.save("SALE_CHANNEL_DELETE",adminUser.getUsername());
                        iAccess.save("PURCHASE_TYPE_POST",adminUser.getUsername());
                        iAccess.save("PURCHASE_TYPE_GET",adminUser.getUsername());
                        iAccess.save("PURCHASE_PAYMENT_TYPE_GET",adminUser.getUsername());
                        iAccess.save("PURCHASE_PAYMENT_TYPE_POST",adminUser.getUsername());
                        iAccess.save("PURCHASE_PAYMENT_TYPE_PUT",adminUser.getUsername());
                        iAccess.save("SIZE_GET",adminUser.getUsername());
                        iAccess.save("SIZE_POST",adminUser.getUsername());
                        iAccess.save("SIZE_DELETE",adminUser.getUsername());
                        iAccess.save("SIZE_TYPE_GET",adminUser.getUsername());
                        iAccess.save("SIZE_TYPE_POST",adminUser.getUsername());
                        iAccess.save("SIZE_TYPE_DELETE",adminUser.getUsername());
                        iAccess.save("STOCK_REPLENISHMENT_POST",adminUser.getUsername());
                        iAccess.save("STOCK_REPLENISHMENT_GET",adminUser.getUsername());
                        iAccess.save("STOCK_REPLENISHMENT_ITEM_GET",adminUser.getUsername());
                        iAccess.save("STOCK_RETURN_POST",adminUser.getUsername());
                        iAccess.save("STOCK_RETURN_GET",adminUser.getUsername());
                        iAccess.save("STOCK_RETURN_ITEM_GET",adminUser.getUsername());
                        iAccess.save("STOCK_TRANSACTION_GET",adminUser.getUsername());
                        iAccess.save("STOCK_TRANSACTION_ITEM_GET",adminUser.getUsername());
                        iAccess.save("STOCK_TRANSACTION_TYPE_GET",adminUser.getUsername());
                        iAccess.save("STOCK_TRANSACTION_TYPE_POST",adminUser.getUsername());
                        iAccess.save("STOCK_TRANSACTION_TYPE_DELETE",adminUser.getUsername());
                        iAccess.save("STOCK_TRANSFER_POST",adminUser.getUsername());
                        iAccess.save("STOCK_TRANSFER_GET",adminUser.getUsername());
                        iAccess.save("STOCK_TRANSFER_ITEM_GET",adminUser.getUsername());
                        iAccess.save("STORE_GET",adminUser.getUsername());
                        iAccess.save("STORE_POST",adminUser.getUsername());
                        iAccess.save("STORE_PUT",adminUser.getUsername());
                        iAccess.save("STORE_DELETE",adminUser.getUsername());
                        iAccess.save("STORE_TYPE_POST",adminUser.getUsername());
                        iAccess.save("SUB_CATEGORY_PRODUCT_GET",adminUser.getUsername());
                        iAccess.save("SUB_CATEGORY_PRODUCT_POST",adminUser.getUsername());
                        iAccess.save("SUB_CATEGORY_PRODUCT_DELETE",adminUser.getUsername());
                        iAccess.save("SUB_CATEGORY_PRODUCT_PUT",adminUser.getUsername());
                        iAccess.save("SUBSCRIPTION_GET",adminUser.getUsername());
                        iAccess.save("SUBSCRIPTION_POST",adminUser.getUsername());
                        iAccess.save("SUBSCRIPTION_PAYMENT_POST",adminUser.getUsername());
                        iAccess.save("SUPPLIER_GET",adminUser.getUsername());
                        iAccess.save("SUPPLIER_POST",adminUser.getUsername());
                        iAccess.save("SUPPLIER_DELETE",adminUser.getUsername());
                        iAccess.save("SUPPLIER_PRODUCT_GET",adminUser.getUsername());
                        iAccess.save("SUPPLIER_PRODUCT_POST",adminUser.getUsername());
                        iAccess.save("SUPPLIER_PRODUCT_DELETE",adminUser.getUsername());
                        iAccess.save("UNIT_GET",adminUser.getUsername());
                        iAccess.save("UNIT_POST",adminUser.getUsername());
                        iAccess.save("UNIT_DELETE",adminUser.getUsername());
                        iAccess.save("UNIT_TYPE_GET",adminUser.getUsername());
                        iAccess.save("UNIT_TYPE_POST",adminUser.getUsername());
                        iAccess.save("UNIT_TYPE_DELETE",adminUser.getUsername());
                        iAccess.save("USER_POST",adminUser.getUsername());
                        iAccess.save("USER_PUT",adminUser.getUsername());
                        iAccess.save("USER_DELETE",adminUser.getUsername());
                        iAccess.save("USER_ROLE_POST",adminUser.getUsername());
                        iAccess.save("WAREHOUSE_GET",adminUser.getUsername());
                        iAccess.save("WAREHOUSE_POST",adminUser.getUsername());
                        iAccess.save("WAREHOUSE_STOCK_GET",adminUser.getUsername());
                        // roles
                        iRole.save("NEGOCIO",adminUser.getUsername());
                        iRole.save("VENTAS",adminUser.getUsername());
                        iRole.save("STOCK",adminUser.getUsername());
                        iRole.save("SERVICIO_CLIENTE",adminUser.getUsername());
                        iRole.save("COURIER",adminUser.getUsername());
                        iRole.save("MARKETING",adminUser.getUsername());
                        // roles by access
                        iRoleAccess.save("VENTAS","BRAND_GET",adminUser.getUsername());
                        iRoleAccess.save("VENTAS","CANCELLATION_REASON_GET",adminUser.getUsername());
                        iRoleAccess.save("VENTAS","CANCELLED_ORDER_GET",adminUser.getUsername());
                        iRoleAccess.save("VENTAS","CANCELLED_ORDER_POST",adminUser.getUsername());
                        iRoleAccess.save("VENTAS","COURIER_GET",adminUser.getUsername());
                        iRoleAccess.save("VENTAS","MODEL_GET",adminUser.getUsername());
                        iRoleAccess.save("VENTAS","ORDER_GET",adminUser.getUsername());
                        iRoleAccess.save("VENTAS","ORDER_POST",adminUser.getUsername());
                        iRoleAccess.save("VENTAS","ORDER_PUT",adminUser.getUsername());
                        iRoleAccess.save("VENTAS","ORDER_ITEM_GET",adminUser.getUsername());
                        iRoleAccess.save("VENTAS","ORDER_ITEM_POST",adminUser.getUsername());
                        iRoleAccess.save("VENTAS","ORDER_ITEM_PUT",adminUser.getUsername());
                        iRoleAccess.save("VENTAS","ORDER_ITEM_DELETE",adminUser.getUsername());
                        iRoleAccess.save("VENTAS","ORDER_PAYMENT_METHOD_GET",adminUser.getUsername());
                        iRoleAccess.save("VENTAS","ORDER_PAYMENT_STATE_GET",adminUser.getUsername());
                        iRoleAccess.save("VENTAS","ORDER_STATE_GET",adminUser.getUsername());
                        iRoleAccess.save("VENTAS","PRODUCT_GET",adminUser.getUsername());
                        iRoleAccess.save("VENTAS","STORE_GET",adminUser.getUsername());
                        iRoleAccess.save("SERVICIO_CLIENTE","BRAND_GET",adminUser.getUsername());
                        iRoleAccess.save("SERVICIO_CLIENTE","CANCELLATION_REASON_GET",adminUser.getUsername());
                        iRoleAccess.save("SERVICIO_CLIENTE","CANCELLED_ORDER_GET",adminUser.getUsername());
                        iRoleAccess.save("SERVICIO_CLIENTE","CANCELLED_ORDER_POST",adminUser.getUsername());
                        iRoleAccess.save("SERVICIO_CLIENTE","MODEL_GET",adminUser.getUsername());
                        iRoleAccess.save("SERVICIO_CLIENTE","COURIER_GET",adminUser.getUsername());
                        iRoleAccess.save("SERVICIO_CLIENTE","ORDER_GET",adminUser.getUsername());
                        iRoleAccess.save("SERVICIO_CLIENTE","ORDER_PUT",adminUser.getUsername());
                        iRoleAccess.save("SERVICIO_CLIENTE","ORDER_ITEM_GET",adminUser.getUsername());
                        iRoleAccess.save("SERVICIO_CLIENTE","ORDER_ITEM_POST",adminUser.getUsername());
                        iRoleAccess.save("SERVICIO_CLIENTE","ORDER_ITEM_PUT",adminUser.getUsername());
                        iRoleAccess.save("SERVICIO_CLIENTE","ORDER_ITEM_DELETE",adminUser.getUsername());
                        iRoleAccess.save("SERVICIO_CLIENTE","ORDER_PAYMENT_METHOD_GET",adminUser.getUsername());
                        iRoleAccess.save("SERVICIO_CLIENTE","ORDER_PAYMENT_STATE_GET",adminUser.getUsername());
                        iRoleAccess.save("SERVICIO_CLIENTE","ORDER_STATE_GET",adminUser.getUsername());
                        iRoleAccess.save("SERVICIO_CLIENTE","PRODUCT_GET",adminUser.getUsername());
                        iRoleAccess.save("SERVICIO_CLIENTE","STORE_GET",adminUser.getUsername());
                        iRoleAccess.save("MARKETING","BRAND_GET",adminUser.getUsername());
                        iRoleAccess.save("MARKETING","BRAND_POST",adminUser.getUsername());
                        iRoleAccess.save("MARKETING","BRAND_DELETE",adminUser.getUsername());
                        iRoleAccess.save("MARKETING","BRAND_PUT",adminUser.getUsername());
                        iRoleAccess.save("MARKETING","CATEGORY_PRODUCT_GET",adminUser.getUsername());
                        iRoleAccess.save("MARKETING","COLOR_GET",adminUser.getUsername());
                        iRoleAccess.save("MARKETING","MODEL_GET",adminUser.getUsername());
                        iRoleAccess.save("MARKETING","MODEL_POST",adminUser.getUsername());
                        iRoleAccess.save("MARKETING","MODEL_DELETE",adminUser.getUsername());
                        iRoleAccess.save("MARKETING","PRODUCT_GET",adminUser.getUsername());
                        iRoleAccess.save("MARKETING","PRODUCT_POST",adminUser.getUsername());
                        iRoleAccess.save("MARKETING","PRODUCT_DELETE",adminUser.getUsername());
                        iRoleAccess.save("MARKETING","PRODUCT_PRICE_POST",adminUser.getUsername());
                        iRoleAccess.save("MARKETING","SIZE_GET",adminUser.getUsername());
                        iRoleAccess.save("MARKETING","SIZE_TYPE_GET",adminUser.getUsername());
                        iRoleAccess.save("MARKETING","UNIT_GET",adminUser.getUsername());
                        iRoleAccess.save("MARKETING","UNIT_TYPE_GET",adminUser.getUsername());
                        iRoleAccess.save("STOCK","BRAND_GET",adminUser.getUsername());
                        iRoleAccess.save("STOCK","COLOR_GET",adminUser.getUsername());
                        iRoleAccess.save("STOCK","GENERAL_STOCK_GET",adminUser.getUsername());
                        iRoleAccess.save("STOCK","MODEL_GET",adminUser.getUsername());
                        iRoleAccess.save("STOCK","ORDER_GET",adminUser.getUsername());
                        iRoleAccess.save("STOCK","ORDER_ITEM_GET",adminUser.getUsername());
                        iRoleAccess.save("STOCK","ORDER_STOCK_GET",adminUser.getUsername());
                        iRoleAccess.save("STOCK","ORDER_STOCK_POST",adminUser.getUsername());
                        iRoleAccess.save("STOCK","ORDER_STOCK_ITEM_GET",adminUser.getUsername());
                        iRoleAccess.save("STOCK","PURCHASE_GET",adminUser.getUsername());
                        iRoleAccess.save("STOCK","PURCHASE_POST",adminUser.getUsername());
                        iRoleAccess.save("STOCK","PURCHASE_ITEM_GET",adminUser.getUsername());
                        iRoleAccess.save("STOCK","PURCHASE_TYPE_GET",adminUser.getUsername());
                        iRoleAccess.save("STOCK","SIZE_GET",adminUser.getUsername());
                        iRoleAccess.save("STOCK","SIZE_TYPE_GET",adminUser.getUsername());
                        iRoleAccess.save("STOCK","STOCK_REPLENISHMENT_GET",adminUser.getUsername());
                        iRoleAccess.save("STOCK","STOCK_REPLENISHMENT_POST",adminUser.getUsername());
                        iRoleAccess.save("STOCK","STOCK_REPLENISHMENT_ITEM_GET",adminUser.getUsername());
                        iRoleAccess.save("STOCK","STOCK_RETURN_GET",adminUser.getUsername());
                        iRoleAccess.save("STOCK","STOCK_RETURN_POST",adminUser.getUsername());
                        iRoleAccess.save("STOCK","STOCK_RETURN_ITEM_GET",adminUser.getUsername());
                        iRoleAccess.save("STOCK","STOCK_TRANSACTION_GET",adminUser.getUsername());
                        iRoleAccess.save("STOCK","STOCK_TRANSACTION_ITEM_GET",adminUser.getUsername());
                        iRoleAccess.save("STOCK","SUPPLIER_GET",adminUser.getUsername());
                        iRoleAccess.save("STOCK","SUPPLIER_POST",adminUser.getUsername());
                        iRoleAccess.save("STOCK","SUPPLIER_DELETE",adminUser.getUsername());
                        iRoleAccess.save("STOCK","SUPPLIER_PRODUCT_GET",adminUser.getUsername());
                        iRoleAccess.save("STOCK","SUPPLIER_PRODUCT_POST",adminUser.getUsername());
                        iRoleAccess.save("STOCK","SUPPLIER_PRODUCT_DELETE",adminUser.getUsername());
                        iRoleAccess.save("STOCK","UNIT_GET",adminUser.getUsername());
                        iRoleAccess.save("STOCK","UNIT_TYPE_GET",adminUser.getUsername());
                        iRoleAccess.save("STOCK","WAREHOUSE_GET",adminUser.getUsername());
                        iRoleAccess.save("STOCK","WAREHOUSE_POST",adminUser.getUsername());
                        iRoleAccess.save("COURIER","COURIER_PUT",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","BRAND_GET",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","CLIENT_GET",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","COLOR_GET",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","CANCELLED_ORDER_GET",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","COURIER_GET",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","COURIER_POST",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","GENERAL_STOCK_GET",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","MEMBERSHIP_GET",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","MEMBERSHIP_PAYMENT_GET",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","MODEL_GET",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","ORDER_GET",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","ORDER_ITEM_GET",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","ORDER_STOCK_GET",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","ORDER_STOCK_ITEM_GET",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","ORDER_PAYMENT_METHOD_GET",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","ORDER_PAYMENT_STATE_GET",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","ORDER_STATE_GET",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","PRODUCT_GET",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","PURCHASE_GET",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","PURCHASE_ITEM_GET",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","ROLE_GET",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","SIZE_GET",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","SIZE_TYPE_GET",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","STOCK_REPLENISHMENT_GET",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","STOCK_REPLENISHMENT_ITEM_GET",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","STOCK_RETURN_GET",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","STOCK_RETURN_ITEM_GET",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","STOCK_TRANSACTION_GET",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","STOCK_TRANSACTION_ITEM_GET",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","STOCK_TRANSFER_GET",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","STOCK_TRANSFER_ITEM_GET",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","STORE_GET",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","STORE_PUT",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","STORE_POST",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","STORE_DELETE",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","SUBSCRIPTION_GET",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","SUBSCRIPTION_PAYMENT_POST",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","SUPPLIER_GET",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","UNIT_GET",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","UNIT_TYPE_GET",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","USER_GET",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","USER_POST",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","WAREHOUSE_GET",adminUser.getUsername());
                        iRoleAccess.save("NEGOCIO","WAREHOUSE_STOCK_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","ACCESS_POST",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","ACCESS_DELETE",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","ACCESS_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","ACCESS_PUT",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","BRAND_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","CANCELLED_ORDER_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","CANCELLATION_REASON_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","CANCELLATION_REASON_POST",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","CANCELLATION_REASON_PUT",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","CATEGORY_POST",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","CATEGORY_DELETE",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","CATEGORY_PUT",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","CLIENT_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","CLIENT_POST",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","CLIENT_DELETE",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","CLOSING_CHANNEL_POST",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","COLOR_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","COLOR_POST",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","COLOR_DELETE",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","COURIER_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","COURIER_POST",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","DELIVERY_STATUS_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","DELIVERY_STATUS_DELETE",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","DELIVERY_STATUS_POST",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","DELIVERY_STATUS_PUT",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","DELIVERY_MANIFEST_STATUS_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","DELIVERY_MANIFEST_STATUS_DELETE",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","DELIVERY_MANIFEST_STATUS_POST",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","DELIVERY_MANIFEST_STATUS_PUT",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","DELIVERY_MANIFEST_ITEM_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","DELIVERY_MANIFEST_ITEM_PUT",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","DEPARTMENT_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","DEPARTMENT_POST",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","DEPARTMENT_DELETE",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","DISTRICT_POST",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","DISTRICT_DELETE",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","ENTRY_CHANNEL_POST",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","GENERAL_STOCK_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","MEMBERSHIP_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","MEMBERSHIP_PAYMENT_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","MODEL_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","MODULE_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","MODULE_POST",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","MODULE_DELETE",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","ONBOARD_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","ORDER_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","ORDER_ITEM_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","ORDER_STOCK_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","ORDER_STOCK_ITEM_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","ORDER_PAYMENT_METHOD_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","ORDER_PAYMENT_METHOD_POST",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","ORDER_PAYMENT_METHOD_DELETE",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","ORDER_PAYMENT_STATE_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","ORDER_PAYMENT_STATE_POST",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","ORDER_PAYMENT_STATE_DELETE",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","ORDER_STATE_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","ORDER_STATE_POST",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","ORDER_STATE_DELETE",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","PAYMENT_GATEWAY_POST",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","PRODUCT_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","PROVINCE_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","PROVINCE_POST",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","PROVINCE_DELETE",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","PURCHASE_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","PURCHASE_ITEM_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","PURCHASE_DOCUMENT_POST",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","PURCHASE_DOCUMENT_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","PURCHASE_DOCUMENT_DELETE",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","ROLE_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","ROLE_POST",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","ROLE_PUT",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","ROLE_DELETE",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","ROLE_ACCESS_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","ROLE_ACCESS_POST",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","ROLE_ACCESS_PUT",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","ROLE_ACCESS_DELETE",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","SALE_CHANNEL_DELETE",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","SALE_CHANNEL_POST",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","PURCHASE_TYPE_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","PURCHASE_TYPE_POST",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","PURCHASE_PAYMENT_TYPE_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","PURCHASE_PAYMENT_TYPE_POST",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","PURCHASE_PAYMENT_TYPE_PUT",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","SIZE_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","SIZE_POST",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","SIZE_DELETE",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","SIZE_TYPE_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","SIZE_TYPE_POST",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","SIZE_TYPE_DELETE",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","STOCK_REPLENISHMENT_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","STOCK_REPLENISHMENT_ITEM_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","STOCK_RETURN_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","STOCK_RETURN_ITEM_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","STOCK_TRANSACTION_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","STOCK_TRANSACTION_ITEM_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","STOCK_TRANSACTION_TYPE_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","STOCK_TRANSACTION_TYPE_POST",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","STOCK_TRANSFER_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","STOCK_TRANSFER_ITEM_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","STORE_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","SUB_CATEGORY_PRODUCT_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","SUB_CATEGORY_PRODUCT_POST",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","SUB_CATEGORY_PRODUCT_DELETE",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","SUB_CATEGORY_PRODUCT_PUT",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","SUBSCRIPTION_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","SUBSCRIPTION_POST",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","SUPPLIER_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","UNIT_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","UNIT_POST",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","UNIT_DELETE",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","UNIT_TYPE_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","UNIT_TYPE_POST",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","UNIT_TYPE_DELETE",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","USER_POST",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","USER_PUT",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","USER_DELETE",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","USER_ROLE_POST",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","WAREHOUSE_GET",adminUser.getUsername());
                        iRoleAccess.save("ADMINISTRACION","WAREHOUSE_STOCK_GET",adminUser.getUsername());
                        // mock modules
                        iModule.save("Módulo de Ventas", 3.00, adminUser.getUsername());
                        iModule.save("Módulo de Gestión", 5.00, adminUser.getUsername());
                        iModule.save("Analítica de Ventas", 3.00, adminUser.getUsername());
                        iModule.save("Integracion con Shopify", 5.00, adminUser.getUsername());
                        iModule.save("Módulo de Almacén", 5.00, adminUser.getUsername());
                        iModule.save("Facturación Electronica", 15.00, adminUser.getUsername());
                        iModule.save("Módulo de Remarketing", 8.00, adminUser.getUsername());
                        iModule.save("Integracion con Marketplace", 10.00, adminUser.getUsername());
                        iModule.save("Integracion Tienda Virtual", 10.00, adminUser.getUsername());
                        iModule.save("Modulo de Courier", 5.00, adminUser.getUsername());
                        iModule.save("Modulo de Finanzas", 5.00, adminUser.getUsername());

                        // mock subscriptions
                        iSubscription.save("mensual", 1, 0.00, adminUser.getUsername());
                        iSubscription.save("semestral", 6, 10.00, adminUser.getUsername());
                        iSubscription.save("anual", 12, 20.00, adminUser.getUsername());

                        // mock categories
                        iCategory.save("Joyas y bisuteria", "Joyas y bisuteria", adminUser.getUsername());
                        iCategory.save("Moda", "Moda", adminUser.getUsername());
                        iCategory.save("Tecnologia", "Tecnologia", adminUser.getUsername());
                        iCategory.save("Cosmeticos", "Cosmeticos", adminUser.getUsername());
                        iCategory.save("Otro", "Otro", adminUser.getUsername());

                        // mock closing channels
                        iClosingChannel.save("WHATSAPP", adminUser.getUsername());
                        iClosingChannel.save("INSTAGRAM", adminUser.getUsername());
                        iClosingChannel.save("FACEBOOK", adminUser.getUsername());
                        iClosingChannel.save("TIENDA", adminUser.getUsername());
                        iClosingChannel.save("WEB", adminUser.getUsername());
                        iClosingChannel.save("MARKET PLACE", adminUser.getUsername());
                        iClosingChannel.save("TIK TOK", adminUser.getUsername());
                        iClosingChannel.save("OTRO", adminUser.getUsername());

                        // mock entry channels
                        iEntryChannel.save("tiktok", adminUser.getUsername());
                        iEntryChannel.save("whatsapp", adminUser.getUsername());
                        iEntryChannel.save("instagram", adminUser.getUsername());
                        iEntryChannel.save("facebook", adminUser.getUsername());
                        iEntryChannel.save("twitter", adminUser.getUsername());
                        iEntryChannel.save("web", adminUser.getUsername());
                        iEntryChannel.save("otro", adminUser.getUsername());

                        // mock store types
                        iStoreType.save("shopify", adminUser.getUsername());
                        iStoreType.save("woocommerce", adminUser.getUsername());
                        iStoreType.save("tiendada", adminUser.getUsername());
                        iStoreType.save("ninguna", adminUser.getUsername());
                        iStoreType.save("otro", adminUser.getUsername());
                        iStoreType.save("wix", adminUser.getUsername());
                        iStoreType.save("prestashop", adminUser.getUsername());

                        // mock color
                        iColor.save("BLANCO","BLA", adminUser.getUsername());
                        iColor.save("NEGRO","NEG", adminUser.getUsername());
                        iColor.save("HUESO","HUE", adminUser.getUsername());
                        iColor.save("PERLA","PER", adminUser.getUsername());
                        iColor.save("BEIGE","BEI", adminUser.getUsername());
                        iColor.save("CAMEL","CAM", adminUser.getUsername());
                        iColor.save("VERDE","VER", adminUser.getUsername());
                        iColor.save("VINO","VIN", adminUser.getUsername());
                        iColor.save("ROJO","ROJ", adminUser.getUsername());
                        iColor.save("NUDE", "NUD",adminUser.getUsername());
                        iColor.save("GRIS","GRI", adminUser.getUsername());
                        iColor.save("MORADO","MOR", adminUser.getUsername());
                        iColor.save("AZUL", "AZU",adminUser.getUsername());
                        iColor.save("AMARILLO","AMA", adminUser.getUsername());
                        iColor.save("NARANJA","NAR", adminUser.getUsername());
                        iColor.save("ACERO", "ACE",adminUser.getUsername());
                        iColor.save("JADE", "JAD",adminUser.getUsername());
                        iColor.save("COMBINADO", "COM",adminUser.getUsername());

                        // mock size type
                        iSizeType.save("ROPA", adminUser.getUsername());
                        iSizeType.save("CALZADO", adminUser.getUsername());
                        iSizeType.save("COMPLEMENTOS", adminUser.getUsername());
                        iSizeType.save("ACCESORIOS", adminUser.getUsername());

                        // mock category products
                        iCategoryProduct.save("ROPA","RP001","ROPA",adminUser.getUsername());
                        iCategoryProduct.save("CALZADO","CA001","CALZADO",adminUser.getUsername());
                        iCategoryProduct.save("COMPLEMENTOS","CO001","COMPLEMENTOS",adminUser.getUsername());
                        iCategoryProduct.save("ACCESORIOS","AC001","ACCESORIOS",adminUser.getUsername());

                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                        .categoryName("ROPA")
                                        .sku("BLS")
                                        .name("BLUSA")
                                        .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("ROPA")
                                .sku("CS")
                                .name("CAMISA")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("ROPA")
                                .sku("VT")
                                .name("VESTIDO")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("ROPA")
                                .sku("SC")
                                .name("SACO")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("ROPA")
                                .sku("SH")
                                .name("SHORT")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("ROPA")
                                .sku("PST")
                                .name("PANTALON_SASTRE")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("ROPA")
                                .sku("BLZ")
                                .name("BLAZER")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("ROPA")
                                .sku("CHA")
                                .name("CHALECO")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("ROPA")
                                .sku("POL")
                                .name("POLO")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("ROPA")
                                .sku("VTS")
                                .name("VESTIDO_SHORT")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("ROPA")
                                .sku("FAL")
                                .name("FALDA")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("ROPA")
                                .sku("HOO")
                                .name("HOODIES")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("ROPA")
                                .sku("CHO")
                                .name("CHOMPAS")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("ROPA")
                                .sku("PTD")
                                .name("PANTALON_DENIM")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("ROPA")
                                .sku("PA")
                                .name("PANTALONES")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("ROPA")
                                .sku("TO")
                                .name("TOPS")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("ROPA")
                                .sku("EN")
                                .name("ENTERIZOS")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("COMPLEMENTOS")
                                .sku("BOL")
                                .name("BOLSOS")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("COMPLEMENTOS")
                                .sku("CAR")
                                .name("CARTERAS")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("COMPLEMENTOS")
                                .sku("GOR")
                                .name("GORRAS")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("COMPLEMENTOS")
                                .sku("MOCH")
                                .name("MOCHILAS")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("COMPLEMENTOS")
                                .sku("BIL")
                                .name("BILLETERAS")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("CALZADO")
                                .sku("BOA")
                                .name("BOTIN_ALTO")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("CALZADO")
                                .sku("BOB")
                                .name("BOTIN_BAJO")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("CALZADO")
                                .sku("ZA")
                                .name("ZAPATILLAS")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("CALZADO")
                                .sku("OXF")
                                .name("OXFORD")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("CALZADO")
                                .sku("BAL")
                                .name("BALERINAS")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("CALZADO")
                                .sku("SAP")
                                .name("SANDALIAS_PLANAS")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("CALZADO")
                                .sku("PLA")
                                .name("PLATAFORMAS")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("CALZADO")
                                .sku("SA")
                                .name("SANDALIAS")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("CALZADO")
                                .sku("SAT")
                                .name("SANDALIAS_CON_TACO")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("CALZADO")
                                .sku("CHU")
                                .name("CHUNKIS")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("CALZADO")
                                .sku("ALP")
                                .name("ALPAGARTAS")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("CALZADO")
                                .sku("MUL")
                                .name("MULES")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("CALZADO")
                                .sku("BAB")
                                .name("BABUCHA")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("CALZADO")
                                .sku("BOT")
                                .name("BOTAS")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("CALZADO")
                                .sku("STI")
                                .name("STILETTO")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("CALZADO")
                                .sku("SUE")
                                .name("SUECOS")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("CALZADO")
                                .sku("MOC")
                                .name("MOCASIN")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("CALZADO")
                                .sku("ZAU")
                                .name("ZAPATILLAS_URBANAS")
                                .tokenUser(adminUser.getUsername())
                                .build());
                        iSubCategoryProduct.save(RequestSubCategoryProduct.builder()
                                .categoryName("ACCESORIOS")
                                .sku("ACC")
                                .name("ACCESORIOS")
                                .tokenUser(adminUser.getUsername())
                                .build());


                        // mock size
                        iSize.save("XS", "ropa", adminUser.getUsername());
                        iSize.save("S", "ropa", adminUser.getUsername());
                        iSize.save("L", "ropa", adminUser.getUsername());
                        iSize.save("M", "ropa", adminUser.getUsername());
                        iSize.save("XL", "ropa", adminUser.getUsername());
                        iSize.save("XXL", "ropa", adminUser.getUsername());

                        iSize.save("35", "calzado", adminUser.getUsername());
                        iSize.save("36", "calzado", adminUser.getUsername());
                        iSize.save("37", "calzado", adminUser.getUsername());
                        iSize.save("38","calzado", adminUser.getUsername());
                        iSize.save("39",  "calzado", adminUser.getUsername());
                        iSize.save("40", "calzado", adminUser.getUsername());

                        iSize.save("accesorio", "accesorios", adminUser.getUsername());
                        iSize.save("complemento","complementos",adminUser.getUsername());

                        // order state
                        iOrderState.save("pendiente","#f2433d",adminUser.getUsername());
                        iOrderState.save("entregado", "#52c41a",adminUser.getUsername());
                        iOrderState.save("preparado","#00FF00", adminUser.getUsername());
                        iOrderState.save("pendiente de stock","#faad14",adminUser.getUsername());
                        iOrderState.save("pagado","#FFA500",adminUser.getUsername());
                        iOrderState.save("reservado","#2f54eb", adminUser.getUsername());
                        iOrderState.save("fallido","#f5222d",adminUser.getUsername());
                        iOrderState.save("por recoger","#1890ff",adminUser.getUsername());
                        iOrderState.save("no hay stock","#d9d9d9",adminUser.getUsername());
                        iOrderState.save("llamar","#722ed1",adminUser.getUsername());
                        iOrderState.save("devolucion","#ad8b00", adminUser.getUsername());
                        iOrderState.save("agendado","#13c2c2",adminUser.getUsername());
                        iOrderState.save("en ruta","#004d80",adminUser.getUsername());
                        iOrderState.save("llamado","#008080",adminUser.getUsername());
                        iOrderState.save("cancelado","#f5222d",adminUser.getUsername());

                        // payment state
                        iOrderPaymentState.save("por recaudar",adminUser.getUsername());
                        iOrderPaymentState.save("recaudado",adminUser.getUsername());
                        iOrderPaymentState.save("perdida",adminUser.getUsername());

                        // sale channel
                        iSaleChannel.save("MP FALLABELA",adminUser.getUsername());
                        iSaleChannel.save("IG ARANNI",adminUser.getUsername());
                        iSaleChannel.save("IG KUNCA",adminUser.getUsername());
                        iSaleChannel.save("TIENDA OP",adminUser.getUsername());
                        iSaleChannel.save("VENTA B2B",adminUser.getUsername());
                        iSaleChannel.save("WEB KUNCA.PE",adminUser.getUsername());
                        iSaleChannel.save("MP RIPLEY",adminUser.getUsername());
                        iSaleChannel.save("WEB KUNCA.SHOP",adminUser.getUsername());
                        iSaleChannel.save("MERCADO LIBRE",adminUser.getUsername());
                        iSaleChannel.save("MP YAPE",adminUser.getUsername());
                        iSaleChannel.save("MP PLATANITOS",adminUser.getUsername());
                        iSaleChannel.save("WEB ARANNI.PE",adminUser.getUsername());
                        iSaleChannel.save("LIVE SHOPPING ARANNI",adminUser.getUsername());
                        iSaleChannel.save("LIVE SHOPPING KUNCA",adminUser.getUsername());

                        // management type
                        iManagementType.save("canje",adminUser.getUsername());
                        iManagementType.save("venta", adminUser.getUsername());
                        iManagementType.save("reserva",adminUser.getUsername());
                        iManagementType.save("cambio",adminUser.getUsername());
                        iManagementType.save("preventa",adminUser.getUsername());
                        iManagementType.save("recupero",adminUser.getUsername());

                        // payment type
                        iOrderPaymentMethod.save("yape",adminUser.getUsername());
                        iOrderPaymentMethod.save("pos",adminUser.getUsername());
                        iOrderPaymentMethod.save("efectivo",adminUser.getUsername());
                        iOrderPaymentMethod.save("link",adminUser.getUsername());
                        iOrderPaymentMethod.save("cambio",adminUser.getUsername());
                        iOrderPaymentMethod.save("plin",adminUser.getUsername());
                        iOrderPaymentMethod.save("plataforma mp/web",adminUser.getUsername());
                        iOrderPaymentMethod.save("bcp",adminUser.getUsername());
                        iOrderPaymentMethod.save("contraentrega",adminUser.getUsername());
                        iOrderPaymentMethod.save("canje",adminUser.getUsername());
                        iOrderPaymentMethod.save("interbank",adminUser.getUsername());
                        iOrderPaymentMethod.save("banco de la nacion",adminUser.getUsername());

                        //unit type
                        iUnitType.save("ropa",adminUser.getUsername());
                        iUnitType.save("calzado",adminUser.getUsername());
                        iUnitType.save("ACCESORIOS",adminUser.getUsername());
                        iUnitType.save("COMPLEMENTOS",adminUser.getUsername());

                        // unit
                        RequestUnit requestUnit1 = RequestUnit.builder()
                                .name("UND")
                                .unitType("ROPA")
                                .build();

                        iUnit.save(requestUnit1,adminUser.getUsername());

                        RequestUnit requestUnit2 = RequestUnit.builder()
                                .name("PAR")
                                .unitType("CALZADO")
                                .build();

                        iUnit.save(requestUnit2,adminUser.getUsername());

                        RequestUnit requestUnit3 = RequestUnit.builder()
                                .name("UND")
                                        .unitType("ACCESORIOS")
                                                .build();

                        iUnit.save(requestUnit3,adminUser.getUsername());

                        RequestUnit requestUnit4 = RequestUnit.builder()
                                .name("UND")
                                .unitType("COMPLEMENTOS")
                                .build();

                        iUnit.save(requestUnit4,adminUser.getUsername());

                        // purchase type
                        iPurchaseType.save("compra",adminUser.getUsername());
                        iPurchaseType.save("devolucion",adminUser.getUsername());
                        iPurchaseType.save("restockaje",adminUser.getUsername());

                        // order return type
                        iOrderReturnType.save("rechazo",adminUser.getUsername());
                        iOrderReturnType.save("cambio",adminUser.getUsername());

                        // cancellation reason
                        iCancellationReason.save("No hay stock",adminUser.getUsername());
                        iCancellationReason.save("Demora en entrega",adminUser.getUsername());
                        iCancellationReason.save("Mala calidad",adminUser.getUsername());
                        iCancellationReason.save("Se le daño el producto - 30 dias",adminUser.getUsername());
                        iCancellationReason.save("Otros motivos",adminUser.getUsername());
                        iCancellationReason.save("Muy caro el envio",adminUser.getUsername());
                        iCancellationReason.save("Zona peligrosa",adminUser.getUsername());
                        iCancellationReason.save("Cliente no confiable para contraentrega",adminUser.getUsername());
                        iCancellationReason.save("Robo por motorizado",adminUser.getUsername());
                        iCancellationReason.save("No le gusto producto",adminUser.getUsername());

                        // payment gateway
                        iPaymentGateway.save("mercado pago",adminUser.getUsername());
                        iPaymentGateway.save("demo",adminUser.getUsername());

                        // membership states
                        iMembershipState.save("activa",adminUser.getUsername());
                        iMembershipState.save("pagada",adminUser.getUsername());
                        iMembershipState.save("expirada",adminUser.getUsername());
                        // purchase documents
                        iPurchaseDocument.save("factura",adminUser.getUsername());
                        iPurchaseDocument.save("boleta",adminUser.getUsername());
                        iPurchaseDocument.save("recibo",adminUser.getUsername());
                        iPurchaseDocument.save("sin comprobante",adminUser.getUsername());
                        // customer types
                        iCustomerType.save("tradicional",adminUser.getUsername());
                        iCustomerType.save("mayorista",adminUser.getUsername());
                        // discounts
                        iDiscount.save("monto",adminUser.getUsername());
                        iDiscount.save("porcentaje",adminUser.getUsername());
                        iDiscount.save("no aplica",adminUser.getUsername());
                        // delivery points
                        iDeliveryPoint.save("lima",adminUser.getUsername());
                        iDeliveryPoint.save("punto scharf",adminUser.getUsername());
                        iDeliveryPoint.save("provincia",adminUser.getUsername());
                        iDeliveryPoint.save("recojo en tienda",adminUser.getUsername());
                        // stock transaction types mocks
                        iStockTransactionType.save("compra", adminUser.getUsername());
                        iStockTransactionType.save("pedido", adminUser.getUsername());
                        iStockTransactionType.save("transferencia-entrada", adminUser.getUsername());
                        iStockTransactionType.save("transferencia-salida", adminUser.getUsername());
                        iStockTransactionType.save("devolucion-comprador",adminUser.getUsername());
                        iStockTransactionType.save("devolucion-proveedor",adminUser.getUsername());

                        iPurchasePaymentType.save("credito",adminUser.getUsername());
                        iPurchasePaymentType.save("efectivo",adminUser.getUsername());
                        iPurchasePaymentType.save("transferencia",adminUser.getUsername());

                        iDeliveryStatus.save("PENDIENTE",adminUser.getUsername());
                        iDeliveryStatus.save("COMPLETO",adminUser.getUsername());

                        iDeliveryManifestStatus.save("ABIERTA",adminUser.getUsername());
                        iDeliveryManifestStatus.save("CERRADA",adminUser.getUsername());

                        User business1 = userRepository.save(User.builder()
                                .username("JCOILA")
                                .name("JUAN")
                                .surname("ROMERO")
                                .dni("00000000")
                                .email("jca@gmail.com")
                                .address("cr 12 h 34")
                                .gender("M")
                                .mobile("00000000")
                                        .district(districtB)
                                        .districtId(districtB.getId())
                                        .client(client1)
                                        .clientId(client1.getId())
                                .password(passwordEncoder.encode("n>53F-8W5L7Dw+"))
                                .status(true)
                                .registrationDate(OffsetDateTime.now())
                                .updateDate(OffsetDateTime.now())
                                .build());

                        iUserRole.save(business1.getUsername(),"NEGOCIO", adminUser.getUsername());

                        // mock stores
                        RequestStoreSave requestStoreSave1 = RequestStoreSave.builder()
                                .url("https://shopify.com")
                                .storeType("SHOPIFY")
                                .name("SHOPIFY")
                                .build();

                        iStore.save(requestStoreSave1,business1.getUsername());

                        RequestCourier requestCourier = RequestCourier.builder()
                                .courier("SIN COURIER")
                                .address("calle 0")
                                .plate("000-000")
                                .phone("000000000")
                                .build();

                        iCourier.save(requestCourier,adminUser.getUsername());
                        iSize.save("STD","ROPA","JROMERO");
                        iSize.save("PSZ","ROPA","JROMERO");
                        RequestStoreSave requestStore1 = RequestStoreSave.builder()
                                .name("aranni")
                                .url("https://www.aranni.com.pe")
                                .storeType("otro")
                                .build();
                          iStore.save(requestStore1,"JCOILA");

                        RequestStoreSave requestStore2 = RequestStoreSave.builder()
                                .name("kunca")
                                .url("https://kunca.pe")
                                .storeType("otro")
                                .build();
                        iStore.save(requestStore2,"JCOILA");
                        iSaleChannel.save("WHATSAPP","JROMERO");

                        iDistrict.save("BELLAVISTA","JROMERO","CALLAO");
                        iDistrict.save("COMAS","JROMERO","LIMA");
                        iDistrict.save("INDEPENDENCIA","JROMERO","LIMA");
                        iDistrict.save("LA VICTORIA","JROMERO","LIMA");
                        iDistrict.save("MIRAFLORES","JROMERO","LIMA");
                        iDistrict.save("PUEBLO LIBRE","JROMERO","LIMA");
                        iDistrict.save("SAN ISIDRO","JROMERO","LIMA");
                        iDistrict.save("SAN LUIS","JROMERO","LIMA");
                        iDistrict.save("SAN MIGUEL","JROMERO","LIMA");
                        iDistrict.save("SANTA ROSA","JROMERO","LIMA");
                        iColor.save("FUCSIA","FUC","JROMERO");
                        iColor.save("MARRON","MAR","JROMERO");
                        iColor.save("DORADO","DOR","JROMERO");
                        iColor.save("ROSADO","ROS","JROMERO");
                        iColor.save("PLATA QUEMADA","PLAQ","JROMERO");
                        iColor.save("LACRE","LAC","JROMERO");
                        iAuditEvent.save("ACTIVATE_ORDER_ITEM","JROMERO");
                        iColor.save("CELESTE","CEL","JROMERO");
                        iColor.save("LILA","LIL","JROMERO");
                        iColor.save("PLATA","PLA","JROMERO");
                        iColor.save("PRINT","PRI","JROMERO");
                        iColor.save("MELON","MEL","JROMERO");
                        iColor.save("CHOCOLATE","CHO","JROMERO");
                        iColor.save("MOSTAZA","MOS","JROMERO");
                        iDistrict.save("SALAMANCA","JROMERO","LIMA");
                        iColor.save("VERDE LORO","VERL","JROMERO");
                        iColor.save("VERDE PERA","VERP","JROMERO");
                        iColor.save("AZUL ELECTRICO","AZUE","JROMERO");
                }catch (RuntimeException e){
                        e.printStackTrace();
                        throw new RuntimeException(e.getMessage());
                }
        }

}
