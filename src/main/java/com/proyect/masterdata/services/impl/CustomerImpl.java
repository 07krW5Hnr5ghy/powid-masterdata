package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.CustomerDTO;
import com.proyect.masterdata.dto.request.RequestCustomer;
import com.proyect.masterdata.dto.response.ResponseExistCustomer;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.ICustomer;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class CustomerImpl implements ICustomer {
    private final CustomerRepository customerRepository;
    private final DistrictRepository districtRepository;
    private final ProvinceRepository provinceRepository;
    private final UserRepository userRepository;
    private final CustomerTypeRepository customerTypeRepository;
    private final IAudit iAudit;
    @Override
    public ResponseSuccess save(RequestCustomer requestCustomer) throws BadRequestExceptions, InternalErrorExceptions {
        District district;
        Province province;
        User user;
        Customer customer;
        CustomerType customerType;
        try{
            province = provinceRepository.findByNameAndStatusTrue(requestCustomer.getProvince().toUpperCase());
            user = userRepository.findByUsernameAndStatusTrue(requestCustomer.getTokenUser());
            customer = customerRepository.findByPhone(requestCustomer.getPhone().toUpperCase());
            customerType = customerTypeRepository.findByNameAndStatusTrue(requestCustomer.getCustomerType().toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }
        if(province==null){
            throw new BadRequestExceptions(Constants.ErrorProvince);
        }else{
            district = districtRepository.findByNameAndProvinceIdAndStatusTrue(requestCustomer.getDistrict().toUpperCase(), province.getId());
        }
        if(district == null){
            throw new BadRequestExceptions(Constants.ErrorDistrict);
        }
        if(customerType==null){
            throw new BadRequestExceptions(Constants.ErrorCustomerType);
        }
        if(customer!=null){
            throw new BadRequestExceptions(Constants.ErrorCustomerExist);
        }
        try {
            Customer newCustomer = customerRepository.save(Customer.builder()
                    .name(requestCustomer.getName())
                    .phone(requestCustomer.getPhone())
                    .address(requestCustomer.getAddress())
                    .instagram(requestCustomer.getInstagram())
                    .reference(requestCustomer.getReference())
                    .registrationDate(OffsetDateTime.now())
                    .updateDate(OffsetDateTime.now())
                    .district(district)
                    .districtId(district.getId())
                    .clientId(user.getClientId())
                    .status(true)
                    .user(user)
                    .userId(user.getId())
                    .customerTypeId(customerType.getId())
                    .customerType(customerType)
                    .dni(requestCustomer.getDni())
                    .build());
            iAudit.save("ADD_CUSTOMER","COMPRADOR "+newCustomer.getName()+"/"+newCustomer.getPhone()+" CREADO.",newCustomer.getPhone(),user.getUsername());
            return ResponseSuccess.builder()
                    .message(Constants.register)
                    .code(200)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public CompletableFuture<ResponseSuccess> saveAsync(RequestCustomer requestCustomer) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            District district;
            Province province;
            User user;
            Customer customer;
            CustomerType customerType;
            try{
                province = provinceRepository.findByNameAndStatusTrue(requestCustomer.getProvince().toUpperCase());
                user = userRepository.findByUsernameAndStatusTrue(requestCustomer.getTokenUser());
                customer = customerRepository.findByPhone(requestCustomer.getPhone().toUpperCase());
                customerType = customerTypeRepository.findByNameAndStatusTrue(requestCustomer.getCustomerType().toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(province==null){
                throw new BadRequestExceptions(Constants.ErrorProvince);
            }else{
                district = districtRepository.findByNameAndProvinceIdAndStatusTrue(requestCustomer.getDistrict().toUpperCase(), province.getId());
            }
            if(district == null){
                throw new BadRequestExceptions(Constants.ErrorDistrict);
            }
            if(customerType==null){
                throw new BadRequestExceptions(Constants.ErrorCustomerType);
            }
            if(customer!=null){
                throw new BadRequestExceptions(Constants.ErrorCustomerExist);
            }
            try {
                Customer newCustomer = customerRepository.save(Customer.builder()
                        .name(requestCustomer.getName())
                        .phone(requestCustomer.getPhone())
                        .address(requestCustomer.getAddress())
                        .instagram(requestCustomer.getInstagram())
                        .reference(requestCustomer.getReference())
                        .registrationDate(OffsetDateTime.now())
                        .updateDate(OffsetDateTime.now())
                        .district(district)
                        .districtId(district.getId())
                        .clientId(user.getClientId())
                        .status(true)
                        .customerType(customerType)
                        .customerTypeId(customerType.getId())
                        .user(user)
                        .dni(requestCustomer.getDni())
                        .userId(user.getId())
                        .build());
                iAudit.save("ADD_CUSTOMER","COMPRADOR "+newCustomer.getName()+"/"+newCustomer.getPhone()+" CREADO.",newCustomer.getPhone(),user.getUsername());
                return ResponseSuccess.builder()
                        .message(Constants.register)
                        .code(200)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<List<CustomerDTO>> listFilter(String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Customer> customerList;
            User user;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                customerList = customerRepository.findAllByClientId(user.getClientId());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(customerList.isEmpty()){
                return new ArrayList<>();
            }
            return customerList.stream().map(customer -> CustomerDTO.builder()
                    .status(customer.getStatus())
                    .id(customer.getId())
                    .user(customer.getUser().getUsername())
                    .name(customer.getName())
                    .phone(customer.getPhone())
                    .customerType(customer.getCustomerType().getName())
                    .department(customer.getDistrict().getProvince().getDepartment().getName())
                    .province(customer.getDistrict().getProvince().getName())
                    .district(customer.getDistrict().getName())
                    .address(customer.getAddress())
                    .instagram(customer.getInstagram())
                    .dni(customer.getDni())
                    .build()).toList();
        });
    }


    public ResponseExistCustomer existsCustomer(String phone, String tokenUser ) throws BadRequestExceptions, InterruptedException {
        Customer customer;
        User user;
        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            customer = customerRepository.findByPhone(phone);
        } catch (RuntimeException e) {
            throw new RuntimeException(e);
        }

        if(user==null){
            throw new BadRequestExceptions(Constants.ErrorUserExist);
        }

        if(customer==null){
            return ResponseExistCustomer.builder()
                    .exist(false)
                    .build();
        }

        return ResponseExistCustomer.builder()
                .exist(true)
                .build();
    }
}
