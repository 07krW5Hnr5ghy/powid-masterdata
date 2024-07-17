package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Customer;
import com.proyect.masterdata.domain.CustomerType;
import com.proyect.masterdata.domain.District;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.CustomerDTO;
import com.proyect.masterdata.dto.request.RequestCustomer;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.CustomerRepository;
import com.proyect.masterdata.repository.CustomerTypeRepository;
import com.proyect.masterdata.repository.DistrictRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.ICustomer;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class CustomerImp implements ICustomer {
    private final CustomerRepository customerRepository;
    private final DistrictRepository districtRepository;
    private final UserRepository userRepository;
    private final CustomerTypeRepository customerTypeRepository;
    @Override
    public ResponseSuccess save(RequestCustomer requestCustomer) throws BadRequestExceptions, InternalErrorExceptions {
        District district;
        User user;
        Customer customer;
        CustomerType customerType;
        try{
            district = districtRepository.findByNameAndStatusTrue(requestCustomer.getDistrict().toUpperCase());
            user = userRepository.findByUsernameAndStatusTrue(requestCustomer.getTokenUser());
            customer = customerRepository.findByPhone(requestCustomer.getPhone().toUpperCase());
            customerType = customerTypeRepository.findByNameAndStatusTrue(requestCustomer.getCustomerType().toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(district == null){
            throw new BadRequestExceptions(Constants.ErrorDistrict);
        }
        if(customerType==null){
            throw new BadRequestExceptions(Constants.ErrorCustomerType);
        }
        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }
        if(customer!=null){
            throw new BadRequestExceptions(Constants.ErrorCustomerExist);
        }
        try {
            customerRepository.save(Customer.builder()
                    .name(requestCustomer.getName())
                    .phone(requestCustomer.getPhone())
                    .address(requestCustomer.getAddress())
                    .instagram(requestCustomer.getInstagram())
                    .reference(requestCustomer.getReference())
                    .registrationDate(new Date(System.currentTimeMillis()))
                    .updateDate(new Date(System.currentTimeMillis()))
                    .district(district)
                    .districtId(district.getId())
                    .clientId(user.getClientId())
                    .tokenUser(user.getUsername())
                    .customerTypeId(customerType.getId())
                    .customerType(customerType)
                    .dni(requestCustomer.getDni())
                    .build());
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
            User user;
            Customer customer;
            CustomerType customerType;
            try{
                district = districtRepository.findByNameAndStatusTrue(requestCustomer.getDistrict().toUpperCase());
                user = userRepository.findByUsernameAndStatusTrue(requestCustomer.getTokenUser());
                customer = customerRepository.findByPhone(requestCustomer.getPhone().toUpperCase());
                customerType = customerTypeRepository.findByNameAndStatusTrue(requestCustomer.getCustomerType().toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(district == null){
                throw new BadRequestExceptions(Constants.ErrorDistrict);
            }
            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(customerType==null){
                throw new BadRequestExceptions(Constants.ErrorCustomerType);
            }
            if(customer!=null){
                throw new BadRequestExceptions(Constants.ErrorCustomerExist);
            }
            try {
                customerRepository.save(Customer.builder()
                        .name(requestCustomer.getName())
                        .phone(requestCustomer.getPhone())
                        .address(requestCustomer.getAddress())
                        .instagram(requestCustomer.getInstagram())
                        .reference(requestCustomer.getReference())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .updateDate(new Date(System.currentTimeMillis()))
                        .district(district)
                        .districtId(district.getId())
                        .clientId(user.getClientId())
                                .customerType(customerType)
                                .customerTypeId(customerType.getId())
                        .tokenUser(user.getUsername())
                        .build());
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
                    .name(customer.getName())
                    .phone(customer.getPhone())
                    .customerType(customer.getCustomerType().getName())
                    .department(customer.getDistrict().getProvince().getDepartment().getName())
                    .province(customer.getDistrict().getProvince().getName())
                    .district(customer.getDistrict().getName())
                    .address(customer.getAddress())
                    .instagram(customer.getInstagram())
                    .build()).toList();
        });
    }
}
