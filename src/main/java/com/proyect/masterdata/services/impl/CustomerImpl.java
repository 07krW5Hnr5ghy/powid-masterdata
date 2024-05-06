package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.request.RequestCustomer;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.ICustomer;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class CustomerImpl implements ICustomer {

    private final UserRepository userRepository;
    private final CustomerRepository customerRepository;
    private final DepartmentRepository departmentRepository;
    private final ProvinceRepository provinceRepository;
    private final DistrictRepository districtRepository;
    private final CustomerTypeRepository customerTypeRepository;
    @Override
    public CompletableFuture<ResponseSuccess> save(Ordering ordering, RequestCustomer requestCustomer, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Department department;
            Province province;
            District district;
            CustomerType customerType;

            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                department = departmentRepository.findByNameAndStatusTrue(requestCustomer.getDepartment().toUpperCase());
                province = provinceRepository.findByNameAndStatusTrue(requestCustomer.getProvince().toUpperCase());
                district = districtRepository.findByNameAndStatusTrue(requestCustomer.getDistrict().toUpperCase());
                customerType = customerTypeRepository.findByNameAndStatusTrue(requestCustomer.getType().toUpperCase());
            } catch (RuntimeException e){
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(department == null){
                throw new BadRequestExceptions(Constants.ErrorDepartment);
            }

            if(province == null){
                throw new BadRequestExceptions(Constants.ErrorProvince);
            }

            if(district == null){
                throw new BadRequestExceptions(Constants.ErrorDistrict);
            }

            if(customerType == null){
                throw new BadRequestExceptions(Constants.ErrorCustomerType);
            }

            try{
                customerRepository.save(Customer.builder()
                        .address(requestCustomer.getAddress().toUpperCase())
                        .name(requestCustomer.getName().toUpperCase())
                        .phone(requestCustomer.getPhone())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .ordering(ordering)
                        .orderId(ordering.getId())
                        .customerType(customerType)
                        .customerTypeId(customerType.getId())
                        .department(department)
                        .departmentId(department.getId())
                        .province(province)
                        .provinceId(province.getId())
                        .district(district)
                        .districtId(district.getId())
                        .instagram(requestCustomer.getInstagram())
                        .reference(requestCustomer.getReference().toUpperCase())
                        .phone(requestCustomer.getPhone())
                        .build());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            }catch (RuntimeException e){
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }
}
