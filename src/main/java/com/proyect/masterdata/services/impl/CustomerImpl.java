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

@Service
@RequiredArgsConstructor
@Log4j2
public class CustomerImpl implements ICustomer {

    private final UserRepository userRepository;
    private final CustomerRepository customerRepository;
    private final DepartmentRepository departmentRepository;
    private final ProvinceRepository provinceRepository;
    private final DistrictRepository districtRepository;
    @Override
    public ResponseSuccess save(RequestCustomer requestCustomer, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        Department department;
        Province province;
        District district;

        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            department = departmentRepository.findByNameAndStatusTrue(requestCustomer.getDepartment().toUpperCase());
            province = provinceRepository.findByNameAndStatusTrue(requestCustomer.getProvince().toUpperCase());
            district = districtRepository.findByNameAndStatusTrue(requestCustomer.getDistrict().toUpperCase());
        } catch (RuntimeException e){
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

        try{
            customerRepository.save(Customer.builder()
                            .address(requestCustomer.getAddress().toUpperCase())
                            .name(requestCustomer.getName().toUpperCase())
                            .phone(requestCustomer.getPhone())
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .order(requestCustomer.getOrder())
                            .orderId(requestCustomer.getOrder().getId())
                            .type(requestCustomer.getType())
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
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }
}
