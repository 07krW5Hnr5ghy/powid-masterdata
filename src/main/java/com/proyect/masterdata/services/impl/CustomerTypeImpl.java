package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.CustomerType;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.CustomerTypeRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.ICustomerType;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
@Log4j2
public class CustomerTypeImpl implements ICustomerType {
    private final UserRepository userRepository;
    public final CustomerTypeRepository customerTypeRepository;
    @Override
    public ResponseSuccess save(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        User user;
        CustomerType customerType;
        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            customerType = customerTypeRepository.findByNameAndStatusTrue(name.toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }
        if(customerType != null){
            throw new BadRequestExceptions(Constants.ErrorCustomerType);
        }
        try{
            customerTypeRepository.save(CustomerType.builder()
                            .name(name.toUpperCase())
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .updateDate(new Date(System.currentTimeMillis()))
                            .status(true)
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
    public List<String> list() throws BadRequestExceptions, InternalErrorExceptions {
        List<CustomerType> customerTypes;
        try {
            customerTypes = customerTypeRepository.findAllByStatusTrue();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(customerTypes.isEmpty()){
            return Collections.emptyList();
        }
        return customerTypes.stream().map(CustomerType::getName).toList();
    }
}
