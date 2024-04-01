package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.SupplierType;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.SupplierTypeRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.ISupplierType;
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
public class SupplierTypeImpl implements ISupplierType {
    private final UserRepository userRepository;
    private final SupplierTypeRepository supplierTypeRepository;
    @Override
    public ResponseSuccess save(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        User user;
        SupplierType supplierType;
        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            supplierType = supplierTypeRepository.findByNameAndStatusTrue(name.toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }
        if(supplierType != null){
            throw new BadRequestExceptions(Constants.ErrorSupplierTypeExists);
        }
        try {
            supplierTypeRepository.save(SupplierType.builder()
                            .name(name.toUpperCase())
                            .status(true)
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .updateDate(new Date(System.currentTimeMillis()))
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
    public List<String> listSupplierType() throws BadRequestExceptions {
        List<SupplierType> supplierTypes;
        try {
            supplierTypes = supplierTypeRepository.findAllByStatusTrue();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(supplierTypes.isEmpty()){
            return Collections.emptyList();
        }
        return supplierTypes.stream().map(SupplierType::getName).toList();
    }
}
