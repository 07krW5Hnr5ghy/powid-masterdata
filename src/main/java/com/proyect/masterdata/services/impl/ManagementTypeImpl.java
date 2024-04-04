package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.ManagementType;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.ManagementTypeRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IManagementType;
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
public class ManagementTypeImpl implements IManagementType {
    private final UserRepository userRepository;
    private final ManagementTypeRepository managementTypeRepository;
    @Override
    public ResponseSuccess save(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        ManagementType managementType;

        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            managementType = managementTypeRepository.findByNameAndStatusTrue(name.toUpperCase());
        }catch (RuntimeException e){
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(managementType != null){
            throw new BadRequestExceptions(Constants.ErrorManagementTypeExists);
        }

        try{
            managementTypeRepository.save(ManagementType.builder()
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .status(true)
                            .name(name.toUpperCase())
                            .tokenUser(user.getUsername())
                    .build());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        }catch (RuntimeException e){
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public List<String> list() throws InternalErrorExceptions, BadRequestExceptions {
        List<ManagementType> managementTypes;
        try {
            managementTypes = managementTypeRepository.findAllByStatusTrue();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(managementTypes.isEmpty()){
            return Collections.emptyList();
        }
        return managementTypes.stream().map(ManagementType::getName).toList();
    }
}
