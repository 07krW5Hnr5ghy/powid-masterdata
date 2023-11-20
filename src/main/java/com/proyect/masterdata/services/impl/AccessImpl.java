package com.proyect.masterdata.services.impl;

import java.util.Date;

import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.Access;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.AccessRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAccess;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class AccessImpl implements IAccess {

    private final UserRepository userRepository;
    private final AccessRepository accessRepository;

    @Override
    public ResponseSuccess save(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {

        boolean existsTokenUser;
        boolean existsAccess;

        try {
            existsTokenUser = userRepository.existsByUsername(tokenUser.toUpperCase());
            existsAccess = accessRepository.existsByName(name.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (!existsTokenUser) {
            throw new BadRequestExceptions("Usuario no existe");
        }

        if (existsAccess) {
            throw new BadRequestExceptions("Acceso no existe");
        }

        try {
            accessRepository.save(Access.builder()
                    .name(name.toUpperCase())
                    .status(true)
                    .dateRegistration(new Date(System.currentTimeMillis()))
                    .tokenUser(tokenUser.toUpperCase())
                    .build());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

}
