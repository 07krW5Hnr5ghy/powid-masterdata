package com.proyect.masterdata.services.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.AccessDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import org.springframework.scheduling.annotation.Async;
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
        User user;
        Access access;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            access = accessRepository.findByName(name.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (access != null) {
            throw new BadRequestExceptions(Constants.ErrorAccessExists);
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

    @Override
    @Async
    public CompletableFuture<ResponseSuccess> saveAsync(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(() -> {
            User user;
            Access access;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                access = accessRepository.findByName(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (access != null) {
                throw new BadRequestExceptions(Constants.ErrorAccessExists);
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
        });
    }

    @Override
    public ResponseDelete delete(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        Access access;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            access = accessRepository.findByNameAndStatusTrue(name.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (access == null) {
            throw new BadRequestExceptions(Constants.ErrorAccess);
        }

        try {
            access.setStatus(false);
            access.setDateUpDate(new Date(System.currentTimeMillis()));
            access.setTokenUser(tokenUser.toUpperCase());
            accessRepository.save(access);
            return ResponseDelete.builder()
                    .message(Constants.delete)
                    .code(200)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public List<AccessDTO> list() throws BadRequestExceptions {
        List<Access> accesses = new ArrayList<>();

        try{
            accesses = accessRepository.findAllByStatusTrue();
        }catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if(accesses.isEmpty()){
            return Collections.emptyList();
        }

        return accesses.stream().map(access -> AccessDTO.builder()
                .name(access.getName())
                .build()).toList();
    }

    @Override
    public List<AccessDTO> listFalse() throws BadRequestExceptions {
        List<Access> accesses = new ArrayList<>();

        try{
            accesses = accessRepository.findAllByStatusFalse();
        }catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }

        if(accesses.isEmpty()){
            return Collections.emptyList();
        }

        return accesses.stream().map(access -> AccessDTO.builder()
                .name(access.getName())
                .build()).toList();
    }

    @Override
    public ResponseSuccess activate(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        Access access;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            access = accessRepository.findByNameAndStatusFalse(name.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (access == null) {
            throw new BadRequestExceptions(Constants.ErrorAccess);
        }

        try {
            access.setStatus(true);
            access.setDateUpDate(new Date(System.currentTimeMillis()));
            access.setTokenUser(tokenUser.toUpperCase());
            accessRepository.save(access);
            return ResponseSuccess.builder()
                    .message(Constants.update)
                    .code(200)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

}
