package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Access;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.AccessDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.AccessRepository;
import com.proyect.masterdata.repository.AccessRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAccess;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class AccessImpl implements IAccess {

    private final UserRepository userRepository;
    private final AccessRepository accessRepository;
    private final AccessRepositoryCustom accessRepositoryCustom;
    private final IAudit iAudit;
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
            Access newAccess = accessRepository.save(Access.builder()
                    .name(name.toUpperCase())
                    .status(true)
                    .registrationDate(new Date(System.currentTimeMillis()))
                    .tokenUser(tokenUser.toUpperCase())
                    .build());
            iAudit.save("ADD_ACCESS","ADD ACCESS " + newAccess.getName() + " .",user.getUsername());
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
                Access newAccess = accessRepository.save(Access.builder()
                        .name(name.toUpperCase())
                        .status(true)
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .tokenUser(tokenUser.toUpperCase())
                        .build());
                iAudit.save("ADD_ACCESS","ADD ACCESS " + newAccess.getName() + " .",user.getUsername());
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
            access.setUpdateDate(new Date(System.currentTimeMillis()));
            access.setTokenUser(tokenUser.toUpperCase());
            accessRepository.save(access);
            iAudit.save("DELETE_ACCESS","DELETE ACCESS "+access.getName()+" .",user.getUsername());
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
    public CompletableFuture<ResponseDelete> deleteAsync(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(() -> {
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
                access.setUpdateDate(new Date(System.currentTimeMillis()));
                access.setTokenUser(tokenUser.toUpperCase());
                accessRepository.save(access);
                iAudit.save("DELETE_ACCESS","DELETE ACCESS "+access.getName()+" .",user.getUsername());
                return ResponseDelete.builder()
                        .message(Constants.delete)
                        .code(200)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<List<AccessDTO>> list() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Access> accessList;
            try {
                accessList = accessRepository.findAllByStatusTrue();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if(accessList.isEmpty()){
                return Collections.emptyList();
            }
            return accessList.stream().map(access -> AccessDTO.builder()
                    .name(access.getName())
                    .build()).toList();
        });
    }

    @Override
    public CompletableFuture<Page<AccessDTO>> listPagination(String name,Date registrationStartDate,Date registrationEndDate,Date updateStartDate,Date updateEndDate, String sort, String sortColumn, Integer pageNumber,
                                Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(() -> {
            Page<Access> accessPage;

            try {
                accessPage = accessRepositoryCustom.searchForAccess(name,registrationStartDate,registrationEndDate,updateStartDate,updateEndDate,sort, sortColumn, pageNumber, pageSize, true);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (accessPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<AccessDTO> accessDTOs = accessPage.getContent().stream().map(access -> AccessDTO.builder()
                    .name(access.getName())
                    .registrationDate(access.getRegistrationDate())
                    .updateDate(access.getUpdateDate())
                    .build()).toList();

            return new PageImpl<>(accessDTOs, accessPage.getPageable(),
                    accessPage.getTotalElements());
        });

    }

    @Override
    public CompletableFuture<Page<AccessDTO>> listFalse(String name,Date registrationStartDate,Date registrationEndDate,Date updateStartDate,Date updateEndDate, String sort, String sortColumn, Integer pageNumber,
                                                        Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(() -> {
            Page<Access> accessPage;

            try {
                accessPage = accessRepositoryCustom.searchForAccess(name,registrationStartDate,registrationEndDate,updateStartDate,updateEndDate,sort, sortColumn, pageNumber, pageSize, false);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (accessPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<AccessDTO> accessDTOs = accessPage.getContent().stream().map(access -> AccessDTO.builder()
                    .name(access.getName())
                    .registrationDate(access.getRegistrationDate())
                    .updateDate(access.getUpdateDate())
                    .build()).toList();

            return new PageImpl<>(accessDTOs, accessPage.getPageable(),
                    accessPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(() -> {
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
                access.setUpdateDate(new Date(System.currentTimeMillis()));
                access.setTokenUser(tokenUser.toUpperCase());
                accessRepository.save(access);
                iAudit.save("ACTIVATE_ACCESS","ACTIVATE ACCESS " +  access.getName() + " .",user.getUsername());
                return ResponseSuccess.builder()
                        .message(Constants.update)
                        .code(200)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

}
