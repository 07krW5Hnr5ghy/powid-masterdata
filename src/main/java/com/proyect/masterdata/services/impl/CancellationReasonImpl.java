package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.CancellationReason;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.CancellationReasonDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.CancellationReasonRepository;
import com.proyect.masterdata.repository.CancellationReasonRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.ICancellationReason;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class CancellationReasonImpl implements ICancellationReason {
    private final CancellationReasonRepository cancellationReasonRepository;
    private final UserRepository userRepository;
    private final CancellationReasonRepositoryCustom cancellationReasonRepositoryCustom;
    private final IAudit iAudit;
    @Override
    public ResponseSuccess save(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        CancellationReason cancellationReason;

        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            cancellationReason = cancellationReasonRepository.findByName(name.toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(cancellationReason != null){
            throw new BadRequestExceptions(Constants.ErrorCancellationReasonExists);
        }

        try{

            CancellationReason newCancellationReason = cancellationReasonRepository.save(CancellationReason.builder()
                            .name(name.toUpperCase())
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .updateDate(new Date(System.currentTimeMillis()))
                            .status(true)
                            .tokenUser(tokenUser.toUpperCase())
                    .build());
            iAudit.save("ADD_CANCELLATION_REASON","RAZON DE CANCELACION "+newCancellationReason.getName()+" CREADA .",newCancellationReason.getName(),user.getUsername());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();

        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public CompletableFuture<ResponseSuccess> saveAsync(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            CancellationReason cancellationReason;

            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                cancellationReason = cancellationReasonRepository.findByName(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(cancellationReason != null){
                throw new BadRequestExceptions(Constants.ErrorCancellationReasonExists);
            }

            try{

                CancellationReason newCancellationReason = cancellationReasonRepository.save(CancellationReason.builder()
                        .name(name.toUpperCase())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .updateDate(new Date(System.currentTimeMillis()))
                        .status(true)
                        .tokenUser(tokenUser.toUpperCase())
                        .build());
                iAudit.save("ADD_CANCELLATION_REASON","RAZON DE CANCELACION "+newCancellationReason.getName()+" CREADA.",newCancellationReason.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();

            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<CancellationReasonDTO>> listPagination(String name,Date registrationStartDate, Date registrationEndDate, Date updateStartDate, Date updateEndDate, String sort, String sortColumn, Integer pageNumber,
                                                               Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(() -> {
            Page<CancellationReason> cancellationReasonPage;

            try {
                cancellationReasonPage = cancellationReasonRepositoryCustom.searchForCancellationReason(name,registrationStartDate,registrationEndDate,updateStartDate,updateStartDate, sort, sortColumn, pageNumber, pageSize, true);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (cancellationReasonPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<CancellationReasonDTO> cancellationReasonDTOs = cancellationReasonPage.getContent().stream().map(cancellationReason -> CancellationReasonDTO.builder()
                    .name(cancellationReason.getName())
                    .registrationDate(cancellationReason.getRegistrationDate())
                    .updateDate(cancellationReason.getUpdateDate())
                    .build()).toList();

            return new PageImpl<>(cancellationReasonDTOs, cancellationReasonPage.getPageable(),
                    cancellationReasonPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<Page<CancellationReasonDTO>> listFalse(String name,Date registrationStartDate, Date registrationEndDate, Date updateStartDate, Date updateEndDate, String sort, String sortColumn, Integer pageNumber,
                                                     Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(() -> {
            Page<CancellationReason> cancellationReasonPage;

            try {
                cancellationReasonPage = cancellationReasonRepositoryCustom.searchForCancellationReason(name,registrationStartDate,registrationEndDate,updateStartDate,updateStartDate, sort, sortColumn, pageNumber, pageSize, false);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (cancellationReasonPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<CancellationReasonDTO> cancellationReasonDTOs = cancellationReasonPage.getContent().stream().map(cancellationReason -> CancellationReasonDTO.builder()
                    .name(cancellationReason.getName())
                    .registrationDate(cancellationReason.getRegistrationDate())
                    .updateDate(cancellationReason.getUpdateDate())
                    .build()).toList();

            return new PageImpl<>(cancellationReasonDTOs, cancellationReasonPage.getPageable(),
                    cancellationReasonPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<String>> list() throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<CancellationReason> cancellationReasonList;
            try{
                cancellationReasonList = cancellationReasonRepository.findAllByStatusTrue();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(cancellationReasonList.isEmpty()){
                return Collections.emptyList();
            }
            try {
                return cancellationReasonList.stream().map(CancellationReason::getName).toList();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> delete(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(() -> {
            User user;
            CancellationReason cancellationReason;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                cancellationReason = cancellationReasonRepository.findByNameAndStatusTrue(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(cancellationReason == null){
                throw new BadRequestExceptions(Constants.ErrorCancellationReason);
            }

            try{
                cancellationReason.setStatus(false);
                cancellationReason.setUpdateDate(new Date(System.currentTimeMillis()));
                cancellationReason.setTokenUser(tokenUser.toUpperCase());
                cancellationReasonRepository.save(cancellationReason);
                iAudit.save("DELETE_CANCELLATION_REASON","RAZON DE CANCELACION "+cancellationReason.getName()+" DESACTIVADA.",cancellationReason.getName(),user.getUsername());
                return ResponseDelete.builder()
                        .code(200)
                        .message(Constants.delete)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            CancellationReason cancellationReason;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                cancellationReason = cancellationReasonRepository.findByNameAndStatusFalse(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(cancellationReason == null){
                throw new BadRequestExceptions(Constants.ErrorCancellationReason);
            }

            try{
                cancellationReason.setStatus(true);
                cancellationReason.setUpdateDate(new Date(System.currentTimeMillis()));
                cancellationReason.setTokenUser(tokenUser.toUpperCase());
                cancellationReasonRepository.save(cancellationReason);
                iAudit.save("ACTIVATE_CANCELLATION_REASON","RAZON DE CANCELACION "+cancellationReason.getName()+" ACTIVADA.",cancellationReason.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.update)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }
}
