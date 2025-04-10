package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.KardexOperationType;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.KardexOperationTypeDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.KardexOperationTypeRepository;
import com.proyect.masterdata.repository.KardexOperationTypeRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IKardexOperationType;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class KardexOperationTypeImpl implements IKardexOperationType {
    private final UserRepository userRepository;
    private final KardexOperationTypeRepository kardexOperationTypeRepository;
    private final IAudit iAudit;
    private final KardexOperationTypeRepositoryCustom kardexOperationTypeRepositoryCustom;
    @Override
    public CompletableFuture<ResponseSuccess> save(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            KardexOperationType kardexOperationType;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }else{
                kardexOperationType = kardexOperationTypeRepository.findByNameAndClientId(name.toUpperCase(),user.getClientId());
            }
            if (kardexOperationType != null) {
                throw new BadRequestExceptions(Constants.ErrorKardexOperationTypeExists.toUpperCase());
            }

            try {
                KardexOperationType newKardexOperationType = kardexOperationTypeRepository.save(KardexOperationType.builder()
                        .name(name.toUpperCase())
                        .registrationDate(OffsetDateTime.now())
                        .status(true)
                        .user(user)
                        .userId(user.getId())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .build());
                iAudit.save("ADD_KARDEX_OPERATION_TYPE","OPERACION DE KARDEX "+newKardexOperationType.getName()+" CREADA.",newKardexOperationType.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> delete(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            KardexOperationType kardexOperationType;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }else{
                kardexOperationType = kardexOperationTypeRepository.findByNameAndClientIdAndStatusTrue(name.toUpperCase(),user.getClientId());
            }
            if (kardexOperationType == null) {
                throw new BadRequestExceptions(Constants.ErrorKardexOperationType.toUpperCase());
            }

            try {
                kardexOperationType.setStatus(false);
                kardexOperationType.setUpdateDate(OffsetDateTime.now());
                kardexOperationType.setUser(user);
                kardexOperationType.setUserId(user.getId());
                kardexOperationTypeRepository.save(kardexOperationType);
                iAudit.save("DELETE_KARDEX_OPERATION_TYPE","OPERACION DE KARDEX "+kardexOperationType.getName()+" DESACTIVADA.", kardexOperationType.getName(), user.getUsername());
                return ResponseDelete.builder()
                        .code(200)
                        .message(Constants.delete)
                        .build();
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            KardexOperationType kardexOperationType;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }else{
                kardexOperationType = kardexOperationTypeRepository.findByNameAndClientIdAndStatusTrue(name.toUpperCase(),user.getClientId());
            }
            if (kardexOperationType == null) {
                throw new BadRequestExceptions(Constants.ErrorKardexOperationType.toUpperCase());
            }

            try {
                kardexOperationType.setStatus(true);
                kardexOperationType.setUpdateDate(OffsetDateTime.now());
                kardexOperationType.setUser(user);
                kardexOperationType.setUserId(user.getId());
                kardexOperationTypeRepository.save(kardexOperationType);
                iAudit.save("ACTIVATE_KARDEX_OPERATION_TYPE","OPERACION DE KARDEX "+kardexOperationType.getName()+" ACTIVADA.", kardexOperationType.getName(), user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.update)
                        .build();
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<List<KardexOperationTypeDTO>> listKardexOperationType(String username) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<KardexOperationType> kardexOperationTypes;
            User user;
            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                kardexOperationTypes = kardexOperationTypeRepository.findAllByStatusTrueAndClientId(user.getClientId());
            }
            if (kardexOperationTypes.isEmpty()) {
                return Collections.emptyList();
            }
            return kardexOperationTypes.stream().map(kardexOperationType -> KardexOperationTypeDTO.builder()
                    .id(kardexOperationType.getId())
                    .updateDate(kardexOperationType.getUpdateDate())
                    .name(kardexOperationType.getName())
                    .registrationDate(kardexOperationType.getRegistrationDate())
                    .user(kardexOperationType.getUser().getUsername())
                    .status(kardexOperationType.getStatus())
                    .build()).toList();
        });
    }

    @Override
    public CompletableFuture<Page<KardexOperationTypeDTO>> list(String name, OffsetDateTime registrationStartDate, OffsetDateTime registrationEndDate, OffsetDateTime updateStartDate, OffsetDateTime updateEndDate, String sort, String sortColumn, Integer pageNumber, Integer pageSize, Boolean status) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<KardexOperationType> kardexOperationTypePage;
            try {
                kardexOperationTypePage = kardexOperationTypeRepositoryCustom.searchForKardexOperationType(
                        name,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateEndDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        status);
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if (kardexOperationTypePage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }
            List<KardexOperationTypeDTO> kardexOperationTypeDTOS = kardexOperationTypePage.getContent().stream().map(kardexOperationType -> KardexOperationTypeDTO.builder()
                    .id(kardexOperationType.getId())
                    .updateDate(kardexOperationType.getUpdateDate())
                    .name(kardexOperationType.getName())
                    .registrationDate(kardexOperationType.getRegistrationDate())
                    .user(kardexOperationType.getUser().getUsername())
                    .status(kardexOperationType.getStatus())
                    .build()).toList();
            return new PageImpl<>(kardexOperationTypeDTOS,
                    kardexOperationTypePage.getPageable(), kardexOperationTypePage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<KardexOperationTypeDTO>> listFilter(String username) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<KardexOperationType> kardexOperationTypes;
            User user;
            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                kardexOperationTypes = kardexOperationTypeRepository.findAllByClientId(user.getClientId());
            }
            if (kardexOperationTypes.isEmpty()) {
                return Collections.emptyList();
            }
            return kardexOperationTypes.stream().map(kardexOperationType -> KardexOperationTypeDTO.builder()
                    .id(kardexOperationType.getId())
                    .updateDate(kardexOperationType.getUpdateDate())
                    .name(kardexOperationType.getName())
                    .registrationDate(kardexOperationType.getRegistrationDate())
                    .user(kardexOperationType.getUser().getUsername())
                    .status(kardexOperationType.getStatus())
                    .build()).toList();
        });
    }
}
