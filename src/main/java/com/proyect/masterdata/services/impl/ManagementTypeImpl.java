package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.ManagementType;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.ManagementTypeDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.ManagementTypeRepository;
import com.proyect.masterdata.repository.ManagementTypeRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IManagementType;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class ManagementTypeImpl implements IManagementType {
    private final UserRepository userRepository;
    private final ManagementTypeRepository managementTypeRepository;
    private final ManagementTypeRepositoryCustom managementTypeRepositoryCustom;
    private final IAudit iAudit;
    @Override
    public CompletableFuture<ResponseSuccess> save(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
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
                ManagementType newManagement = managementTypeRepository.save(ManagementType.builder()
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .status(true)
                        .name(name.toUpperCase())
                        .tokenUser(user.getUsername())
                        .build());
                iAudit.save("ADD_MANAGEMENT_TYPE","TIPO DE GESTION "+newManagement.getName()+" CREADO.",newManagement.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            }catch (RuntimeException e){
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> delete(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            ManagementType managementType;
            try{
                user = userRepository.findByUsernameAndStatusTrue(name.toUpperCase());
                managementType = managementTypeRepository.findByNameAndStatusTrue(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(managementType==null){
                throw new BadRequestExceptions(Constants.ErrorManagementType);
            }
            try {
                managementType.setStatus(false);
                managementType.setUpdateDate(new Date(System.currentTimeMillis()));
                managementType.setTokenUser(user.getUsername());
                managementTypeRepository.save(managementType);
                iAudit.save("DELETE_MANAGEMENT_TYPE","TIPO DE GESTION "+managementType.getName()+" DESACTIVADO.",managementType.getName(),user.getUsername());
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
    public CompletableFuture<ResponseSuccess> activate(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            ManagementType managementType;
            try{
                user = userRepository.findByUsernameAndStatusTrue(name.toUpperCase());
                managementType = managementTypeRepository.findByNameAndStatusFalse(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(managementType==null){
                throw new BadRequestExceptions(Constants.ErrorManagementType);
            }
            try {
                managementType.setStatus(true);
                managementType.setUpdateDate(new Date(System.currentTimeMillis()));
                managementType.setTokenUser(user.getUsername());
                managementTypeRepository.save(managementType);
                iAudit.save("ACTIVATE_MANAGEMENT_TYPE","TIPO DE GESTION "+managementType.getName()+" ACTIVADO.",managementType.getName(),user.getUsername());
                return ResponseSuccess.builder()
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
    public CompletableFuture<Page<ManagementTypeDTO>> listPagination(
            String name,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<ManagementType> managementTypePage;
            try {
                managementTypePage = managementTypeRepositoryCustom.searchForManagementType(
                        name,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateStartDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        true);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(managementTypePage.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }
            List<ManagementTypeDTO> managementTypeDTOs = managementTypePage.getContent().stream().map(managementType -> ManagementTypeDTO.builder()
                    .name(managementType.getName())
                    .registrationDate(managementType.getRegistrationDate())
                    .updateDate(managementType.getUpdateDate())
                    .build()
            ).toList();
            return new PageImpl<>(managementTypeDTOs,managementTypePage.getPageable(),managementTypePage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<Page<ManagementTypeDTO>> listFalse(String name, Date registrationStartDate, Date registrationEndDate, Date updateStartDate, Date updateEndDate, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<ManagementType> managementTypePage;
            try {
                managementTypePage = managementTypeRepositoryCustom.searchForManagementType(
                        name,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateStartDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        false);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(managementTypePage.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }
            List<ManagementTypeDTO> managementTypeDTOs = managementTypePage.getContent().stream().map(managementType -> ManagementTypeDTO.builder()
                    .name(managementType.getName())
                    .registrationDate(managementType.getRegistrationDate())
                    .updateDate(managementType.getUpdateDate())
                    .build()
            ).toList();
            return new PageImpl<>(managementTypeDTOs,managementTypePage.getPageable(),managementTypePage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<String>> list() throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<ManagementType> managementTypeList;
            try {
                managementTypeList = managementTypeRepository.findAllByStatusTrue();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(managementTypeList.isEmpty()){
                return Collections.emptyList();
            }
            return managementTypeList.stream().map(ManagementType::getName).toList();
        });
    }
}
