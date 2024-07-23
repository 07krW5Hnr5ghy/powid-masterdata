package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.CustomerType;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.CustomerTypeDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.CustomerTypeRepository;
import com.proyect.masterdata.repository.CustomerTypeRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.ICustomerType;
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
public class CustomerTypeImpl implements ICustomerType {
    private final UserRepository userRepository;
    private final CustomerTypeRepository customerTypeRepository;
    private final CustomerTypeRepositoryCustom customerTypeRepositoryCustom;
    private final IAudit iAudit;
    @Override
    public CompletableFuture<ResponseSuccess> save(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
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
                CustomerType newCustomerType = customerTypeRepository.save(CustomerType.builder()
                        .name(name.toUpperCase())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .updateDate(new Date(System.currentTimeMillis()))
                        .status(true)
                        .build());
                iAudit.save("ADD_CUSTOMER_TYPE","ADD CUSTOMER TYPE "+newCustomerType.getName()+".",user.getUsername());
                return ResponseSuccess.builder()
                        .message(Constants.register)
                        .code(200)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> delete(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            CustomerType customerType;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                customerType = customerTypeRepository.findByNameAndStatusTrue(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(customerType==null){
                throw new BadRequestExceptions(Constants.ErrorCustomerType);
            }
            try {
                customerType.setStatus(false);
                customerType.setUpdateDate(new Date(System.currentTimeMillis()));
                customerType.setTokenUser(user.getUsername());
                customerTypeRepository.save(customerType);
                iAudit.save("DELETE_CUSTOMER_TYPE","DELETE CUSTOMER TYPE "+customerType.getName()+".",user.getUsername());
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
            CustomerType customerType;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                customerType = customerTypeRepository.findByNameAndStatusFalse(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(customerType==null){
                throw new BadRequestExceptions(Constants.ErrorCustomerType);
            }
            try {
                customerType.setStatus(true);
                customerType.setUpdateDate(new Date(System.currentTimeMillis()));
                customerType.setTokenUser(user.getUsername());
                customerTypeRepository.save(customerType);
                iAudit.save("ACTIVATE_CUSTOMER_TYPE","DELETE CUSTOMER TYPE "+customerType.getName()+".",user.getUsername());
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
    public CompletableFuture<List<String>> list() throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<CustomerType> customerTypeList;
            try{
                customerTypeList = customerTypeRepository.findAllByStatusTrue();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(customerTypeList.isEmpty()){
                return Collections.emptyList();
            }
            return customerTypeList.stream().map(CustomerType::getName).toList();
        });
    }

    @Override
    public CompletableFuture<Page<CustomerTypeDTO>> listPagination(
            String name,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<CustomerType> customerTypePage;
            try {
                customerTypePage = customerTypeRepositoryCustom.searchForCustomerType(
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
            if(customerTypePage.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }
            List<CustomerTypeDTO> customerTypeDTOs = customerTypePage.getContent().stream().map(customerType -> CustomerTypeDTO.builder()
                    .name(customerType.getName())
                    .registrationDate(customerType.getRegistrationDate())
                    .updateDate(customerType.getUpdateDate())
                    .build()).toList();
            return new PageImpl<>(customerTypeDTOs,customerTypePage.getPageable(),customerTypePage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<Page<CustomerTypeDTO>> listFalse(String name, Date registrationStartDate, Date registrationEndDate, Date updateStartDate, Date updateEndDate, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<CustomerType> customerTypePage;
            try {
                customerTypePage = customerTypeRepositoryCustom.searchForCustomerType(
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
            if(customerTypePage.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }
            List<CustomerTypeDTO> customerTypeDTOs = customerTypePage.getContent().stream().map(customerType -> CustomerTypeDTO.builder()
                    .name(customerType.getName())
                    .registrationDate(customerType.getRegistrationDate())
                    .updateDate(customerType.getUpdateDate())
                    .build()).toList();
            return new PageImpl<>(customerTypeDTOs,customerTypePage.getPageable(),customerTypePage.getTotalElements());
        });
    }
}
