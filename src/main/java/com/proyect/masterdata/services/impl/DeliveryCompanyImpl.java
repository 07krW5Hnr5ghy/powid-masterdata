package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.DeliveryCompany;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.DeliveryCompanyDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.DeliveryCompanyRepository;
import com.proyect.masterdata.repository.DeliveryCompanyRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IDeliveryCompany;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class DeliveryCompanyImpl implements IDeliveryCompany {
    private final UserRepository userRepository;
    private final DeliveryCompanyRepository deliveryCompanyRepository;
    private final IAudit iAudit;
    private final DeliveryCompanyRepositoryCustom deliveryCompanyRepositoryCustom;
    @Override
    public CompletableFuture<ResponseSuccess> save(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            DeliveryCompany deliveryCompany;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                deliveryCompany = deliveryCompanyRepository.findByName(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(deliveryCompany!=null){
                throw new BadRequestExceptions(Constants.ErrorDeliveryCompanyExists);
            }
            try{
                DeliveryCompany newDeliveryCompany = deliveryCompanyRepository.save(DeliveryCompany.builder()
                                .name(name.toUpperCase())
                                .user(user)
                                .userId(user.getId())
                                .client(user.getClient())
                                .clientId(user.getClientId())
                                .registrationDate(OffsetDateTime.now())
                                .updateDate(OffsetDateTime.now())
                        .build());
                iAudit.save("ADD_DELIVERY_COMPANY","EMPRESA DE COURIER "+newDeliveryCompany.getName()+" CREADA.",newDeliveryCompany.getName(),user.getUsername());
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
    public CompletableFuture<ResponseDelete> delete(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            DeliveryCompany deliveryCompany;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                deliveryCompany = deliveryCompanyRepository.findByName(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(deliveryCompany==null){
                throw new BadRequestExceptions(Constants.ErrorDeliveryCompany);
            }
            try{
                deliveryCompany.setStatus(false);
                deliveryCompany.setUpdateDate(OffsetDateTime.now());
                deliveryCompany.setUser(user);
                deliveryCompany.setUserId(user.getId());
                iAudit.save("DELETE_DELIVERY_COMPANY","EMPRESA DE COURIER "+deliveryCompany.getName()+" CREADA.",deliveryCompany.getName(),user.getUsername());
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
            DeliveryCompany deliveryCompany;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                deliveryCompany = deliveryCompanyRepository.findByName(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(deliveryCompany==null){
                throw new BadRequestExceptions(Constants.ErrorDeliveryCompany);
            }
            try{
                deliveryCompany.setStatus(true);
                deliveryCompany.setUpdateDate(OffsetDateTime.now());
                deliveryCompany.setUser(user);
                deliveryCompany.setUserId(user.getId());
                iAudit.save("ACTIVATE_DELIVERY_COMPANY","EMPRESA DE COURIER "+deliveryCompany.getName()+" CREADA.",deliveryCompany.getName(),user.getUsername());
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

    @Override
    public CompletableFuture<List<DeliveryCompanyDTO>> listDeliveryCompany() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<DeliveryCompany> deliveryCompanies;
            try {
                deliveryCompanies = deliveryCompanyRepository.findAllByStatusTrue();
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if (deliveryCompanies.isEmpty()) {
                return Collections.emptyList();
            }
            return deliveryCompanies.stream().map(deliveryCompany -> DeliveryCompanyDTO.builder()
                    .name(deliveryCompany.getName())
                    .registrationDate(deliveryCompany.getRegistrationDate())
                    .updateDate(deliveryCompany.getUpdateDate())
                    .build()).toList();
        });
    }

    @Override
    public CompletableFuture<Page<DeliveryCompanyDTO>> list(String user, String name, OffsetDateTime registrationStartDate, OffsetDateTime registrationEndDate, OffsetDateTime updateStartDate, OffsetDateTime updateEndDate, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<DeliveryCompany> deliveryCompanyPage;
            UUID clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                deliveryCompanyPage = deliveryCompanyRepositoryCustom.searchForDeliveryCompany(
                        clientId,
                        name,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateEndDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        true);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if(deliveryCompanyPage.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }
            List<DeliveryCompanyDTO> deliveryCompanyDTOS = deliveryCompanyPage.stream().map(deliveryCompany -> DeliveryCompanyDTO.builder()
                    .name(deliveryCompany.getName())
                    .registrationDate(deliveryCompany.getRegistrationDate())
                    .updateDate(deliveryCompany.getUpdateDate())
                    .build()).toList();
            return new PageImpl<>(deliveryCompanyDTOS,deliveryCompanyPage.getPageable(),deliveryCompanyPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<Page<DeliveryCompanyDTO>> listFalse(String user, String name, OffsetDateTime registrationStartDate, OffsetDateTime registrationEndDate, OffsetDateTime updateStartDate, OffsetDateTime updateEndDate, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<DeliveryCompany> deliveryCompanyPage;
            UUID clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                deliveryCompanyPage = deliveryCompanyRepositoryCustom.searchForDeliveryCompany(
                        clientId,
                        name,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateEndDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        false);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if(deliveryCompanyPage.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }
            List<DeliveryCompanyDTO> deliveryCompanyDTOS = deliveryCompanyPage.stream().map(deliveryCompany -> DeliveryCompanyDTO.builder()
                    .name(deliveryCompany.getName())
                    .registrationDate(deliveryCompany.getRegistrationDate())
                    .updateDate(deliveryCompany.getUpdateDate())
                    .build()).toList();
            return new PageImpl<>(deliveryCompanyDTOS,deliveryCompanyPage.getPageable(),deliveryCompanyPage.getTotalElements());
        });
    }
}
