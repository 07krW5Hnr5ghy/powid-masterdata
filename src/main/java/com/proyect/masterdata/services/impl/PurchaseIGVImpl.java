package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.PurchaseIGV;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.PurchaseIGVDTO;
import com.proyect.masterdata.dto.request.RequestPurchaseIGV;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.PurchaseIGVRepository;
import com.proyect.masterdata.repository.PurchaseIGVRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IPurchaseIGV;
import com.proyect.masterdata.utils.Constants;
import lombok.AllArgsConstructor;
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
@AllArgsConstructor
@Log4j2
public class PurchaseIGVImpl implements IPurchaseIGV {
    private final UserRepository userRepository;
    private final PurchaseIGVRepository purchaseIGVRepository;
    private final IAudit iAudit;
    private final PurchaseIGVRepositoryCustom purchaseIGVRepositoryCustom;
    @Override
    public ResponseSuccess save(RequestPurchaseIGV requestPurchaseIGV) throws BadRequestExceptions, InternalErrorExceptions {
        User user;
        PurchaseIGV purchaseIGV;
        try {
            user = userRepository.findByUsernameAndStatusTrue(requestPurchaseIGV.getUsername());
            purchaseIGV = purchaseIGVRepository.findByName(requestPurchaseIGV.getName());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
        if(user==null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }
        if(purchaseIGV !=null){
            throw new BadRequestExceptions(Constants.ErrorPurchaseIGVExists);
        }
        try{
            PurchaseIGV newPurchaseIGV = purchaseIGVRepository.save(PurchaseIGV.builder()
                    .name(requestPurchaseIGV.getName().toUpperCase())
                    .registrationDate(OffsetDateTime.now())
                    .updateDate(OffsetDateTime.now())
                    .user(user)
                    .userId(user.getId())
                    .percentage(requestPurchaseIGV.getPercentage())
                    .status(true)
                    .client(user.getClient())
                    .clientId(user.getClientId())
                    .build());
            iAudit.save("ADD_PURCHASE_DISCOUNT","DESCUENTO DE COMPRA "+ newPurchaseIGV.getName()+" CREADO.", newPurchaseIGV.getName(),user.getUsername());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public CompletableFuture<ResponseSuccess> saveAsync(RequestPurchaseIGV requestPurchaseIGV) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            PurchaseIGV purchaseIGV;
            try {
                user = userRepository.findByUsernameAndStatusTrue(requestPurchaseIGV.getUsername());
                purchaseIGV = purchaseIGVRepository.findByName(requestPurchaseIGV.getName());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(purchaseIGV !=null){
                throw new BadRequestExceptions(Constants.ErrorPurchaseIGVExists);
            }
            try{
                PurchaseIGV newPurchaseIGV = purchaseIGVRepository.save(PurchaseIGV.builder()
                        .name(requestPurchaseIGV.getName().toUpperCase())
                        .registrationDate(OffsetDateTime.now())
                        .updateDate(OffsetDateTime.now())
                        .user(user)
                        .userId(user.getId())
                        .percentage(requestPurchaseIGV.getPercentage())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .status(true)
                        .build());
                iAudit.save("ADD_PURCHASE_DISCOUNT","DESCUENTO DE COMPRA "+ newPurchaseIGV.getName()+" CREADO.", newPurchaseIGV.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<List<PurchaseIGVDTO>> listPurchaseDiscount() throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<PurchaseIGV> purchaseIGVList;
            try{
                purchaseIGVList = purchaseIGVRepository.findAllByStatusTrue();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if (purchaseIGVList.isEmpty()){
                return Collections.emptyList();
            }
            return purchaseIGVList.stream().map(purchaseIGV -> PurchaseIGVDTO.builder()
                    .id(purchaseIGV.getId())
                    .value(purchaseIGV.getValue())
                    .percentage(purchaseIGV.getPercentage())
                    .name(purchaseIGV.getName())
                    .status(purchaseIGV.getStatus())
                    .user(purchaseIGV.getUser().getUsername())
                    .build()).toList();
        });
    }
    @Override
    public CompletableFuture<Page<PurchaseIGVDTO>> list(String username, String name, Double value, Boolean percentage, OffsetDateTime registrationStartDate, OffsetDateTime registrationEndDate, OffsetDateTime updateStartDate, OffsetDateTime updateEndDate, String sort, String sortColumn, Integer pageNumber, Integer pageSize, Boolean status) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            UUID clientId;
            Page<PurchaseIGV> purchaseDiscountPage;
            try{
                clientId = userRepository.findByUsernameAndStatusTrue(username.toUpperCase()).getClientId();
                purchaseDiscountPage = purchaseIGVRepositoryCustom.searchForPurchaseIGV(
                        clientId,
                        name,
                        value,
                        percentage,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateEndDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        status
                );
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if (purchaseDiscountPage.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }
            List<PurchaseIGVDTO> purchaseIGVDTOS = purchaseDiscountPage.getContent().stream().map(purchaseIGV -> PurchaseIGVDTO.builder()
                    .value(purchaseIGV.getValue())
                    .id(purchaseIGV.getId())
                    .percentage(purchaseIGV.getPercentage())
                    .status(purchaseIGV.getStatus())
                    .name(purchaseIGV.getName())
                    .user(purchaseIGV.getUser().getUsername())
                    .build()).toList();
            return new PageImpl<>(purchaseIGVDTOS,purchaseDiscountPage.getPageable(),purchaseDiscountPage.getTotalElements());
        });
    }
}
