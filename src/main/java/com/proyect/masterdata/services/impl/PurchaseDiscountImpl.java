package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.PurchaseDiscount;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.PurchaseDiscountDTO;
import com.proyect.masterdata.dto.request.RequestPurchaseDiscount;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.PurchaseDiscountRepository;
import com.proyect.masterdata.repository.PurchaseDiscountRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IPurchaseDiscount;
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
public class PurchaseDiscountImpl implements IPurchaseDiscount {
    private final UserRepository userRepository;
    private final PurchaseDiscountRepository purchaseDiscountRepository;
    private final IAudit iAudit;
    private final PurchaseDiscountRepositoryCustom purchaseDiscountRepositoryCustom;
    @Override
    public ResponseSuccess save(RequestPurchaseDiscount requestPurchaseDiscount) throws BadRequestExceptions, InternalErrorExceptions {
        User user;
        PurchaseDiscount purchaseDiscount;
        try {
            user = userRepository.findByUsernameAndStatusTrue(requestPurchaseDiscount.getUsername());
            purchaseDiscount = purchaseDiscountRepository.findByName(requestPurchaseDiscount.getName());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
        if(user==null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }
        if(purchaseDiscount!=null){
            throw new BadRequestExceptions(Constants.ErrorPurchaseDiscountExists);
        }
        try{
            PurchaseDiscount newPurchaseDiscount = purchaseDiscountRepository.save(PurchaseDiscount.builder()
                    .name(requestPurchaseDiscount.getName().toUpperCase())
                    .registrationDate(OffsetDateTime.now())
                    .updateDate(OffsetDateTime.now())
                    .user(user)
                    .userId(user.getId())
                    .percentage(requestPurchaseDiscount.getPercentage())
                    .status(true)
                    .client(user.getClient())
                    .clientId(user.getClientId())
                    .build());
            iAudit.save("ADD_PURCHASE_DISCOUNT","DESCUENTO DE COMPRA "+newPurchaseDiscount.getName()+" CREADO.",newPurchaseDiscount.getName(),user.getUsername());
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
    public CompletableFuture<ResponseSuccess> saveAsync(RequestPurchaseDiscount requestPurchaseDiscount) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            PurchaseDiscount purchaseDiscount;
            try {
                user = userRepository.findByUsernameAndStatusTrue(requestPurchaseDiscount.getUsername());
                purchaseDiscount = purchaseDiscountRepository.findByName(requestPurchaseDiscount.getName());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(purchaseDiscount!=null){
                throw new BadRequestExceptions(Constants.ErrorPurchaseDiscountExists);
            }
            try{
                PurchaseDiscount newPurchaseDiscount = purchaseDiscountRepository.save(PurchaseDiscount.builder()
                        .name(requestPurchaseDiscount.getName().toUpperCase())
                        .registrationDate(OffsetDateTime.now())
                        .updateDate(OffsetDateTime.now())
                        .user(user)
                        .userId(user.getId())
                        .percentage(requestPurchaseDiscount.getPercentage())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .status(true)
                        .build());
                iAudit.save("ADD_PURCHASE_DISCOUNT","DESCUENTO DE COMPRA "+newPurchaseDiscount.getName()+" CREADO.",newPurchaseDiscount.getName(),user.getUsername());
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
    public CompletableFuture<List<PurchaseDiscountDTO>> listPurchaseDiscount() throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<PurchaseDiscount> purchaseDiscountList;
            try{
                purchaseDiscountList = purchaseDiscountRepository.findAllByStatusTrue();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if (purchaseDiscountList.isEmpty()){
                return Collections.emptyList();
            }
            return purchaseDiscountList.stream().map(purchaseDiscount -> PurchaseDiscountDTO.builder()
                    .id(purchaseDiscount.getId())
                    .value(purchaseDiscount.getValue())
                    .percentage(purchaseDiscount.getPercentage())
                    .name(purchaseDiscount.getName())
                    .status(purchaseDiscount.getStatus())
                    .user(purchaseDiscount.getUser().getUsername())
                    .build()).toList();
        });
    }
    @Override
    public CompletableFuture<Page<PurchaseDiscountDTO>> list(String username, String name, Double value, Boolean percentage, OffsetDateTime registrationStartDate, OffsetDateTime registrationEndDate, OffsetDateTime updateStartDate, OffsetDateTime updateEndDate, String sort, String sortColumn, Integer pageNumber, Integer pageSize, Boolean status) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            UUID clientId;
            Page<PurchaseDiscount> purchaseDiscountPage;
            try{
                clientId = userRepository.findByUsernameAndStatusTrue(username.toUpperCase()).getClientId();
                purchaseDiscountPage = purchaseDiscountRepositoryCustom.searchForPurchaseDiscount(
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
            List<PurchaseDiscountDTO> purchaseDiscountDTOS = purchaseDiscountPage.getContent().stream().map(purchaseDiscount -> PurchaseDiscountDTO.builder()
                    .value(purchaseDiscount.getValue())
                    .id(purchaseDiscount.getId())
                    .percentage(purchaseDiscount.getPercentage())
                    .status(purchaseDiscount.getStatus())
                    .name(purchaseDiscount.getName())
                    .user(purchaseDiscount.getUser().getUsername())
                    .build()).toList();
            return new PageImpl<>(purchaseDiscountDTOS,purchaseDiscountPage.getPageable(),purchaseDiscountPage.getTotalElements());
        });
    }
}
