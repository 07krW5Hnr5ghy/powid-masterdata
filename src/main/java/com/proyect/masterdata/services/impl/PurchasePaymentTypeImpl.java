package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Color;
import com.proyect.masterdata.domain.PurchasePaymentType;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.PurchasePaymentTypeDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.PurchasePaymentTypeRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IPurchasePaymentType;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class PurchasePaymentTypeImpl implements IPurchasePaymentType {
    private final PurchasePaymentTypeRepository purchasePaymentTypeRepository;
    private final UserRepository userRepository;
    private final IAudit iAudit;
    @Override
    public CompletableFuture<ResponseSuccess> save(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            PurchasePaymentType purchasePaymentType;
            User user;
            try{
                purchasePaymentType = purchasePaymentTypeRepository.findByName(name);
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(purchasePaymentType!=null){
                throw new BadRequestExceptions(Constants.ErrorPurchasePaymentTypeExists);
            }
            try{
                PurchasePaymentType newPurchasePaymentType = purchasePaymentTypeRepository.save(PurchasePaymentType.builder()
                                .name(name.toUpperCase())
                                .registrationDate(OffsetDateTime.now())
                                .updateDate(OffsetDateTime.now())
                                .status(true)
                                .user(user)
                                .userId(user.getId())
                        .build());
                iAudit.save(
                        "ADD_PURCHASE_PAYMENT_TYPE",
                        "TIPO DE PAGO PARA COMPRA "+
                                newPurchasePaymentType.getName()+" CREADO.",
                        newPurchasePaymentType.getName(),
                        user.getUsername());
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
            PurchasePaymentType purchasePaymentType;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                purchasePaymentType = purchasePaymentTypeRepository.findByNameAndStatusTrue(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(purchasePaymentType==null){
                throw new BadRequestExceptions(Constants.ErrorPurchasePaymentType);
            }
            try{
                purchasePaymentType.setStatus(false);
                purchasePaymentType.setUser(user);
                purchasePaymentType.setUserId(user.getId());
                purchasePaymentType.setUpdateDate(OffsetDateTime.now());
                purchasePaymentTypeRepository.save(purchasePaymentType);
                iAudit.save(
                        "DELETE_PURCHASE_PAYMENT_TYPE",
                        "TIPO DE PAGO PARA COMPRA "+
                                purchasePaymentType.getName()+" ELIMINADO.",
                        purchasePaymentType.getName(),
                        user.getUsername());
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
    public CompletableFuture<ResponseSuccess> activate(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            PurchasePaymentType purchasePaymentType;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                purchasePaymentType = purchasePaymentTypeRepository.findByNameAndStatusTrue(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(purchasePaymentType==null){
                throw new BadRequestExceptions(Constants.ErrorPurchasePaymentType);
            }
            try{
                purchasePaymentType.setStatus(true);
                purchasePaymentType.setUser(user);
                purchasePaymentType.setUserId(user.getId());
                purchasePaymentType.setUpdateDate(OffsetDateTime.now());
                purchasePaymentTypeRepository.save(purchasePaymentType);
                iAudit.save(
                        "ACTIVATE_PURCHASE_PAYMENT_TYPE",
                        "TIPO DE PAGO PARA COMPRA "+
                                purchasePaymentType.getName()+" ACTIVADO.",
                        purchasePaymentType.getName(),
                        user.getUsername());
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

    @Override
    public CompletableFuture<List<PurchasePaymentTypeDTO>> listPurchasePaymentType() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<PurchasePaymentType> purchasePaymentTypes;
            try {
                purchasePaymentTypes = purchasePaymentTypeRepository.findAllByStatusTrue();
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if (purchasePaymentTypes.isEmpty()) {
                return Collections.emptyList();
            }
            return purchasePaymentTypes.stream().map(purchasePaymentType -> PurchasePaymentTypeDTO.builder()
                    .name(purchasePaymentType.getName())
                    .registrationDate(purchasePaymentType.getRegistrationDate())
                    .updateDate(purchasePaymentType.getUpdateDate())
                    .status(purchasePaymentType.getStatus())
                    .build()).toList();
        });
    }
}
