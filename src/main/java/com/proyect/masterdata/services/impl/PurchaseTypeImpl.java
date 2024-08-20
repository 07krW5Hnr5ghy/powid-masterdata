package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.PurchaseType;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.PurchaseTypeRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IPurchaseType;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class PurchaseTypeImpl implements IPurchaseType {

    private final UserRepository userRepository;
    private final PurchaseTypeRepository purchaseTypeRepository;
    private final IAudit iAudit;
    @Override
    public ResponseSuccess save(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        PurchaseType purchaseType;

        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            purchaseType = purchaseTypeRepository.findByName(name.toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(purchaseType != null){
            throw new BadRequestExceptions(Constants.ErrorPurchaseTypeExists);
        }

        try{
            PurchaseType newPurchaseType = purchaseTypeRepository.save(PurchaseType.builder()
                            .name(name.toUpperCase())
                            .status(true)
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .tokenUser(user.getUsername())
                    .build());
            iAudit.save("ADD_PURCHASE_TYPE","TIPO DE COMPRA "+ newPurchaseType.getName()+" CREADO.",newPurchaseType.getName(),user.getUsername());
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
            PurchaseType purchaseType;

            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                purchaseType = purchaseTypeRepository.findByName(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(purchaseType != null){
                throw new BadRequestExceptions(Constants.ErrorPurchaseTypeExists);
            }

            try{
                PurchaseType newPurchaseType = purchaseTypeRepository.save(PurchaseType.builder()
                        .name(name.toUpperCase())
                        .status(true)
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .tokenUser(user.getUsername())
                        .build());
                iAudit.save("ADD_PURCHASE_TYPE","TIPO DE COMPRA "+ newPurchaseType.getName()+" CREADO.",newPurchaseType.getName(),user.getUsername());
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
    public CompletableFuture<ResponseDelete> delete(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            PurchaseType purchaseType;

            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                purchaseType = purchaseTypeRepository.findByNameAndStatusTrue(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(purchaseType == null){
                throw new BadRequestExceptions(Constants.ErrorPurchaseType);
            }

            try{
                purchaseType.setStatus(false);
                purchaseType.setTokenUser(user.getUsername());
                purchaseType.setUpdateDate(new Date(System.currentTimeMillis()));
                purchaseTypeRepository.save(purchaseType);
                iAudit.save("DELETE_PURCHASE_TYPE","TIPO DE COMPRA "+ purchaseType.getName()+" DESACTIVADO.",purchaseType.getName(),user.getUsername());
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
            PurchaseType purchaseType;

            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                purchaseType = purchaseTypeRepository.findByNameAndStatusFalse(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(purchaseType == null){
                throw new BadRequestExceptions(Constants.ErrorPurchaseType);
            }

            try{
                purchaseType.setStatus(true);
                purchaseType.setTokenUser(user.getUsername());
                purchaseType.setUpdateDate(new Date(System.currentTimeMillis()));
                purchaseTypeRepository.save(purchaseType);
                iAudit.save("ACTIVATE_PURCHASE_TYPE","TIPO DE COMPRA "+ purchaseType.getName()+" ACTIVADO.",purchaseType.getName(),user.getUsername());
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
    public CompletableFuture<List<String>> list() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<PurchaseType> purchaseTypeList;

            try{
                purchaseTypeList = purchaseTypeRepository.findAllByStatusTrue();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if(purchaseTypeList.isEmpty()){
                return Collections.emptyList();
            }

            return purchaseTypeList.stream().map(PurchaseType::getName).toList();
        });
    }

    @Override
    public CompletableFuture<List<String>> listFilter() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<PurchaseType> purchaseTypeList;

            try{
                purchaseTypeList = purchaseTypeRepository.findAll();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if(purchaseTypeList.isEmpty()){
                return Collections.emptyList();
            }

            return purchaseTypeList.stream().map(PurchaseType::getName).toList();
        });
    }
}
