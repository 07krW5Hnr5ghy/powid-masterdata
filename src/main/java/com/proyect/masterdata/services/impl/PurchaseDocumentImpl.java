package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.PurchaseDocument;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.PurchaseDocumentRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IPurchaseDocument;
import com.proyect.masterdata.utils.Constants;
import lombok.AllArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@AllArgsConstructor
@Log4j2
public class PurchaseDocumentImpl implements IPurchaseDocument {
    private final PurchaseDocumentRepository purchaseDocumentRepository;
    private final UserRepository userRepository;
    private final IAudit iAudit;
    @Override
    public CompletableFuture<ResponseSuccess> save(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            PurchaseDocument purchaseDocument;
            User user;
            try {
                purchaseDocument = purchaseDocumentRepository.findByName(name.toUpperCase());
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(purchaseDocument != null){
                throw new BadRequestExceptions(Constants.ErrorPurchaseDocumentExists);
            }

            try {
                PurchaseDocument newPurchaseDocument=purchaseDocumentRepository.save(PurchaseDocument.builder()
                        .name(name.toUpperCase())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .updateDate(new Date(System.currentTimeMillis()))
                        .status(true)
                        .tokenUser(user.getUsername())
                        .build());
                iAudit.save("ADD_PURCHASE_DOCUMENT","ADD PURCHASE DOCUMENT "+newPurchaseDocument.getName()+".",user.getUsername());
                return ResponseSuccess.builder()
                        .message(Constants.register)
                        .code(200)
                        .build();
            } catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> delete(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            PurchaseDocument purchaseDocument;
            User user;

            try {
                purchaseDocument = purchaseDocumentRepository.findByNameAndStatusTrue(name.toUpperCase());
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(purchaseDocument == null){
                throw new BadRequestExceptions(Constants.ErrorPurchaseDocument);
            }

            try {
                purchaseDocument.setStatus(false);
                purchaseDocument.setUpdateDate(new Date(System.currentTimeMillis()));
                purchaseDocument.setTokenUser(user.getUsername());
                purchaseDocumentRepository.save(purchaseDocument);
                iAudit.save("DELETE_PURCHASE_DOCUMENT","DELETE PURCHASE DOCUMENT "+purchaseDocument.getName()+".",user.getUsername());
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
            PurchaseDocument purchaseDocument;
            User user;

            try {
                purchaseDocument = purchaseDocumentRepository.findByNameAndStatusFalse(name.toUpperCase());
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(purchaseDocument == null){
                throw new BadRequestExceptions(Constants.ErrorPurchaseDocument);
            }

            try {
                purchaseDocument.setStatus(true);
                purchaseDocument.setUpdateDate(new Date(System.currentTimeMillis()));
                purchaseDocument.setTokenUser(user.getUsername());
                purchaseDocumentRepository.save(purchaseDocument);
                iAudit.save("ACTIVATE_PURCHASE_DOCUMENT","ACTIVATE PURCHASE DOCUMENT "+purchaseDocument.getName()+".",user.getUsername());
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
    public CompletableFuture<List<String>> list() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<PurchaseDocument> purchaseDocumentList = new ArrayList<>();
            try {
                purchaseDocumentList = purchaseDocumentRepository.findAllByStatusTrue();
            }catch (RuntimeException e){
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if(purchaseDocumentList.isEmpty()){
                return Collections.emptyList();
            }
            return purchaseDocumentList.stream().map(purchaseDocument -> purchaseDocument.getName().toUpperCase()).toList();
        });
    }
}
