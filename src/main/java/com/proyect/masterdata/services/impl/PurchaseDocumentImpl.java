package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.ShipmentDocument;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.ShipmentDocumentRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IPurchaseDocument;
import com.proyect.masterdata.utils.Constants;
import lombok.AllArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@AllArgsConstructor
@Log4j2
public class PurchaseDocumentImpl implements IPurchaseDocument {
    private final ShipmentDocumentRepository shipmentDocumentRepository;
    private final UserRepository userRepository;
    private final IAudit iAudit;
    @Override
    public CompletableFuture<ResponseSuccess> save(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            ShipmentDocument shipmentDocument;
            User user;
            try {
                shipmentDocument = shipmentDocumentRepository.findByName(name.toUpperCase());
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(shipmentDocument != null){
                throw new BadRequestExceptions(Constants.ErrorShipmentDocumentExists);
            }

            try {
                ShipmentDocument newShipmentDocument = shipmentDocumentRepository.save(ShipmentDocument.builder()
                        .name(name.toUpperCase())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .updateDate(new Date(System.currentTimeMillis()))
                        .status(true)
                        .tokenUser(user.getUsername())
                        .build());
                iAudit.save("ADD_PURCHASE_DOCUMENT","ADD PURCHASE DOCUMENT "+ newShipmentDocument.getName()+".",user.getUsername());
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
            ShipmentDocument shipmentDocument;
            User user;

            try {
                shipmentDocument = shipmentDocumentRepository.findByNameAndStatusTrue(name.toUpperCase());
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(shipmentDocument == null){
                throw new BadRequestExceptions(Constants.ErrorShipmentDocument);
            }

            try {
                shipmentDocument.setStatus(false);
                shipmentDocument.setUpdateDate(new Date(System.currentTimeMillis()));
                shipmentDocument.setTokenUser(user.getUsername());
                shipmentDocumentRepository.save(shipmentDocument);
                iAudit.save("DELETE_PURCHASE_DOCUMENT","DELETE PURCHASE DOCUMENT "+ shipmentDocument.getName()+".",user.getUsername());
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
            ShipmentDocument shipmentDocument;
            User user;

            try {
                shipmentDocument = shipmentDocumentRepository.findByNameAndStatusFalse(name.toUpperCase());
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(shipmentDocument == null){
                throw new BadRequestExceptions(Constants.ErrorShipmentDocument);
            }

            try {
                shipmentDocument.setStatus(true);
                shipmentDocument.setUpdateDate(new Date(System.currentTimeMillis()));
                shipmentDocument.setTokenUser(user.getUsername());
                shipmentDocumentRepository.save(shipmentDocument);
                iAudit.save("ACTIVATE_PURCHASE_DOCUMENT","ACTIVATE PURCHASE DOCUMENT "+ shipmentDocument.getName()+".",user.getUsername());
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
            List<ShipmentDocument> shipmentDocumentList;
            try {
                shipmentDocumentList = shipmentDocumentRepository.findAllByStatusTrue();
            }catch (RuntimeException e){
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if(shipmentDocumentList.isEmpty()){
                return Collections.emptyList();
            }
            return shipmentDocumentList.stream().map(purchaseDocument -> purchaseDocument.getName().toUpperCase()).toList();
        });
    }

    @Override
    public CompletableFuture<List<String>> listFilter() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<ShipmentDocument> shipmentDocumentList;
            try {
                shipmentDocumentList = shipmentDocumentRepository.findAll();
            }catch (RuntimeException e){
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if(shipmentDocumentList.isEmpty()){
                return Collections.emptyList();
            }
            return shipmentDocumentList.stream().map(purchaseDocument -> purchaseDocument.getName().toUpperCase()).toList();
        });
    }
}
