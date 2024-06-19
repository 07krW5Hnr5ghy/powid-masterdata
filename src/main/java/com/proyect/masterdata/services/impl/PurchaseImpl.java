package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.PurchaseDTO;
import com.proyect.masterdata.dto.request.RequestPurchase;
import com.proyect.masterdata.dto.request.RequestPurchaseItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IPurchase;
import com.proyect.masterdata.services.IPurchaseItem;
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
public class PurchaseImpl implements IPurchase {
    private final PurchaseRepository purchaseRepository;
    private final UserRepository userRepository;
    private final IPurchaseItem iPurchaseItem;
    private final PurchaseRepositoryCustom purchaseRepositoryCustom;
    private final PurchaseDocumentRepository purchaseDocumentRepository;
    private final SupplierRepository supplierRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final IAudit iAudit;
    @Override
    public ResponseSuccess save(RequestPurchase requestPurchase) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        Purchase purchase;
        PurchaseDocument purchaseDocument;
        Supplier supplier;

        try {
            user = userRepository.findByUsernameAndStatusTrue(requestPurchase.getTokenUser().toUpperCase());
            purchase = purchaseRepository.findBySerial(requestPurchase.getSerial().toUpperCase());
            purchaseDocument = purchaseDocumentRepository.findByNameAndStatusTrue(requestPurchase.getDocumentName().toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }else{
            supplier = supplierRepository.findByClientIdAndRucAndStatusTrue(user.getClientId(), requestPurchase.getSupplierRuc().toUpperCase());
        }

        if(purchase != null){
            throw new BadRequestExceptions(Constants.ErrorPurchaseExists);
        }

        if(purchaseDocument == null){
            throw new BadRequestExceptions(Constants.ErrorPurchaseDocument);
        }

        try{
            requestPurchase.getPurchaseItemsList().forEach(requestPurchaseItem -> {
                SupplierProduct supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(requestPurchaseItem.getSupplierProductSerial().toUpperCase());
                if(supplierProduct == null){
                    throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
                }
                if(requestPurchaseItem.getQuantity() < 1){
                    throw new BadRequestExceptions(Constants.ErrorPurchaseItemZero);
                }
            });
            Purchase newPurchase = purchaseRepository.save(Purchase.builder()
                            .serial(requestPurchase.getSerial().toUpperCase())
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .status(true)
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .tokenUser(user.getUsername())
                            .purchaseDocument(purchaseDocument)
                            .purchaseDocumentId(purchaseDocument.getId())
                            .supplier(supplier)
                            .supplierId(supplier.getId())
                    .build());
            for(RequestPurchaseItem requestPurchaseItem : requestPurchase.getPurchaseItemsList()){
                iPurchaseItem.save(newPurchase.getId(),requestPurchaseItem,user.getUsername());
            }
            iAudit.save("ADD_PURCHASE","ADD PURCHASE "+newPurchase.getSerial()+".",user.getUsername());
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
    public CompletableFuture<ResponseSuccess> saveAsync(RequestPurchase requestPurchase) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Purchase purchase;
            PurchaseDocument purchaseDocument;
            Supplier supplier;

            try {
                user = userRepository.findByUsernameAndStatusTrue(requestPurchase.getTokenUser().toUpperCase());
                purchase = purchaseRepository.findBySerial(requestPurchase.getSerial().toUpperCase());
                purchaseDocument = purchaseDocumentRepository.findByNameAndStatusTrue(requestPurchase.getDocumentName().toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                supplier = supplierRepository.findByClientIdAndRucAndStatusTrue(user.getClientId(), requestPurchase.getSupplierRuc().toUpperCase());
            }

            if(purchase != null){
                throw new BadRequestExceptions(Constants.ErrorPurchaseExists);
            }

            if(purchaseDocument == null){
                throw new BadRequestExceptions(Constants.ErrorPurchaseDocument);
            }

            try{
                requestPurchase.getPurchaseItemsList().forEach(requestPurchaseItem -> {
                    SupplierProduct supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(requestPurchaseItem.getSupplierProductSerial().toUpperCase());
                    if(supplierProduct == null){
                        throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
                    }
                    if(requestPurchaseItem.getQuantity() < 1){
                        throw new BadRequestExceptions(Constants.ErrorPurchaseItemZero);
                    }
                });
                Purchase newPurchase = purchaseRepository.save(Purchase.builder()
                        .serial(requestPurchase.getSerial().toUpperCase())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .status(true)
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .tokenUser(user.getUsername())
                        .purchaseDocument(purchaseDocument)
                        .purchaseDocumentId(purchaseDocument.getId())
                        .supplier(supplier)
                        .supplierId(supplier.getId())
                        .build());
                for(RequestPurchaseItem requestPurchaseItem : requestPurchase.getPurchaseItemsList()){
                    iPurchaseItem.saveAsync(newPurchase.getId(),requestPurchaseItem,user.getUsername());
                }
                iAudit.save("ADD_PURCHASE","ADD PURCHASE "+newPurchase.getSerial()+".",user.getUsername());
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
    public CompletableFuture<Page<PurchaseDTO>> list(String serial, String user,String documentName, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<Purchase> pagePurchase;
            Long clientId;
            String serialData;
            Long purchaseDocumentId;

            if(serial != null){
                serialData = serial.toUpperCase();
            }else {
                serialData = null;
            }

            if(documentName!=null){
                purchaseDocumentId = purchaseDocumentRepository.findByNameAndStatusTrue(documentName.toUpperCase()).getId();
            }else{
                purchaseDocumentId = null;
            }

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                pagePurchase = purchaseRepositoryCustom.searchForPurchase(clientId,serialData,purchaseDocumentId,sort,sortColumn,pageNumber,pageSize,true);
            }catch (RuntimeException e){
                e.printStackTrace();
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if(pagePurchase.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }

            List<PurchaseDTO> purchaseDTOS = pagePurchase.getContent().stream().map(purchase -> {
                PurchaseDocument purchaseDocument = purchaseDocumentRepository.findById(purchase.getPurchaseDocumentId()).orElse(null);
                return PurchaseDTO.builder()
                        .serial(purchase.getSerial())
                        .registrationDate(purchase.getRegistrationDate())
                        .purchaseDocument(purchaseDocument.getName())
                        .supplier(purchase.getSupplier().getBusinessName())
                        .build();
            }).toList();

            return new PageImpl<>(purchaseDTOS,pagePurchase.getPageable(),pagePurchase.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<PurchaseDTO>> listPurchase(String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Purchase> purchases;
            Long clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                purchases = purchaseRepository.findAllByClientIdAndStatusTrue(clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(purchases.isEmpty()){
                return Collections.emptyList();
            }

            return purchases.stream().map(purchase -> PurchaseDTO.builder()
                    .serial(purchase.getSerial())
                    .registrationDate(purchase.getRegistrationDate())
                    .purchaseDocument(purchase.getPurchaseDocument().getName())
                    .supplier(purchase.getSupplier().getBusinessName())
                    .build()
            ).toList();
        });
    }

    @Override
    public CompletableFuture<List<PurchaseDTO>> listPurchaseFalse(String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Purchase> purchases;
            Long clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                purchases = purchaseRepository.findAllByClientIdAndStatusFalse(clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(purchases.isEmpty()){
                return Collections.emptyList();
            }
            return purchases.stream().map(purchase -> PurchaseDTO.builder()
                    .serial(purchase.getSerial())
                    .registrationDate(purchase.getRegistrationDate())
                    .purchaseDocument(purchase.getPurchaseDocument().getName())
                    .supplier(purchase.getSupplier().getBusinessName())
                    .build()
            ).toList();
        });
    }
}
