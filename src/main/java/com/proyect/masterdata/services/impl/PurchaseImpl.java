package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Purchase;
import com.proyect.masterdata.domain.PurchaseDocument;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.PurchaseDTO;
import com.proyect.masterdata.dto.request.RequestPurchaseItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.PurchaseDocumentRepository;
import com.proyect.masterdata.repository.PurchaseRepository;
import com.proyect.masterdata.repository.PurchaseRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
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

@Service
@RequiredArgsConstructor
@Log4j2
public class PurchaseImpl implements IPurchase {
    private final PurchaseRepository purchaseRepository;
    private final UserRepository userRepository;
    private final IPurchaseItem iPurchaseItem;
    private final PurchaseRepositoryCustom purchaseRepositoryCustom;
    private final PurchaseDocumentRepository purchaseDocumentRepository;
    @Override
    public ResponseSuccess save(String serial,String documentName, List<RequestPurchaseItem> requestPurchaseItemList, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        Purchase purchase;
        PurchaseDocument purchaseDocument;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            purchase = purchaseRepository.findBySerial(serial.toUpperCase());
            purchaseDocument = purchaseDocumentRepository.findByNameAndStatusTrue(documentName.toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(purchase != null){
            throw new BadRequestExceptions(Constants.ErrorPurchaseExists);
        }

        if(purchaseDocument == null){
            throw new BadRequestExceptions(Constants.ErrorPurchaseDocument);
        }

        try{
            Purchase newPurchase = purchaseRepository.save(Purchase.builder()
                            .serial(serial.toUpperCase())
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .status(true)
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .tokenUser(user.getUsername())
                            .purchaseDocument(purchaseDocument)
                            .purchaseDocumentId(purchaseDocument.getId())
                    .build());
            for(RequestPurchaseItem requestPurchaseItem : requestPurchaseItemList){
                iPurchaseItem.save(newPurchase.getId(),requestPurchaseItem,user.getUsername());
            }
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
    public Page<PurchaseDTO> list(String serial, String user,String documentName, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions {
        Page<Purchase> pagePurchase;
        Long clientId;
        String serialData;
        Long purchaseDocumentId;

        if(serial != null){
            serialData = serial.toUpperCase();
        }else {
            serialData = null;
        }

        try {
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
            purchaseDocumentId = purchaseDocumentRepository.findByNameAndStatusTrue(documentName.toUpperCase()).getId();
            pagePurchase = purchaseRepositoryCustom.searchForPurchase(clientId,serialData,purchaseDocumentId,sort,sortColumn,pageNumber,pageSize,true);
        }catch (RuntimeException e){
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
                .build();
        }).toList();

        return new PageImpl<>(purchaseDTOS,pagePurchase.getPageable(),pagePurchase.getTotalElements());
    }

    @Override
    public List<PurchaseDTO> listPurchase(String user) throws BadRequestExceptions, InternalErrorExceptions {
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

        return purchases.stream().map(purchase -> {
            PurchaseDocument purchaseDocument = purchaseDocumentRepository.findById(purchase.getPurchaseDocumentId()).orElse(null);
            return PurchaseDTO.builder()
                    .serial(purchase.getSerial())
                    .registrationDate(purchase.getRegistrationDate())
                    .purchaseDocument(purchaseDocument.getName())
                    .build();
        }).toList();
    }

    @Override
    public List<PurchaseDTO> listPurchaseFalse(String user) throws BadRequestExceptions, InternalErrorExceptions {
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
        return purchases.stream().map(purchase -> {
            PurchaseDocument purchaseDocument = purchaseDocumentRepository.findById(purchase.getPurchaseDocumentId()).orElse(null);
            return PurchaseDTO.builder()
                    .serial(purchase.getSerial())
                    .registrationDate(purchase.getRegistrationDate())
                    .purchaseDocument(purchaseDocument.getName())
                    .build();
        }).toList();
    }
}
