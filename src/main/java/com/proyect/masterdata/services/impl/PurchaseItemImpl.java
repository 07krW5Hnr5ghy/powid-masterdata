package com.proyect.masterdata.services.impl;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.domain.Purchase;
import com.proyect.masterdata.domain.PurchaseItem;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IAudit;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.SupplierProduct;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.PurchaseItemDTO;
import com.proyect.masterdata.dto.request.RequestPurchaseItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.IPurchaseItem;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class PurchaseItemImpl implements IPurchaseItem {
    private final UserRepository userRepository;
    private final PurchaseRepository purchaseRepository;
    private final PurchaseItemRepository purchaseItemRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final PurchaseItemRepositoryCustom purchaseItemRepositoryCustom;
    private final IAudit iAudit;
    @Override
    public ResponseSuccess save(Long purchaseId, RequestPurchaseItem requestPurchaseItem, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        Purchase purchase;
        SupplierProduct supplierProduct;
        PurchaseItem purchaseItem;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            purchase = purchaseRepository.findById(purchaseId).orElse(null);
            supplierProduct = supplierProductRepository
                    .findBySerialAndStatusTrue(requestPurchaseItem.getSupplierProductSerial().toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (supplierProduct == null) {
            throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
        }

        if(purchase == null){
            throw new BadRequestExceptions(Constants.ErrorPurchase);
        }else{
            purchaseItem = purchaseItemRepository.findByPurchaseIdAndSupplierProductId(purchase.getId(),
                    supplierProduct.getId());
        }

        if (purchaseItem != null) {
            throw new BadRequestExceptions(Constants.ErrorPurchaseExists);
        }

        try {
            PurchaseItem newPurchaseItem = purchaseItemRepository.save(PurchaseItem.builder()
                    .client(user.getClient())
                    .clientId(user.getClientId())
                    .quantity(requestPurchaseItem.getQuantity())
                    .registrationDate(new Date(System.currentTimeMillis()))
                    .purchase(purchase)
                    .purchaseId(purchase.getId())
                    .status(true)
                    .supplierProduct(supplierProduct)
                    .supplierProductId(supplierProduct.getId())
                    .tokenUser(user.getUsername())
                    .build());
            iAudit.save("ADD_PURCHASE_ITEM","ADD PURCHASE ITEM "+newPurchaseItem.getSupplierProduct().getSerial()+" FOR PURCHASE "+newPurchaseItem.getPurchase().getSerial()+".",user.getUsername());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e) {
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public CompletableFuture<ResponseSuccess> saveAsync(Long purchaseId, RequestPurchaseItem requestPurchaseItem, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Purchase purchase;
            SupplierProduct supplierProduct;
            PurchaseItem purchaseItem;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                purchase = purchaseRepository.findById(purchaseId).orElse(null);
                supplierProduct = supplierProductRepository
                        .findBySerialAndStatusTrue(requestPurchaseItem.getSupplierProductSerial().toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (supplierProduct == null) {
                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
            }

            if(purchase == null){
                throw new BadRequestExceptions(Constants.ErrorPurchase);
            }else{
                purchaseItem = purchaseItemRepository.findByPurchaseIdAndSupplierProductId(purchase.getId(),
                        supplierProduct.getId());
            }

            if (purchaseItem != null) {
                throw new BadRequestExceptions(Constants.ErrorPurchaseExists);
            }

            try {

                PurchaseItem newPurchaseItem = purchaseItemRepository.save(PurchaseItem.builder()
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .quantity(requestPurchaseItem.getQuantity())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .purchase(purchase)
                        .purchaseId(purchase.getId())
                        .status(true)
                        .supplierProduct(supplierProduct)
                        .supplierProductId(supplierProduct.getId())
                        .tokenUser(user.getUsername())
                        .build());
                iAudit.save("ADD_PURCHASE_ITEM","ADD PURCHASE ITEM "+newPurchaseItem.getSupplierProduct().getSerial()+" FOR PURCHASE "+newPurchaseItem.getPurchase().getSerial()+".",user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            } catch (RuntimeException e) {
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<PurchaseItemDTO>> list(String serial, String user, String supplierProductSerial, String sort, String sortColumn,
                                      Integer pageNumber, Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<PurchaseItem> pagePurchase;
            Long clientId;
            Long supplierProductId;
            Long purchaseId;

            if(serial != null){
                purchaseId = purchaseRepository.findBySerial(serial.toUpperCase()).getId();
            }else {
                purchaseId = null;
            }

            if(supplierProductSerial != null){
                supplierProductId = supplierProductRepository.findBySerial(supplierProductSerial.toUpperCase()).getId();
            }else {
                supplierProductId = null;
            }

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                pagePurchase = purchaseItemRepositoryCustom.searchForPurchaseItem(clientId, purchaseId,supplierProductId, sort, sortColumn,
                        pageNumber, pageSize, true);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.ResultsFound);
            }

            if (pagePurchase.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<PurchaseItemDTO> purchaseItemDTOS = pagePurchase.getContent().stream().map(purchaseItem -> PurchaseItemDTO.builder()
                    .registrationDate(purchaseItem.getRegistrationDate())
                    .quantity(purchaseItem.getQuantity())
                    .serial(purchaseItem.getPurchase().getSerial())
                    .supplierProductSerial(purchaseItem.getSupplierProduct().getSerial())
                    .unitPrice(purchaseItem.getSupplierProduct().getPurchasePrice())
                    .id(purchaseItem.getId())
                    .build()).toList();

            return new PageImpl<>(purchaseItemDTOS, pagePurchase.getPageable(), pagePurchase.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> delete(String purchaseSerial, String serialSupplierProduct, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            SupplierProduct supplierProduct;
            Purchase purchase;
            PurchaseItem purchaseItem;

            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                purchase = purchaseRepository.findBySerial(purchaseSerial.toUpperCase());
                supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(serialSupplierProduct.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(purchase == null){
                throw new BadRequestExceptions(Constants.ErrorPurchase);
            }

            if(supplierProduct == null){
                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
            }else {
                purchaseItem = purchaseItemRepository.findByPurchaseIdAndSupplierProductIdAndStatusTrue(purchase.getId(),supplierProduct.getId());
            }

            try{
                purchaseItem.setStatus(false);
                purchaseItem.setUpdateDate(new Date(System.currentTimeMillis()));
                purchaseItem.setTokenUser(user.getUsername());
                purchaseItemRepository.save(purchaseItem);
                iAudit.save("DELETE_PURCHASE_ITEM","DELETE PURCHASE ITEM "+purchaseItem.getSupplierProduct().getSerial()+" FOR PURCHASE "+purchaseItem.getPurchase().getSerial()+".",user.getUsername());
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
    public CompletableFuture<ResponseSuccess> activate(String purchaseSerial, String serialSupplierProduct, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            SupplierProduct supplierProduct;
            Purchase purchase;
            PurchaseItem purchaseItem;

            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                purchase = purchaseRepository.findBySerial(purchaseSerial.toUpperCase());
                supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(serialSupplierProduct.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(purchase == null){
                throw new BadRequestExceptions(Constants.ErrorPurchase);
            }

            if(supplierProduct == null){
                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
            }else{
                purchaseItem = purchaseItemRepository.findByPurchaseIdAndSupplierProductIdAndStatusFalse(purchase.getId(),supplierProduct.getId());
            }


            try{
                purchaseItem.setStatus(true);
                purchaseItem.setUpdateDate(new Date(System.currentTimeMillis()));
                purchaseItem.setTokenUser(user.getUsername());
                purchaseItemRepository.save(purchaseItem);
                iAudit.save("ACTIVATE_PURCHASE_ITEM","ACTIVATE PURCHASE ITEM "+purchaseItem.getSupplierProduct().getSerial()+" FOR PURCHASE "+purchaseItem.getPurchase().getSerial()+".",user.getUsername());
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
    public CompletableFuture<List<PurchaseItemDTO>> listPurchaseItem(String user,Long id) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<PurchaseItem> purchaseItems;
            Long clientId;
            try{
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                if(id != null){
                    purchaseItems = purchaseItemRepository.findAllByClientIdAndPurchaseIdAndStatusTrue(clientId,id);
                }else{
                    purchaseItems = purchaseItemRepository.findAllByClientIdAndStatusTrue(clientId);
                }
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(purchaseItems.isEmpty()){
                return Collections.emptyList();
            }

            return purchaseItems.stream().map(purchaseItem -> PurchaseItemDTO.builder()
                    .registrationDate(purchaseItem.getRegistrationDate())
                    .quantity(purchaseItem.getQuantity())
                    .serial(purchaseItem.getPurchase().getSerial())
                    .supplierProductSerial(purchaseItem.getSupplierProduct().getSerial())
                    .supplier(purchaseItem.getSupplierProduct().getSupplier().getBusinessName())
                    .unitPrice(purchaseItem.getSupplierProduct().getPurchasePrice())
                    .id(purchaseItem.getId())
                    .build()).toList();
        });
    }

    @Override
    public CompletableFuture<List<PurchaseItemDTO>> listPurchaseItemFalse(String user,Long id) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<PurchaseItem> purchaseItems;
            Long clientId;
            try{
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                if(id != null){
                    purchaseItems = purchaseItemRepository.findAllByClientIdAndPurchaseIdAndStatusFalse(clientId,id);
                }else{
                    purchaseItems = purchaseItemRepository.findAllByClientIdAndStatusFalse(clientId);
                }
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(purchaseItems.isEmpty()){
                return Collections.emptyList();
            }

            return purchaseItems.stream().map(purchaseItem -> PurchaseItemDTO.builder()
                    .registrationDate(purchaseItem.getRegistrationDate())
                    .quantity(purchaseItem.getQuantity())
                    .serial(purchaseItem.getPurchase().getSerial())
                    .supplierProductSerial(purchaseItem.getSupplierProduct().getSerial())
                    .supplier(purchaseItem.getSupplierProduct().getSupplier().getBusinessName())
                    .unitPrice(purchaseItem.getSupplierProduct().getPurchasePrice())
                    .id(purchaseItem.getId())
                    .build()).toList();
        });
    }

}
