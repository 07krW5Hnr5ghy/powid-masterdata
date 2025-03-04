package com.proyect.masterdata.services.impl;

import java.time.OffsetDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.*;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.dto.PurchaseItemDTO;
import com.proyect.masterdata.dto.request.RequestPurchaseItem;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class PurchaseItemImpl implements IPurchaseItem {

    private final UserRepository userRepository;
    private final PurchaseItemRepository purchaseItemRepository;
    private final WarehouseRepository warehouseRepository;
    private final PurchaseItemRepositoryCustom purchaseItemRepositoryCustom;
    private final PurchaseRepository purchaseRepository;
    private final IWarehouseStock iWarehouseStock;
    private final IGeneralStock iGeneralStock;
    private final IAudit iAudit;
    private final IUtil iUtil;
    private final ProductRepository productRepository;
    @Override
    public PurchaseItem save(Purchase purchase, String warehouse, RequestPurchaseItem requestPurchaseItem,
                             String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        Product product;
        PurchaseItem purchaseItem;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            product = productRepository.findByIdAndStatusTrue(requestPurchaseItem.getProductId());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(product == null){
            throw new BadRequestExceptions(Constants.ErrorProduct);
        }else{
            purchaseItem = purchaseItemRepository.findByPurchaseIdAndProductId(purchase.getId(),product.getId());
        }

        if (purchaseItem != null) {
            throw new BadRequestExceptions(Constants.ErrorPurchaseExists);
        }

        try {

            PurchaseItem newPurchaseItem = purchaseItemRepository.save(PurchaseItem.builder()
                            .purchase(purchase)
                            .purchaseId(purchase.getId())
                            .product(product)
                            .productId(product.getId())
                            .status(true)
                            .registrationDate(OffsetDateTime.now())
                            .updateDate(OffsetDateTime.now())
                            .observations(requestPurchaseItem.getObservations().toUpperCase())
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .quantity(requestPurchaseItem.getQuantity())
                            .user(user)
                            .userId(user.getId())
                    .build());
            String finalSku = iUtil.buildProductSku(product);
            iWarehouseStock.in(purchase.getWarehouse(),product, requestPurchaseItem.getQuantity(), user);
            iGeneralStock.in(product, requestPurchaseItem.getQuantity(), user.getUsername());
            iAudit.save(
                    "ADD_PURCHASE_ITEM",
                    "PRODUCTO DE INVENTARIO "+
                            finalSku+
                            " CREADO EN COMPRA.",
                    newPurchaseItem.getPurchase().getRef(),
                    user.getUsername());
            return newPurchaseItem;
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public CompletableFuture<ResponseSuccess> saveAsync(UUID purchaseId, RequestPurchaseItem requestPurchaseItem, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Product product;
            PurchaseItem purchaseItem;
            Purchase purchase;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                product = productRepository.findByIdAndStatusTrue(requestPurchaseItem.getProductId());
                purchase = purchaseRepository.findById(purchaseId).orElse(null);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(product == null){
                throw new BadRequestExceptions(Constants.ErrorProduct);

            if(purchase==null){
                throw new BadRequestExceptions(Constants.ErrorPurchase);
            }else{
                purchaseItem = purchaseItemRepository.findByPurchaseIdAndProductId(purchase.getId(),product.getId());
            }
            
            if (purchaseItem != null) {
                throw new BadRequestExceptions(Constants.ErrorPurchaseItemExists);
            }

            try {

                PurchaseItem newPurchaseItem = purchaseItemRepository.save(PurchaseItem.builder()
                        .purchase(purchase)
                        .purchaseId(purchase.getId())
                        .product(product)
                        .productId(product.getId())
                        .status(true)
                        .registrationDate(OffsetDateTime.now())
                        .updateDate(OffsetDateTime.now())
                        .observations(requestPurchaseItem.getObservations().toUpperCase())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .quantity(requestPurchaseItem.getQuantity())
                        .user(user)
                        .userId(user.getId())
                        .build());
                String finalSku = iUtil.buildProductSku(product);
                iWarehouseStock.in(purchase.getWarehouse(),product, requestPurchaseItem.getQuantity(), user);
                iGeneralStock.in(product, requestPurchaseItem.getQuantity(), user.getUsername());
                iAudit.save(
                        "ADD_PURCHASE_ITEM",
                        "PRODUCTO DE INVENTARIO "+
                                finalSku+" CREADO EN COMPRA.",
                        newPurchaseItem.getPurchase().getRef(),user.getUsername());
                return ResponseSuccess.builder()
                        .message(Constants.register)
                        .code(200)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> delete(UUID purchaseId,UUID productId, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Product product;
            Purchase purchase;
            PurchaseItem purchaseItem;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                purchase = purchaseRepository.findById(productId).orElse(null);
                product = productRepository.findByIdAndStatusTrue(productId);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(purchase==null){
                throw new BadRequestExceptions(Constants.ErrorPurchase);
            }

            if(product == null){
                throw new BadRequestExceptions(Constants.ErrorProduct);
            }else {
                purchaseItem = purchaseItemRepository.findByPurchaseIdAndProductId(purchase.getId(),product.getId());
            }

            if (purchaseItem == null) {
                throw new BadRequestExceptions(Constants.ErrorPurchaseItem);
            }

            try {
                purchaseItem.setStatus(false);
                purchaseItem.setUpdateDate(OffsetDateTime.now());
                purchaseItem.setUser(user);
                purchaseItem.setUserId(user.getId());
                String finalSku = iUtil.buildProductSku(product);
                iWarehouseStock.out(purchase.getWarehouse(),product, purchaseItem.getQuantity(), user);
                iGeneralStock.out(product, purchaseItem.getQuantity(), user.getUsername());
                iAudit.save(
                        "DELETE_PURCHASE_ITEM",
                        "PRODUCTO DE INVENTARIO "+
                                finalSku+
                                " DESACTIVADO EN COMPRA.",
                        purchaseItem.getPurchase().getRef(),user.getUsername());
                return ResponseDelete.builder()
                        .message(Constants.delete)
                        .code(200)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(UUID purchaseId, UUID supplierProductId, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Product product;
            Purchase purchase;
            PurchaseItem purchaseItem;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                purchase = purchaseRepository.findById(purchaseId).orElse(null);
                product = productRepository.findByIdAndStatusTrue(supplierProductId);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(purchase==null){
                throw new BadRequestExceptions(Constants.ErrorPurchase);
            }

            if(product == null){
                throw new BadRequestExceptions(Constants.ErrorProduct);
            }else {
                purchaseItem = purchaseItemRepository.findByPurchaseIdAndProductId(purchase.getId(),product.getId());
            }

            if (purchaseItem == null) {
                throw new BadRequestExceptions(Constants.ErrorPurchaseItem);
            }

            try {
                purchaseItem.setStatus(true);
                purchaseItem.setUpdateDate(OffsetDateTime.now());
                purchaseItem.setUser(user);
                purchaseItem.setUserId(user.getId());
                String finalSku = iUtil.buildProductSku(product);
                iWarehouseStock.in(purchase.getWarehouse(),product, purchaseItem.getQuantity(), user);
                iGeneralStock.in(product, purchaseItem.getQuantity(), user.getUsername());
                iAudit.save(
                        "ACTIVATE_PURCHASE_ITEM",
                        "PRODUCTO DE INVENTARIO "+
                                finalSku+
                                " ACTIVADO EN COMPRA.",
                        purchaseItem.getPurchase().getRef(),user.getUsername());
                return ResponseSuccess.builder()
                        .message(Constants.update)
                        .code(200)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<PurchaseItemDTO>> list(
            String user,
            Long purchaseNumber,
            String warehouse,
            String model,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<PurchaseItem> pagePurchaseItem;
            UUID clientId;

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                pagePurchaseItem = purchaseItemRepositoryCustom.searchForPurchaseItem(
                        clientId,
                        purchaseNumber,
                        warehouse,
                        model,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.ResultsFound);
            }

            if (pagePurchaseItem.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<PurchaseItemDTO> purchaseItemDTOS = pagePurchaseItem.getContent().stream().map(purchaseItem -> PurchaseItemDTO.builder()
                    .purchase(purchaseItem.getPurchase().getPurchaseNumber())
                    .quantity(purchaseItem.getQuantity())
                    .warehouse(purchaseItem.getPurchase().getWarehouse().getName())
                    .model(purchaseItem.getProduct().getModel().getName())
                    .color(purchaseItem.getProduct().getColor().getName())
                    .size(purchaseItem.getProduct().getSize().getName())
                    .registrationDate(purchaseItem.getRegistrationDate())
                    .build()).toList();

            return new PageImpl<>(purchaseItemDTOS, pagePurchaseItem.getPageable(), pagePurchaseItem.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<PurchaseItemDTO>> listPurchaseItem(String user,UUID id) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<PurchaseItem> purchaseItems;
            UUID clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                if(id != null){
                    purchaseItems = purchaseItemRepository.findAllByClientIdAndPurchaseId(clientId,id);
                }else{
                    purchaseItems = purchaseItemRepository.findAllByClientId(clientId);
                }
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (purchaseItems.isEmpty()){
                return Collections.emptyList();
            }

            return purchaseItems.stream().map(purchaseItem -> PurchaseItemDTO.builder()
                    .purchase(purchaseItem.getPurchase().getPurchaseNumber())
                    .quantity(purchaseItem.getQuantity())
                    .warehouse(purchaseItem.getPurchase().getWarehouse().getName())
                    .model(purchaseItem.getProduct().getModel().getName())
                    .color(purchaseItem.getProduct().getColor().getName())
                    .size(purchaseItem.getProduct().getSize().getName())
                    .registrationDate(purchaseItem.getRegistrationDate())
                    .build()).toList();
        });
    }

}
