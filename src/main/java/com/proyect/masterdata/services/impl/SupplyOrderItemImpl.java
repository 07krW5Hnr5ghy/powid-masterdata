package com.proyect.masterdata.services.impl;

import java.time.OffsetDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.request.RequestSupplyOrderItem;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.*;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.dto.SupplyOrderItemDTO;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class SupplyOrderItemImpl implements ISupplyOrderItem {

    private final UserRepository userRepository;
    private final SupplyOrderItemRepository supplyOrderItemRepository;
    private final WarehouseRepository warehouseRepository;
    private final SupplyOrderItemRepositoryCustom supplyOrderItemRepositoryCustom;
    private final SupplyOrderRepository supplyOrderRepository;
    private final IWarehouseStock iWarehouseStock;
    private final IGeneralStock iGeneralStock;
    private final IAudit iAudit;
    private final IUtil iUtil;
    private final ProductRepository productRepository;
    @Override
    public SupplyOrderItem save(SupplyOrder supplyOrder, String warehouse, RequestSupplyOrderItem requestSupplyOrderItem,
                                String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        Product product;
        SupplyOrderItem supplyOrderItem;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            product = productRepository.findByIdAndStatusTrue(requestSupplyOrderItem.getProductId());
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
            supplyOrderItem = supplyOrderItemRepository.findByPurchaseIdAndProductId(supplyOrder.getId(),product.getId());
        }

        if (supplyOrderItem != null) {
            throw new BadRequestExceptions(Constants.ErrorPurchaseExists);
        }

        try {

            SupplyOrderItem newSupplyOrderItem = supplyOrderItemRepository.save(SupplyOrderItem.builder()
                            .supplyOrder(supplyOrder)
                            .purchaseId(supplyOrder.getId())
                            .product(product)
                            .productId(product.getId())
                            .status(true)
                            .registrationDate(OffsetDateTime.now())
                            .updateDate(OffsetDateTime.now())
                            .observations(requestSupplyOrderItem.getObservations().toUpperCase())
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .quantity(requestSupplyOrderItem.getQuantity())
                            .user(user)
                            .userId(user.getId())
                    .build());
            String finalSku = iUtil.buildProductSku(product);
            iWarehouseStock.in(supplyOrder.getWarehouse(),product, requestSupplyOrderItem.getQuantity(), user);
            iGeneralStock.in(product, requestSupplyOrderItem.getQuantity(), user.getUsername());
            iAudit.save(
                    "ADD_PURCHASE_ITEM",
                    "PRODUCTO DE INVENTARIO "+
                            finalSku+
                            " CREADO EN COMPRA.",
                    newSupplyOrderItem.getSupplyOrder().getRef(),
                    user.getUsername());
            return newSupplyOrderItem;
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public CompletableFuture<ResponseSuccess> saveAsync(UUID purchaseId, RequestSupplyOrderItem requestSupplyOrderItem, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Product product;
            SupplyOrderItem supplyOrderItem;
            SupplyOrder supplyOrder;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                product = productRepository.findByIdAndStatusTrue(requestSupplyOrderItem.getProductId());
                supplyOrder = supplyOrderRepository.findById(purchaseId).orElse(null);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(product == null) {
                throw new BadRequestExceptions(Constants.ErrorProduct);
            }

            if(supplyOrder ==null){
                throw new BadRequestExceptions(Constants.ErrorPurchase);
            }else{
                supplyOrderItem = supplyOrderItemRepository.findByPurchaseIdAndProductId(supplyOrder.getId(),product.getId());
            }
            
            if (supplyOrderItem != null) {
                throw new BadRequestExceptions(Constants.ErrorPurchaseItemExists);
            }

            try {

                SupplyOrderItem newSupplyOrderItem = supplyOrderItemRepository.save(SupplyOrderItem.builder()
                        .supplyOrder(supplyOrder)
                        .purchaseId(supplyOrder.getId())
                        .product(product)
                        .productId(product.getId())
                        .status(true)
                        .registrationDate(OffsetDateTime.now())
                        .updateDate(OffsetDateTime.now())
                        .observations(requestSupplyOrderItem.getObservations().toUpperCase())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .quantity(requestSupplyOrderItem.getQuantity())
                        .user(user)
                        .userId(user.getId())
                        .build());
                String finalSku = iUtil.buildProductSku(product);
                iWarehouseStock.in(supplyOrder.getWarehouse(),product, requestSupplyOrderItem.getQuantity(), user);
                iGeneralStock.in(product, requestSupplyOrderItem.getQuantity(), user.getUsername());
                iAudit.save(
                        "ADD_PURCHASE_ITEM",
                        "PRODUCTO DE INVENTARIO "+
                                finalSku+" CREADO EN COMPRA.",
                        newSupplyOrderItem.getSupplyOrder().getRef(),user.getUsername());
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
            SupplyOrder supplyOrder;
            SupplyOrderItem supplyOrderItem;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                supplyOrder = supplyOrderRepository.findById(productId).orElse(null);
                product = productRepository.findByIdAndStatusTrue(productId);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(supplyOrder ==null){
                throw new BadRequestExceptions(Constants.ErrorPurchase);
            }

            if(product == null){
                throw new BadRequestExceptions(Constants.ErrorProduct);
            }else {
                supplyOrderItem = supplyOrderItemRepository.findByPurchaseIdAndProductId(supplyOrder.getId(),product.getId());
            }

            if (supplyOrderItem == null) {
                throw new BadRequestExceptions(Constants.ErrorPurchaseItem);
            }

            try {
                supplyOrderItem.setStatus(false);
                supplyOrderItem.setUpdateDate(OffsetDateTime.now());
                supplyOrderItem.setUser(user);
                supplyOrderItem.setUserId(user.getId());
                String finalSku = iUtil.buildProductSku(product);
                iWarehouseStock.out(supplyOrder.getWarehouse(),product, supplyOrderItem.getQuantity(), user);
                iGeneralStock.out(product, supplyOrderItem.getQuantity(), user.getUsername());
                iAudit.save(
                        "DELETE_PURCHASE_ITEM",
                        "PRODUCTO DE INVENTARIO "+
                                finalSku+
                                " DESACTIVADO EN COMPRA.",
                        supplyOrderItem.getSupplyOrder().getRef(),user.getUsername());
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
            SupplyOrder supplyOrder;
            SupplyOrderItem supplyOrderItem;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                supplyOrder = supplyOrderRepository.findById(purchaseId).orElse(null);
                product = productRepository.findByIdAndStatusTrue(supplierProductId);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(supplyOrder ==null){
                throw new BadRequestExceptions(Constants.ErrorPurchase);
            }

            if(product == null){
                throw new BadRequestExceptions(Constants.ErrorProduct);
            }else {
                supplyOrderItem = supplyOrderItemRepository.findByPurchaseIdAndProductId(supplyOrder.getId(),product.getId());
            }

            if (supplyOrderItem == null) {
                throw new BadRequestExceptions(Constants.ErrorPurchaseItem);
            }

            try {
                supplyOrderItem.setStatus(true);
                supplyOrderItem.setUpdateDate(OffsetDateTime.now());
                supplyOrderItem.setUser(user);
                supplyOrderItem.setUserId(user.getId());
                String finalSku = iUtil.buildProductSku(product);
                iWarehouseStock.in(supplyOrder.getWarehouse(),product, supplyOrderItem.getQuantity(), user);
                iGeneralStock.in(product, supplyOrderItem.getQuantity(), user.getUsername());
                iAudit.save(
                        "ACTIVATE_PURCHASE_ITEM",
                        "PRODUCTO DE INVENTARIO "+
                                finalSku+
                                " ACTIVADO EN COMPRA.",
                        supplyOrderItem.getSupplyOrder().getRef(),user.getUsername());
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
    public CompletableFuture<Page<SupplyOrderItemDTO>> list(
            String user,
            Long purchaseNumber,
            String warehouse,
            String model,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<SupplyOrderItem> pagePurchaseItem;
            UUID clientId;

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                pagePurchaseItem = supplyOrderItemRepositoryCustom.searchForPurchaseItem(
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

            List<SupplyOrderItemDTO> supplyOrderItemDTOS = pagePurchaseItem.getContent().stream().map(purchaseItem -> SupplyOrderItemDTO.builder()
                    .purchase(purchaseItem.getSupplyOrder().getOrderNumber())
                    .quantity(purchaseItem.getQuantity())
                    .warehouse(purchaseItem.getSupplyOrder().getWarehouse().getName())
                    .model(purchaseItem.getProduct().getModel().getName())
                    .color(purchaseItem.getProduct().getColor().getName())
                    .size(purchaseItem.getProduct().getSize().getName())
                    .registrationDate(purchaseItem.getRegistrationDate())
                    .build()).toList();

            return new PageImpl<>(supplyOrderItemDTOS, pagePurchaseItem.getPageable(), pagePurchaseItem.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<SupplyOrderItemDTO>> listPurchaseItem(String user, UUID id) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<SupplyOrderItem> supplyOrderItems;
            UUID clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                if(id != null){
                    supplyOrderItems = supplyOrderItemRepository.findAllByClientIdAndPurchaseId(clientId,id);
                }else{
                    supplyOrderItems = supplyOrderItemRepository.findAllByClientId(clientId);
                }
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (supplyOrderItems.isEmpty()){
                return Collections.emptyList();
            }

            return supplyOrderItems.stream().map(purchaseItem -> SupplyOrderItemDTO.builder()
                    .purchase(purchaseItem.getSupplyOrder().getOrderNumber())
                    .quantity(purchaseItem.getQuantity())
                    .warehouse(purchaseItem.getSupplyOrder().getWarehouse().getName())
                    .model(purchaseItem.getProduct().getModel().getName())
                    .color(purchaseItem.getProduct().getColor().getName())
                    .size(purchaseItem.getProduct().getSize().getName())
                    .registrationDate(purchaseItem.getRegistrationDate())
                    .build()).toList();
        });
    }

}
