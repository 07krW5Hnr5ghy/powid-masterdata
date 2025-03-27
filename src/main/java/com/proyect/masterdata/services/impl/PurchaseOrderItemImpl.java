package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Product;
import com.proyect.masterdata.domain.PurchaseOrder;
import com.proyect.masterdata.domain.PurchaseOrderItem;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.PurchaseOrderItemDTO;
import com.proyect.masterdata.dto.request.RequestPurchaseOrderItem;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.*;
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
public class PurchaseOrderItemImpl implements IPurchaseOrderItem {
    private final UserRepository userRepository;
    private final PurchaseOrderItemRepository purchaseOrderItemRepository;
    private final WarehouseRepository warehouseRepository;
    private final PurchaseOrderItemRepositoryCustom purchaseOrderItemRepositoryCustom;
    private final PurchaseOrderRepository purchaseOrderRepository;
    private final IWarehouseStock iWarehouseStock;
    private final IGeneralStock iGeneralStock;
    private final IAudit iAudit;
    private final IUtil iUtil;
    private final ProductRepository productRepository;
    private final ProductPriceRepository productPriceRepository;
    @Override
    public PurchaseOrderItem save(PurchaseOrder purchaseOrder,RequestPurchaseOrderItem requestPurchaseOrderItem,
                                String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        Product product;
        PurchaseOrderItem purchaseOrderItem;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            product = productRepository.findByIdAndStatusTrue(requestPurchaseOrderItem.getProductId());
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
            purchaseOrderItem = purchaseOrderItemRepository.findByPurchaseOrderIdAndProductId(purchaseOrder.getId(),product.getId());
        }

        if (purchaseOrderItem != null) {
            throw new BadRequestExceptions(Constants.ErrorPurchaseExists);
        }

        try {

            PurchaseOrderItem newPurchaseOrderItem = purchaseOrderItemRepository.save(PurchaseOrderItem.builder()
                    .purchaseOrder(purchaseOrder)
                    .purchaseOrderId(purchaseOrder.getId())
                    .product(product)
                    .productId(product.getId())
                    .status(true)
                    .registrationDate(OffsetDateTime.now())
                    .updateDate(OffsetDateTime.now())
                    .observations(requestPurchaseOrderItem.getObservations().toUpperCase())
                    .client(user.getClient())
                    .clientId(user.getClientId())
                    .quantity(requestPurchaseOrderItem.getQuantity())
                    .user(user)
                    .userId(user.getId())
                    .build());
            String finalSku = iUtil.buildProductSku(product);
            iAudit.save(
                    "ADD_PURCHASE_ORDER_ITEM",
                    "PRODUCTO DE INVENTARIO "+
                            finalSku+
                            " CREADO EN ORDEN DE COMPRA.",
                    newPurchaseOrderItem.getPurchaseOrder().getRef(),
                    user.getUsername());
            return newPurchaseOrderItem;
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public CompletableFuture<ResponseSuccess> saveAsync(UUID purchaseId, RequestPurchaseOrderItem requestPurchaseOrderItem, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Product product;
            PurchaseOrderItem purchaseOrderItem;
            PurchaseOrder purchaseOrder;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                product = productRepository.findByIdAndStatusTrue(requestPurchaseOrderItem.getProductId());
                purchaseOrder = purchaseOrderRepository.findById(purchaseId).orElse(null);
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

            if(purchaseOrder ==null){
                throw new BadRequestExceptions(Constants.ErrorPurchase);
            }else{
                purchaseOrderItem = purchaseOrderItemRepository.findByPurchaseOrderIdAndProductId(purchaseOrder.getId(),product.getId());
            }

            if (purchaseOrderItem != null) {
                throw new BadRequestExceptions(Constants.ErrorPurchaseItemExists);
            }

            try {

                PurchaseOrderItem newPurchaseOrderItem = purchaseOrderItemRepository.save(PurchaseOrderItem.builder()
                        .purchaseOrder(purchaseOrder)
                        .purchaseOrderId(purchaseOrder.getId())
                        .product(product)
                        .productId(product.getId())
                        .status(true)
                        .registrationDate(OffsetDateTime.now())
                        .updateDate(OffsetDateTime.now())
                        .observations(requestPurchaseOrderItem.getObservations().toUpperCase())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .quantity(requestPurchaseOrderItem.getQuantity())
                        .user(user)
                        .userId(user.getId())
                        .build());
                String finalSku = iUtil.buildProductSku(product);
                iAudit.save(
                        "ADD_PURCHASE_ORDER_ITEM",
                        "PRODUCTO DE INVENTARIO "+
                                finalSku+" CREADO EN ORDEN DE COMPRA.",
                        newPurchaseOrderItem.getPurchaseOrder().getRef(),user.getUsername());
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
    public CompletableFuture<ResponseDelete> delete(UUID purchaseId, UUID productId, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Product product;
            PurchaseOrder purchaseOrder;
            PurchaseOrderItem purchaseOrderItem;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                purchaseOrder = purchaseOrderRepository.findById(productId).orElse(null);
                product = productRepository.findByIdAndStatusTrue(productId);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(purchaseOrder==null){
                throw new BadRequestExceptions(Constants.ErrorPurchase);
            }

            if(product == null){
                throw new BadRequestExceptions(Constants.ErrorProduct);
            }else {
                purchaseOrderItem = purchaseOrderItemRepository.findByPurchaseOrderIdAndProductId(purchaseOrder.getId(),product.getId());
            }

            if(!purchaseOrder.getStatus()){
                throw new BadRequestExceptions(Constants.ErrorPurchaseOrderInactive);
            }

            if (purchaseOrderItem == null) {
                throw new BadRequestExceptions(Constants.ErrorPurchaseItem);
            }

            try {
                purchaseOrderItem.setStatus(false);
                purchaseOrderItem.setUpdateDate(OffsetDateTime.now());
                purchaseOrderItem.setUser(user);
                purchaseOrderItem.setUserId(user.getId());
                String finalSku = iUtil.buildProductSku(product);
                purchaseOrderItemRepository.save(purchaseOrderItem);
                iAudit.save(
                        "DELETE_PURCHASE_ORDER_ITEM",
                        "PRODUCTO DE INVENTARIO "+
                                finalSku+
                                " DESACTIVADO EN ORDEN DE COMPRA.",
                        purchaseOrderItem.getPurchaseOrder().getRef(),user.getUsername());
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
            PurchaseOrder purchaseOrder;
            PurchaseOrderItem purchaseOrderItem;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                purchaseOrder = purchaseOrderRepository.findById(purchaseId).orElse(null);
                product = productRepository.findByIdAndStatusTrue(supplierProductId);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(purchaseOrder ==null){
                throw new BadRequestExceptions(Constants.ErrorPurchase);
            }

            if(product == null){
                throw new BadRequestExceptions(Constants.ErrorProduct);
            }else {
                purchaseOrderItem = purchaseOrderItemRepository.findByPurchaseOrderIdAndProductId(purchaseOrder.getId(),product.getId());
            }

            if (purchaseOrderItem == null) {
                throw new BadRequestExceptions(Constants.ErrorPurchaseItem);
            }

            if(!purchaseOrder.getStatus()){
                throw new BadRequestExceptions(Constants.ErrorPurchaseOrderInactive);
            }

            try {
                purchaseOrderItem.setStatus(true);
                purchaseOrderItem.setUpdateDate(OffsetDateTime.now());
                purchaseOrderItem.setUser(user);
                purchaseOrderItem.setUserId(user.getId());
                String finalSku = iUtil.buildProductSku(product);
                purchaseOrderItemRepository.save(purchaseOrderItem);
                iAudit.save(
                        "ACTIVATE_PURCHASE_ORDER_ITEM",
                        "PRODUCTO DE INVENTARIO "+
                                finalSku+
                                " ACTIVADO EN ORDEN DE COMPRA.",
                        purchaseOrderItem.getPurchaseOrder().getRef(),user.getUsername());
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
    public CompletableFuture<Page<PurchaseOrderItemDTO>> list(
            String user,
            Long orderNumber,
            String ref,
            Integer quantity,
            String model,
            String product,
            String color,
            String size,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<PurchaseOrderItem> pagePurchaseItem;
            UUID clientId;

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                pagePurchaseItem = purchaseOrderItemRepositoryCustom.searchForPurchaseOrderItem(
                        clientId,
                        orderNumber,
                        ref,
                        quantity,
                        model,
                        product,
                        color,
                        size,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateEndDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        status);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.ResultsFound);
            }

            if (pagePurchaseItem.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<PurchaseOrderItemDTO> purchaseOrderItemDTOS = pagePurchaseItem.getContent().stream().map(purchaseOrderItem -> PurchaseOrderItemDTO.builder()
                    .id(purchaseOrderItem.getId())
                    .ref(purchaseOrderItem.getPurchaseOrder().getRef())
                    .productId(purchaseOrderItem.getProductId())
                    .product(purchaseOrderItem.getProduct().getName())
                    .productSku(iUtil.buildProductSku(purchaseOrderItem.getProduct()))
                    .orderNumber(purchaseOrderItem.getPurchaseOrder().getOrderNumber())
                    .quantity(purchaseOrderItem.getQuantity())
                    .model(purchaseOrderItem.getProduct().getModel().getName())
                    .color(purchaseOrderItem.getProduct().getColor().getName())
                    .size(purchaseOrderItem.getProduct().getSize().getName())
                    .registrationDate(purchaseOrderItem.getRegistrationDate())
                    .updateDate(purchaseOrderItem.getUpdateDate())
                    .user(purchaseOrderItem.getUser().getUsername())
                    .status(purchaseOrderItem.getStatus())
                    .observations(purchaseOrderItem.getObservations())
                    .unitPrice(productPriceRepository.findByProductIdAndStatusTrue(purchaseOrderItem.getProductId()).getUnitSalePrice())
                    .build()).toList();

            return new PageImpl<>(purchaseOrderItemDTOS, pagePurchaseItem.getPageable(), pagePurchaseItem.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<PurchaseOrderItemDTO>> listPurchaseOrderItem(String user, UUID id) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<PurchaseOrderItem> purchaseOrderItems;
            UUID clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                if(id != null){
                    purchaseOrderItems = purchaseOrderItemRepository.findAllByClientIdAndPurchaseOrderId(clientId,id);
                }else{
                    purchaseOrderItems = purchaseOrderItemRepository.findAllByClientId(clientId);
                }
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (purchaseOrderItems.isEmpty()){
                return Collections.emptyList();
            }

            return purchaseOrderItems.stream().map(purchaseOrderItem -> PurchaseOrderItemDTO.builder()
                    .id(purchaseOrderItem.getId())
                    .ref(purchaseOrderItem.getPurchaseOrder().getRef())
                    .productId(purchaseOrderItem.getProductId())
                    .product(purchaseOrderItem.getProduct().getName())
                    .productSku(iUtil.buildProductSku(purchaseOrderItem.getProduct()))
                    .orderNumber(purchaseOrderItem.getPurchaseOrder().getOrderNumber())
                    .quantity(purchaseOrderItem.getQuantity())
                    .model(purchaseOrderItem.getProduct().getModel().getName())
                    .color(purchaseOrderItem.getProduct().getColor().getName())
                    .size(purchaseOrderItem.getProduct().getSize().getName())
                    .registrationDate(purchaseOrderItem.getRegistrationDate())
                    .updateDate(purchaseOrderItem.getUpdateDate())
                    .user(purchaseOrderItem.getUser().getUsername())
                    .status(purchaseOrderItem.getStatus())
                    .observations(purchaseOrderItem.getObservations())
                    .unitPrice(productPriceRepository.findByProductIdAndStatusTrue(purchaseOrderItem.getProductId()).getUnitSalePrice())
                    .build()).toList();
        });
    }
}
