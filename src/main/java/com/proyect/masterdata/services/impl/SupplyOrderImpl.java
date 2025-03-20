package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.CheckStockDTO;
import com.proyect.masterdata.dto.SupplyOrderDTO;
import com.proyect.masterdata.dto.SupplyOrderItemDTO;
import com.proyect.masterdata.dto.request.RequestSupplyOrder;
import com.proyect.masterdata.dto.request.RequestSupplyOrderItem;
import com.proyect.masterdata.dto.request.RequestStockTransactionItem;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.*;
import com.proyect.masterdata.utils.Constants;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.*;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class SupplyOrderImpl implements ISupplyOrder {

    private final UserRepository userRepository;
    private final SupplyOrderRepository supplyOrderRepository;
    private final WarehouseRepository warehouseRepository;
    private final IStockTransaction iStockTransaction;
    private final ISupplyOrderItem iSupplyOrderItem;
    private final IWarehouseStock iWarehouseStock;
    private final IGeneralStock iGeneralStock;
    private final SupplyOrderRepositoryCustom supplyOrderRepositoryCustom;
    private final IAudit iAudit;
    private final ProductRepository productRepository;
    private final WarehouseStockRepository warehouseStockRepository;
    private final IUtil iUtil;
    private final SupplyOrderItemRepository supplyOrderItemRepository;
    private final SupplierRepository supplierRepository;
    private final PurchaseDocumentRepository purchaseDocumentRepository;
    private final ProductPriceRepository productPriceRepository;
    private final PurchasePaymentMethodRepository purchasePaymentMethodRepository;
    @Override
    @Transactional
    public ResponseSuccess save(RequestSupplyOrder requestSupplyOrder, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {

        User user;
        Warehouse warehouse;
        SupplyOrder supplyOrder;
        Supplier supplier;
        PurchaseDocument purchaseDocument;
        PurchasePaymentMethod purchasePaymentMethod;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            purchaseDocument = purchaseDocumentRepository.findByNameAndStatusTrue(requestSupplyOrder.getPurchaseDocument());
            purchasePaymentMethod = purchasePaymentMethodRepository.findByNameAndStatusTrue(requestSupplyOrder.getPurchasePaymentMethod());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }else{
            supplier = supplierRepository.findByRucAndClientIdAndStatusTrue(requestSupplyOrder.getSupplierRuc(), user.getClientId());
            warehouse = warehouseRepository.findByClientIdAndNameAndStatusTrue(user.getClientId(),requestSupplyOrder.getWarehouse().toUpperCase());
        }

        if (warehouse == null) {
            throw new BadRequestExceptions(Constants.ErrorWarehouse);
        }else{
            supplyOrder = supplyOrderRepository.findByRef(requestSupplyOrder.getRef());
        }

        if (!Objects.equals(warehouse.getClientId(), user.getClientId())) {
            throw new BadRequestExceptions(Constants.ErrorWarehouse);
        }

        if (supplyOrder != null) {
            throw new BadRequestExceptions(Constants.ErrorPurchaseExists);
        }

        if(supplier==null){
            throw new BadRequestExceptions(Constants.ErrorSupplier);
        }

        if(purchaseDocument == null){
            throw new BadRequestExceptions(Constants.ErrorPurchaseDocument);
        }

        if(purchasePaymentMethod==null){
            throw new BadRequestExceptions(Constants.ErrorPurchasePaymentMethod);
        }

        try{

            List<RequestStockTransactionItem> requestStockTransactionItemList = requestSupplyOrder.getRequestSupplyOrderItemList().stream().map(supplyOrderItem -> RequestStockTransactionItem.builder()
                    .quantity(supplyOrderItem.getQuantity())
                    .productId(supplyOrderItem.getProductId())
                    .build()).toList();

            Long orderNumber = supplyOrderRepository.countByClientId(user.getClientId())+1L;
            // Parse to LocalDate
            LocalDate localDate = LocalDate.parse(requestSupplyOrder.getDeliveryDate(), DateTimeFormatter.ofPattern("yyyy-MM-dd"));

            // Convert LocalDate to OffsetDateTime (Midnight at UTC)
            OffsetDateTime offsetDateTime = localDate.atStartOfDay().atOffset(ZoneOffset.ofHours(-5));
            SupplyOrder newSupplyOrder = supplyOrderRepository.save(SupplyOrder.builder()
                            .ref(requestSupplyOrder.getRef().toUpperCase())
                            .orderNumber(orderNumber)
                            .supplier(supplier)
                            .supplierId(supplier.getId())
                            .status(true)
                            .registrationDate(OffsetDateTime.now())
                            .updateDate(OffsetDateTime.now())
                            .warehouse(warehouse)
                            .warehouseId(warehouse.getId())
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .user(user).userId(user.getId())
                            .purchaseDocument(purchaseDocument)
                            .purchaseDocumentId(purchaseDocument.getId())
                            .purchasePaymentMethod(purchasePaymentMethod)
                            .purchasePaymentMethodId(purchasePaymentMethod.getId())
                            .deliveryDate(offsetDateTime)
                      .build());
            iStockTransaction.save("S"+newSupplyOrder.getOrderNumber(), warehouse,requestStockTransactionItemList,"INGRESO",user);
            iAudit.save("ADD_PURCHASE","COMPRA " + newSupplyOrder.getOrderNumber() +" CREADA.", newSupplyOrder.getOrderNumber().toString(),user.getUsername());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        }catch (RuntimeException e){
            e.printStackTrace();
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    @Transactional
    public CompletableFuture<ResponseSuccess> saveAsync(RequestSupplyOrder requestSupplyOrder, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Warehouse warehouse;
            SupplyOrder supplyOrder;
            Supplier supplier;
            PurchaseDocument purchaseDocument;
            PurchasePaymentMethod purchasePaymentMethod;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                purchaseDocument = purchaseDocumentRepository.findByNameAndStatusTrue(requestSupplyOrder.getPurchaseDocument());
                purchasePaymentMethod = purchasePaymentMethodRepository.findByNameAndStatusTrue(requestSupplyOrder.getPurchasePaymentMethod());
            } catch (RuntimeException e) {
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                supplier = supplierRepository.findByRucAndClientIdAndStatusTrue(requestSupplyOrder.getSupplierRuc(), user.getClientId());
                warehouse = warehouseRepository.findByClientIdAndNameAndStatusTrue(user.getClientId(),requestSupplyOrder.getWarehouse().toUpperCase());
            }

            if (warehouse == null) {
                throw new BadRequestExceptions(Constants.ErrorWarehouse);
            }else{
                supplyOrder = supplyOrderRepository.findByRef(requestSupplyOrder.getRef());
            }

            if (!Objects.equals(warehouse.getClientId(), user.getClientId())) {
                throw new BadRequestExceptions(Constants.ErrorWarehouse);
            }

            if (supplyOrder != null) {
                throw new BadRequestExceptions(Constants.ErrorPurchaseExists);
            }

            if(supplier==null){
                throw new BadRequestExceptions(Constants.ErrorSupplier);
            }

            if(purchaseDocument == null){
                throw new BadRequestExceptions(Constants.ErrorPurchaseDocument);
            }

            if(purchasePaymentMethod==null){
                throw new BadRequestExceptions(Constants.ErrorPurchasePaymentMethod);
            }

            try{
                List<RequestStockTransactionItem> requestStockTransactionItemList = requestSupplyOrder.getRequestSupplyOrderItemList().stream().map(supplyOrderItem -> RequestStockTransactionItem.builder()
                        .quantity(supplyOrderItem.getQuantity())
                        .productId(supplyOrderItem.getProductId())
                        .build()).toList();
                // Parse to LocalDate
                LocalDate localDate = LocalDate.parse(requestSupplyOrder.getDeliveryDate(), DateTimeFormatter.ofPattern("yyyy-MM-dd"));
                // Convert LocalDate to OffsetDateTime (Midnight at UTC)
                OffsetDateTime offsetDateTime = localDate.atStartOfDay().atOffset(ZoneOffset.ofHours(-5));
                Long orderNumber = supplyOrderRepository.countByClientId(user.getClientId())+1L;
                SupplyOrder newSupplyOrder = supplyOrderRepository.save(SupplyOrder.builder()
                        .ref(requestSupplyOrder.getRef().toUpperCase())
                        .status(true)
                        .supplier(supplier)
                        .supplierId(supplier.getId())
                        .orderNumber(orderNumber)
                        .registrationDate(OffsetDateTime.now())
                        .updateDate(OffsetDateTime.now())
                        .warehouse(warehouse)
                        .warehouseId(warehouse.getId())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .user(user).userId(user.getId())
                        .purchaseDocument(purchaseDocument)
                        .purchaseDocumentId(purchaseDocument.getId())
                        .deliveryDate(offsetDateTime)
                        .purchasePaymentMethod(purchasePaymentMethod)
                        .purchasePaymentMethodId(purchasePaymentMethod.getId())
                        .build());
                for(RequestSupplyOrderItem requestSupplyOrderItem : requestSupplyOrder.getRequestSupplyOrderItemList()){
                    iSupplyOrderItem.save(newSupplyOrder,warehouse.getName(), requestSupplyOrderItem,user.getUsername());
                }
                iStockTransaction.save("S"+newSupplyOrder.getOrderNumber(), warehouse,requestStockTransactionItemList,"INGRESO",user);
                iAudit.save("ADD_PURCHASE","COMPRA " + newSupplyOrder.getOrderNumber() +" CREADA.", newSupplyOrder.getOrderNumber().toString(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            }catch (RuntimeException e){
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<SupplyOrderDTO>> list(
            Long orderNumber,
            String ref,
            String user,
            String warehouse,
            String supplier,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<SupplyOrder> pagePurchase;
            UUID clientId;

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                pagePurchase = supplyOrderRepositoryCustom.searchForSupplyOrder(
                        clientId,
                        orderNumber,
                        ref,
                        warehouse,
                        supplier,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateEndDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        status);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.ResultsFound);
            }

            if(pagePurchase.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }

            List<SupplyOrderDTO> supplyOrderDTOS = pagePurchase.getContent().stream().map(supplyOrder -> {
                List<SupplyOrderItemDTO> supplyOrderItemDTOS = supplyOrderItemRepository.findAllBySupplyOrderId(supplyOrder.getId()).stream()
                        .map(supplyOrderItem -> {
                            Double igvAmount = (supplyOrderItem.getUnitValue() * supplyOrderItem.getPurchaseIGV().getValue())/100;
                            return SupplyOrderItemDTO.builder()
                                    .id(supplyOrderItem.getId())
                                    .ref(supplyOrderItem.getSupplyOrder().getRef())
                                    .productId(supplyOrderItem.getProductId())
                                    .product(supplyOrderItem.getProduct().getName())
                                    .productSku(iUtil.buildProductSku(supplyOrderItem.getProduct()))
                                    .orderNumber(supplyOrderItem.getSupplyOrder().getOrderNumber())
                                    .quantity(supplyOrderItem.getQuantity())
                                    .warehouse(supplyOrderItem.getSupplyOrder().getWarehouse().getName())
                                    .model(supplyOrderItem.getProduct().getModel().getName())
                                    .color(supplyOrderItem.getProduct().getColor().getName())
                                    .size(supplyOrderItem.getProduct().getSize().getName())
                                    .registrationDate(supplyOrderItem.getRegistrationDate())
                                    .updateDate(supplyOrderItem.getUpdateDate())
                                    .user(supplyOrderItem.getUser().getUsername())
                                    .status(supplyOrderItem.getStatus())
                                    .supplier(supplyOrderItem.getSupplyOrder().getSupplier().getBusinessName())
                                    .observations(supplyOrderItem.getObservations())
                                    .unitSalePrice(supplyOrderItem.getUnitSalePrice())
                                    .discountsAmount(supplyOrderItem.getDiscountsAmount())
                                    .chargesAmount(supplyOrderItem.getChargesAmount())
                                    .igv(supplyOrderItem.getPurchaseIGV().getName())
                                    .igvAmount(supplyOrderItem.getPurchaseIGV().getValue())
                                    .igvPercentage(supplyOrderItem.getPurchaseIGV().getPercentage())
                                    .unitPurchasePrice(supplyOrderItem.getUnitValue()+igvAmount)
                                    .build();
                        })
                        .toList();
                return SupplyOrderDTO.builder()
                        .purchaseDocument(supplyOrder.getPurchaseDocument().getName())
                        .ref(supplyOrder.getRef())
                        .warehouse(supplyOrder.getWarehouse().getName())
                        .registrationDate(supplyOrder.getRegistrationDate())
                        .updateDate(supplyOrder.getUpdateDate())
                        .orderNumber(supplyOrder.getOrderNumber())
                        .status(supplyOrder.getStatus())
                        .supplyOrderItemDTOList(supplyOrderItemDTOS)
                        .deliveryDate(supplyOrder.getDeliveryDate())
                        .user(supplyOrder.getUser().getUsername())
                        .id(supplyOrder.getId())
                        .supplier(supplyOrder.getSupplier().getBusinessName())
                        .build();
            }).toList();

            return new PageImpl<>(supplyOrderDTOS,pagePurchase.getPageable(),pagePurchase.getTotalElements());
        });
    }
    @Override
    public CompletableFuture<List<CheckStockDTO>> checkStock(UUID productId, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<WarehouseStock> warehouseStocks;
            Product product;
            User user;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                product = productRepository.findByIdAndStatusTrue(productId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(product == null){
                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
            }else{
                warehouseStocks = warehouseStockRepository.findAllByProductId(product.getId());
            }

            try {
                return warehouseStocks.stream().map(warehouseStock -> CheckStockDTO.builder()
                        .warehouse(warehouseStock.getWarehouse().getName())
                        .quantity(warehouseStock.getQuantity())
                        .build()
                ).toList();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> closeSupplyOrder(UUID supplyOrderId, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            SupplyOrder supplyOrder;
            try {
                user=userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                supplyOrder=supplyOrderRepository.findByIdAndStatusTrue(supplyOrderId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(supplyOrder==null){
                throw new BadRequestExceptions(Constants.ErrorSupplyOrderInactive);
            }
            try {
                supplyOrder.setStatus(false);
                supplyOrder.setUpdateDate(OffsetDateTime.now());
                supplyOrder.setUser(user);
                supplyOrder.setUserId(user.getId());
                supplyOrderRepository.save(supplyOrder);
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

}
