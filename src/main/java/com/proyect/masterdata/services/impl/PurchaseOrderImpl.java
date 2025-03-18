package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.CheckStockDTO;
import com.proyect.masterdata.dto.PurchaseOrderDTO;
import com.proyect.masterdata.dto.PurchaseOrderItemDTO;
import com.proyect.masterdata.dto.request.RequestStockTransactionItem;
import com.proyect.masterdata.dto.request.RequestPurchaseOrder;
import com.proyect.masterdata.dto.request.RequestPurchaseOrderItem;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.*;
import com.proyect.masterdata.utils.Constants;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.time.OffsetDateTime;
import java.time.ZoneOffset;
import java.time.format.DateTimeFormatter;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

@Service
@AllArgsConstructor
@Log4j2
public class PurchaseOrderImpl implements IPurchaseOrder {
    private final UserRepository userRepository;
    private final PurchaseOrderRepository purchaseOrderRepository;
    private final WarehouseRepository warehouseRepository;
    private final IStockTransaction iStockTransaction;
    private final IPurchaseOrderItem iPurchaseOrderItem;
    private final IWarehouseStock iWarehouseStock;
    private final IGeneralStock iGeneralStock;
    private final PurchaseOrderRepositoryCustom purchaseOrderRepositoryCustom;
    private final IAudit iAudit;
    private final ProductRepository productRepository;
    private final WarehouseStockRepository warehouseStockRepository;
    private final IUtil iUtil;
    private final PurchaseOrderItemRepository purchaseOrderItemRepository;
    private final SupplierRepository supplierRepository;
    private final PurchaseDocumentRepository purchaseDocumentRepository;
    private final ProductPriceRepository productPriceRepository;
    @Override
    @Transactional
    public ResponseSuccess save(RequestPurchaseOrder requestPurchaseOrder, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {

        User user;
        Warehouse warehouse;
        PurchaseOrder purchaseOrder;
        Supplier supplier;
        PurchaseDocument purchaseDocument;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            warehouse = warehouseRepository.findByNameAndStatusTrue(requestPurchaseOrder.getWarehouse().toUpperCase());
            purchaseDocument = purchaseDocumentRepository.findByNameAndStatusTrue(requestPurchaseOrder.getPurchaseDocument());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }else{
            supplier = supplierRepository.findByRucAndClientIdAndStatusTrue(requestPurchaseOrder.getSupplierRuc(), user.getClientId());
        }

        if (warehouse == null) {
            throw new BadRequestExceptions(Constants.ErrorWarehouse);
        }else{
            purchaseOrder = purchaseOrderRepository.findByRef(requestPurchaseOrder.getRef());
        }

        if (!Objects.equals(warehouse.getClientId(), user.getClientId())) {
            throw new BadRequestExceptions(Constants.ErrorWarehouse);
        }

        if (purchaseOrder != null) {
            throw new BadRequestExceptions(Constants.ErrorPurchaseExists);
        }

        if(supplier==null){
            throw new BadRequestExceptions(Constants.ErrorSupplier);
        }

        if(purchaseDocument == null){
            throw new BadRequestExceptions(Constants.ErrorPurchaseDocument);
        }

        try{

            List<RequestStockTransactionItem> requestStockTransactionItemList = requestPurchaseOrder.getRequestPurchaseOrderItemList().stream().map(purchaseOrderItem -> RequestStockTransactionItem.builder()
                    .quantity(purchaseOrderItem.getQuantity())
                    .productId(purchaseOrderItem.getProductId())
                    .build()).toList();
            Long orderNumber = purchaseOrderRepository.countByClientId(user.getClientId())+1L;
            PurchaseOrder newPurchaseOrder = purchaseOrderRepository.save(PurchaseOrder.builder()
                    .ref(requestPurchaseOrder.getRef().toUpperCase())
                    .orderNumber(orderNumber)
                    .supplier(supplier)
                    .supplierId(supplier.getId())
                    .status(true)
                    .registrationDate(OffsetDateTime.now())
                    .updateDate(OffsetDateTime.now())
                    .client(user.getClient())
                    .clientId(user.getClientId())
                    .user(user).userId(user.getId())
                    .build());
            iAudit.save("ADD_PURCHASE_ORDER","ORDEN DE COMPRA " + newPurchaseOrder.getOrderNumber() +" CREADA.", newPurchaseOrder.getOrderNumber().toString(),user.getUsername());
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
    public CompletableFuture<ResponseSuccess> saveAsync(RequestPurchaseOrder requestPurchaseOrder, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Warehouse warehouse;
            PurchaseOrder purchaseOrder;
            Supplier supplier;
            PurchaseDocument purchaseDocument;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                warehouse = warehouseRepository.findByNameAndStatusTrue(requestPurchaseOrder.getWarehouse().toUpperCase());
                purchaseDocument = purchaseDocumentRepository.findByNameAndStatusTrue(requestPurchaseOrder.getPurchaseDocument());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                supplier = supplierRepository.findByRucAndClientIdAndStatusTrue(requestPurchaseOrder.getSupplierRuc(), user.getClientId());
            }

            if (warehouse == null) {
                throw new BadRequestExceptions(Constants.ErrorWarehouse);
            }else{
                purchaseOrder = purchaseOrderRepository.findByRef(requestPurchaseOrder.getRef());
            }

            if (!Objects.equals(warehouse.getClientId(), user.getClientId())) {
                throw new BadRequestExceptions(Constants.ErrorWarehouse);
            }

            if (purchaseOrder != null) {
                throw new BadRequestExceptions(Constants.ErrorPurchaseExists);
            }

            if(supplier==null){
                throw new BadRequestExceptions(Constants.ErrorSupplier);
            }

            if(purchaseDocument == null){
                throw new BadRequestExceptions(Constants.ErrorPurchaseDocument);
            }

            try{
                List<RequestStockTransactionItem> requestStockTransactionItemList = requestPurchaseOrder.getRequestPurchaseOrderItemList().stream().map(purchaseOrderItem -> RequestStockTransactionItem.builder()
                        .quantity(purchaseOrderItem.getQuantity())
                        .productId(purchaseOrderItem.getProductId())
                        .build()).toList();
                // Parse to LocalDate
                LocalDate localDate = LocalDate.parse(requestPurchaseOrder.getDeliveryDate(), DateTimeFormatter.ofPattern("yyyy-MM-dd"));

                // Convert LocalDate to OffsetDateTime (Midnight at UTC)
                OffsetDateTime offsetDateTime = localDate.atStartOfDay().atOffset(ZoneOffset.UTC);
                Long orderNumber = purchaseOrderRepository.countByClientId(user.getClientId())+1L;
                PurchaseOrder newPurchaseOrder = purchaseOrderRepository.save(PurchaseOrder.builder()
                        .ref(requestPurchaseOrder.getRef().toUpperCase())
                        .status(true)
                        .supplier(supplier)
                        .supplierId(supplier.getId())
                        .orderNumber(orderNumber)
                        .registrationDate(OffsetDateTime.now())
                        .updateDate(OffsetDateTime.now())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .user(user).userId(user.getId())
                        .build());
                for(RequestPurchaseOrderItem requestPurchaseOrderItem : requestPurchaseOrder.getRequestPurchaseOrderItemList()){
                    iPurchaseOrderItem.save(newPurchaseOrder,requestPurchaseOrderItem,user.getUsername());
                }
                iStockTransaction.save("S"+newPurchaseOrder.getOrderNumber(), warehouse,requestStockTransactionItemList,"INGRESO",user);
                iAudit.save("ADD_PURCHASE_ORDER","ORDEN DE COMPRA " + newPurchaseOrder.getOrderNumber() +" CREADA.", newPurchaseOrder.getOrderNumber().toString(),user.getUsername());
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
    public CompletableFuture<Page<PurchaseOrderDTO>> list(
            Long orderNumber,
            String ref,
            String user,
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
            Page<PurchaseOrder> pagePurchase;
            UUID clientId;

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                pagePurchase = purchaseOrderRepositoryCustom.searchForPurchaseOrder(
                        clientId,
                        orderNumber,
                        ref,
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

            List<PurchaseOrderDTO> purchaseOrderDTOS = pagePurchase.getContent().stream().map(purchaseOrder -> {
                List<PurchaseOrderItemDTO> purchaseOrderItemDTOS = purchaseOrderItemRepository.findAllByPurchaseOrderId(purchaseOrder.getId()).stream()
                        .map(purchaseOrderItem -> PurchaseOrderItemDTO.builder()
                                .id(purchaseOrderItem.getId())
                                .color(purchaseOrderItem.getProduct().getColor().getName())
                                .model(purchaseOrderItem.getProduct().getModel().getName())
                                .size(purchaseOrderItem.getProduct().getSize().getName())
                                .productSku(iUtil.buildProductSku(purchaseOrderItem.getProduct()))
                                .ref(purchaseOrderItem.getPurchaseOrder().getRef())
                                .orderNumber(purchaseOrderItem.getPurchaseOrder().getOrderNumber())
                                .productId(purchaseOrderItem.getProductId())
                                .registrationDate(purchaseOrderItem.getRegistrationDate())
                                .updateDate(purchaseOrderItem.getUpdateDate())
                                .quantity(purchaseOrderItem.getQuantity())
                                .product(purchaseOrderItem.getProduct().getName())
                                .status(purchaseOrderItem.getStatus())
                                .user(purchaseOrderItem.getUser().getUsername())
                                .observations(purchaseOrderItem.getObservations())
                                .unitPrice(productPriceRepository.findByProductIdAndStatusTrue(purchaseOrderItem.getProductId()).getUnitSalePrice())
                                .build())
                        .toList();
                return PurchaseOrderDTO.builder()
                        .ref(purchaseOrder.getRef())
                        .registrationDate(purchaseOrder.getRegistrationDate())
                        .updateDate(purchaseOrder.getUpdateDate())
                        .orderNumber(purchaseOrder.getOrderNumber())
                        .status(purchaseOrder.getStatus())
                        .purchaseOrderItemDTOList(purchaseOrderItemDTOS)
                        .user(purchaseOrder.getUser().getUsername())
                        .id(purchaseOrder.getId())
                        .build();
            }).toList();

            return new PageImpl<>(purchaseOrderDTOS,pagePurchase.getPageable(),pagePurchase.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> closePurchaseOrder(UUID purchaseOrderId, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            PurchaseOrder purchaseOrder;
            try {
                user=userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                purchaseOrder=purchaseOrderRepository.findByIdAndStatusTrue(purchaseOrderId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(purchaseOrder==null){
                throw new BadRequestExceptions(Constants.ErrorPurchaseOrderInactive);
            }
            try {
                purchaseOrder.setStatus(false);
                purchaseOrder.setUpdateDate(OffsetDateTime.now());
                purchaseOrder.setUser(user);
                purchaseOrder.setUserId(user.getId());
                purchaseOrderRepository.save(purchaseOrder);
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
