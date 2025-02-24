package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.request.RequestDeliveryManifestItem;
import com.proyect.masterdata.dto.request.RequestStockTransactionItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.*;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class DeliveryManifestItemImpl implements IDeliveryManifestItem{
    private final DeliveryManifestItemRepository deliveryManifestItemRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final OrderItemRepository orderItemRepository;
    private final WarehouseStockRepository warehouseStockRepository;
    private final DeliveryStatusRepository deliveryStatusRepository;
    private final UserRepository userRepository;
    private final IWarehouseStock iWarehouseStock;
    private final IGeneralStock iGeneralStock;
    private final IAudit iAudit;
    private final IStockTransaction iStockTransaction;
    @Override
    public CompletableFuture<DeliveryManifestItem> save(
            RequestDeliveryManifestItem requestDeliveryManifestItem,
            DeliveryManifest deliveryManifest,
            Warehouse warehouse,
            User user) throws BadRequestExceptions, InterruptedException {
        return CompletableFuture.supplyAsync(()->{
            SupplierProduct supplierProduct;
            OrderItem orderItem;
            WarehouseStock warehouseStock;
            DeliveryStatus deliveryStatus;
            try{
                supplierProduct = supplierProductRepository.findByIdAndStatusTrue(requestDeliveryManifestItem.getSupplierProductId());
                orderItem = orderItemRepository.findByIdAndStatusTrue(requestDeliveryManifestItem.getOrderItemId());
                deliveryStatus = deliveryStatusRepository.findByName("PENDIENTE");
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(supplierProduct==null){
                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
            }
            if(orderItem==null){
                throw new BadRequestExceptions(Constants.ErrorOrderItem);
            }else{
                warehouseStock = warehouseStockRepository.findByWarehouseIdAndSupplierProductId(warehouse.getId(),supplierProduct.getId());
            }
            if(warehouseStock.getQuantity()<requestDeliveryManifestItem.getQuantity()){
                throw new BadRequestExceptions(Constants.ErrorWarehouseStockLess);
            }
            try{
                DeliveryManifestItem newDeliveryManifestItem = deliveryManifestItemRepository.save(DeliveryManifestItem.builder()
                        .deliveryManifest(deliveryManifest)
                        .deliveryManifestId(deliveryManifest.getId())
                        .quantity(requestDeliveryManifestItem.getQuantity())
                        .collected(false)
                        .orderItem(orderItem)
                        .orderItemId(orderItem.getId())
                        .supplierProduct(supplierProduct)
                        .supplierProductId(supplierProduct.getId())
                        .deliveryStatus(deliveryStatus)
                        .deliveryStatusId(deliveryStatus.getId())
                        .build());
                iAudit.save(
                        "ADD_DELIVERY_MANIFEST_ITEM",
                        "ITEM DE GUIA "+
                                newDeliveryManifestItem.getId()+
                                "PARA PEDIDO " +
                                newDeliveryManifestItem.getOrderItem().getOrdering().getOrderNumber() +
                                " CREADO.",
                        newDeliveryManifestItem.getId().toString(),user.getUsername());
                return newDeliveryManifestItem;
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }
    @Override
    public CompletableFuture<ResponseSuccess> updateDeliveryManifestItem(UUID deliveryManifestItemId, String username) {
        return CompletableFuture.supplyAsync(()->{
            User user;
            DeliveryManifestItem deliveryManifestItem;
            DeliveryStatus deliveryStatus;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                deliveryManifestItem = deliveryManifestItemRepository.findById(deliveryManifestItemId).orElse(null);
                deliveryStatus = deliveryStatusRepository.findByName("COMPLETO");
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(deliveryManifestItem==null){
                throw new BadRequestExceptions(Constants.ErrorDeliveryManifestItem);
            }
            if(deliveryStatus==null){
                throw new BadRequestExceptions(Constants.ErrorDeliveryStatus);
            }
            try{
                deliveryManifestItem.setDeliveryStatus(deliveryStatus);
                deliveryManifestItem.setDeliveryStatusId(deliveryStatus.getId());
                deliveryManifestItem.setUser(user);
                deliveryManifestItem.setUserId(user.getId());
                deliveryManifestItem.setCollected(true);
                deliveryManifestItemRepository.save(deliveryManifestItem);
                iWarehouseStock.out(
                        deliveryManifestItem.getDeliveryManifest().getWarehouse(),
                        deliveryManifestItem.getSupplierProduct(),
                        deliveryManifestItem.getQuantity(),
                        user
                );
                iGeneralStock.out(
                        deliveryManifestItem.getSupplierProduct(),
                        deliveryManifestItem.getQuantity(),
                        user.getUsername()
                );
                List<RequestStockTransactionItem> stockTransactionList = new ArrayList<>();
                stockTransactionList.add(RequestStockTransactionItem.builder()
                        .supplierProductId(deliveryManifestItem.getSupplierProduct().getId())
                        .quantity(deliveryManifestItem.getQuantity())
                        .build());
                iStockTransaction.save(
                        "O"+deliveryManifestItem.getOrderItem().getOrdering().getOrderNumber(),
                        deliveryManifestItem.getDeliveryManifest().getWarehouse(),
                        stockTransactionList,
                        "PEDIDO",
                        user);
                iAudit.save(
                        "UPDATE_DELIVERY_MANIFEST_ITEM",
                        "ITEM DE GUIA "+
                                deliveryManifestItem.getId()+
                                "PARA PEDIDO " +
                                deliveryManifestItem.getOrderItem().getOrdering().getOrderNumber() +
                                " ACTUALIZADO.",
                        deliveryManifestItem.getId().toString(),user.getUsername());
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
}
