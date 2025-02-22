package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.request.RequestDeliveryManifestItem;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IDeliveryManifestItem;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class DeliveryManifestItemImpl implements IDeliveryManifestItem{
    private final DeliveryManifestItemRepository deliveryManifestItemRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final OrderItemRepository orderItemRepository;
    private final WarehouseStockRepository warehouseStockRepository;
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
            try{
                supplierProduct = supplierProductRepository.findByIdAndStatusTrue(requestDeliveryManifestItem.getSupplierProductId());
                orderItem = orderItemRepository.findByIdAndStatusTrue(requestDeliveryManifestItem.getOrderItemId());
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
                return deliveryManifestItemRepository.save(DeliveryManifestItem.builder()
                        .deliveryManifest(deliveryManifest)
                        .deliveryManifestId(deliveryManifest.getId())
                        .quantity(requestDeliveryManifestItem.getQuantity())
                        .orderItem(orderItem)
                        .orderItemId(orderItem.getId())
                        .supplierProduct(supplierProduct)
                        .supplierProductId(supplierProduct.getId())
                        .build());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }
}
