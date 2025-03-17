package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.DeliveryManifestItemDTO;
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
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class DeliveryManifestItemImpl implements IDeliveryManifestItem{
    private final DeliveryManifestItemRepository deliveryManifestItemRepository;
    private final OrderItemRepository orderItemRepository;
    private final WarehouseStockRepository warehouseStockRepository;
    private final UserRepository userRepository;
    private final IWarehouseStock iWarehouseStock;
    private final IGeneralStock iGeneralStock;
    private final IAudit iAudit;
    private final IStockTransaction iStockTransaction;
    private final DeliveryManifestItemRepositoryCustom deliveryManifestItemRepositoryCustom;
    private final IUtil iUtil;
    private final ProductPriceRepository productPriceRepository;
    @Override
    public CompletableFuture<DeliveryManifestItem> save(
            OrderItem orderItem,
            DeliveryManifest deliveryManifest,
            Warehouse warehouse,
            User user) throws BadRequestExceptions, InterruptedException {
        return CompletableFuture.supplyAsync(()->{
            WarehouseStock warehouseStock;
            DeliveryManifestItem deliveryManifestItem;
            try{
                warehouseStock = warehouseStockRepository.findByWarehouseIdAndProductId(warehouse.getId(),orderItem.getProduct().getId());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(warehouseStock==null){
                throw new BadRequestExceptions(Constants.ErrorWarehouseStock);
            }else{
                deliveryManifestItem = deliveryManifestItemRepository.findByOrderItemIdAndProductIdAndDeliveredTrue(orderItem.getId(),orderItem.getProductId());
            }
            if(deliveryManifestItem!=null){
                throw new BadRequestExceptions(Constants.ErrorDeliveryManifestItemDelivered);
            }
            if(warehouseStock.getQuantity()<orderItem.getQuantity()){
                throw new BadRequestExceptions(Constants.ErrorWarehouseStockLess);
            }
            try{
                DeliveryManifestItem newDeliveryManifestItem = deliveryManifestItemRepository.save(DeliveryManifestItem.builder()
                        .deliveryManifest(deliveryManifest)
                        .deliveryManifestId(deliveryManifest.getId())
                        .quantity(orderItem.getQuantity())
                        .collected(false)
                        .orderItem(orderItem)
                        .orderItemId(orderItem.getId())
                        .delivered(false)
                        .build());
                iWarehouseStock.out(
                        newDeliveryManifestItem.getDeliveryManifest().getWarehouse(),
                        newDeliveryManifestItem.getProduct(),
                        newDeliveryManifestItem.getQuantity(),
                        user
                );
                iGeneralStock.out(
                        newDeliveryManifestItem.getProduct(),
                        newDeliveryManifestItem.getQuantity(),
                        user.getUsername()
                );
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
    public CompletableFuture<ResponseSuccess> updateDeliveryManifestItem(
            UUID deliveryManifestItemId,
            Boolean collected,
            Boolean delivered,
            String username) {
        return CompletableFuture.supplyAsync(()->{
            User user;
            DeliveryManifestItem deliveryManifestItem;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                deliveryManifestItem = deliveryManifestItemRepository.findById(deliveryManifestItemId).orElse(null);
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
            try{
                deliveryManifestItem.setDelivered(collected);
                deliveryManifestItem.setUser(user);
                deliveryManifestItem.setUserId(user.getId());
                deliveryManifestItem.setCollected(delivered);
                deliveryManifestItemRepository.save(deliveryManifestItem);
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

    @Override
    public CompletableFuture<Page<DeliveryManifestItemDTO>> list(
            String user,
            Integer quantity,
            Boolean collected,
            Long orderNumber,
            Long manifestNumber,
            String color,
            String size,
            String model,
            String brand,
            Boolean delivered,
            String courier,
            String warehouse,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) {
        return CompletableFuture.supplyAsync(()->{
            Page<DeliveryManifestItem> deliveryManifestItemPage;
            UUID clientId;
            try{
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                deliveryManifestItemPage = deliveryManifestItemRepositoryCustom.searchForDeliveryManifestItem(
                        clientId,
                        quantity,
                        collected,
                        orderNumber,
                        manifestNumber,
                        color,
                        size,
                        model,
                        brand,
                        delivered,
                        courier,
                        warehouse,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateEndDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize
                );
            }catch (RuntimeException e){
                log.error(e.getMessage());
                e.printStackTrace();
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if(deliveryManifestItemPage.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }
            List<DeliveryManifestItemDTO> deliveryManifestItemDTOS = deliveryManifestItemPage.stream().map(deliveryManifestItem -> {
                ProductPrice productPrice = productPriceRepository.findByProductId(deliveryManifestItem.getProductId());
                Double totalPrice = null;
                if(Objects.equals(deliveryManifestItem.getOrderItem().getDiscount().getName(), "PORCENTAJE")){
                    totalPrice = (productPrice.getUnitSalePrice() * deliveryManifestItem.getOrderItem().getQuantity())-((productPrice.getUnitSalePrice() * deliveryManifestItem.getOrderItem().getQuantity())*(deliveryManifestItem.getOrderItem().getDiscountAmount()/100));
                }

                if(Objects.equals(deliveryManifestItem.getOrderItem().getDiscount().getName(), "MONTO")){
                    totalPrice = (productPrice.getUnitSalePrice() * deliveryManifestItem.getOrderItem().getQuantity())-(deliveryManifestItem.getOrderItem().getDiscountAmount());
                }

                if(Objects.equals(deliveryManifestItem.getOrderItem().getDiscount().getName(), "NO APLICA")){
                    totalPrice = (productPrice.getUnitSalePrice() * deliveryManifestItem.getOrderItem().getQuantity());
                }
                return DeliveryManifestItemDTO.builder()
                        .id(deliveryManifestItem.getId())
                        .phone(deliveryManifestItem.getOrderItem().getOrdering().getCustomer().getPhone())
                        .district(deliveryManifestItem.getOrderItem().getOrdering().getCustomer().getDistrict().getName())
                        .manifestNumber(deliveryManifestItem.getDeliveryManifest().getManifestNumber())
                        .orderNumber(deliveryManifestItem.getOrderItem().getOrdering().getOrderNumber())
                        .quantity(deliveryManifestItem.getQuantity())
                        .customer(deliveryManifestItem.getOrderItem().getOrdering().getCustomer().getName())
                        .skuProduct(iUtil.buildProductSku(deliveryManifestItem.getProduct()))
                        .management(deliveryManifestItem.getOrderItem().getOrdering().getManagementType().getName())
                        .paymentMethod(deliveryManifestItem.getOrderItem().getOrdering().getOrderPaymentMethod().getName())
                        .paymentState(deliveryManifestItem.getOrderItem().getOrdering().getOrderPaymentState().getName())
                        .orderItemAmount(totalPrice)
                        .product(deliveryManifestItem.getProduct().getName())
                        .build();
            }).toList();
            return new PageImpl<>(deliveryManifestItemDTOS,deliveryManifestItemPage.getPageable(),deliveryManifestItemPage.getTotalElements());
        });
    }
}
