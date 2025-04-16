package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.*;
import com.proyect.masterdata.dto.projections.DeliveryManifestItemDTOP;
import com.proyect.masterdata.dto.projections.DeliveryManifestItemProjection;
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
    private final CourierRepository courierRepository;
    private final DeliveryManifestRepository deliveryManifestRepository;
    private final OrderingRepository orderingRepository;
    @Override
    public CompletableFuture<DeliveryManifestItem> save(
            OrderItem orderItem,
            DeliveryManifest deliveryManifest,
            Warehouse warehouse,
            User user) throws BadRequestExceptions, InterruptedException {
        System.out.println("entroooo manifest item");
        return CompletableFuture.supplyAsync(()->{
            WarehouseStock warehouseStock;
            DeliveryManifestItem deliveryManifestItem;
            DeliveryManifestItemDTOP deliveryManifestItemDTOP;

            Boolean deliveryManifestItemExists = false;
            try{
                System.out.println("entra -> id product: " + orderItem.getProduct().getId() + " warehouseId: " + warehouse.getId());
                warehouseStock = warehouseStockRepository.findByWarehouseIdAndProductId(warehouse.getId(),orderItem.getProduct().getId());
                System.out.println( "werestock -> " + warehouseStock);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(warehouseStock==null){
                throw new BadRequestExceptions(Constants.ErrorWarehouseStock);
            }else{
                deliveryManifestItemDTOP = deliveryManifestItemRepository.findByOrderItemIdAndProductId(orderItem.getId(),orderItem.getProductId());
                //System.out.println("deliverymanifestitemDTOP -> " + deliveryManifestItemDTOP.getProduct());
            }
            if(deliveryManifestItemDTOP!=null || orderItem.getDeliveredProducts() >= orderItem.getQuantity()){
                deliveryManifestItemExists = deliveryManifestItemRepository.existsByOrderItemIdAndProductId(orderItem.getId(),orderItem.getProductId());
            }
            if(deliveryManifestItemExists || orderItem.getDeliveredProducts() >= orderItem.getQuantity()){
                throw new BadRequestExceptions(Constants.ErrorDeliveryManifestItemDelivered);
            }
            // codigo comentado mientras se implementa kardex en el inventario
//            if(warehouseStock.getQuantity()<orderItem.getQuantity()){
//                throw new BadRequestExceptions(Constants.ErrorWarehouseStockLess);
//
//            }
            try{
                DeliveryManifestItem newDeliveryManifestItem = deliveryManifestItemRepository.save(DeliveryManifestItem.builder()
                        .deliveryManifest(deliveryManifest)
                        .deliveryManifestId(deliveryManifest.getId())
                        .quantity(orderItem.getPreparedProducts())
                        .deliveredQuantity(0)
                        .collectedQuantity(0)
                        .productId(orderItem.getProduct().getId())
                        .product(orderItem.getProduct())
                        .orderItem(orderItem)
                        .orderItemId(orderItem.getId())
                        .userId(orderItem.getUser().getId())
                        .user(orderItem.getUser())
                        .registrationDate(OffsetDateTime.now())
                        .updateDate(OffsetDateTime.now())
                        .clientId(user.getClientId())
                        .client(user.getClient())
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
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }
    @Override
    public CompletableFuture<ResponseSuccess> markDeliveredDeliveryManifestItem(
            UUID deliveryManifestItemId,
            Integer quantity,
            String username) {
        return CompletableFuture.supplyAsync(()->{
            User user;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
            }catch (RuntimeException e){
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            try{
                List<Object[]> orderNumber = deliveryManifestItemRepository.retrieveDeliveryManifestItemOrderNumber(deliveryManifestItemId,user.getClientId());
                for(Object[] result:orderNumber){
                    if(quantity<=(Integer)result[1]){
                        deliveryManifestItemRepository.setDeliveredQuantityDeliveredManifestItem(
                                deliveryManifestItemId,
                                user.getClientId(),
                                OffsetDateTime.now(),
                                quantity
                        );
                    }else{
                        throw new BadRequestExceptions(Constants.ErrorDeliveryManifestItemDeliveredQuantity);
                    }
                    iAudit.save(
                            "UPDATE_DELIVERY_MANIFEST_ITEM",
                            "ITEM DE GUIA "+
                                    deliveryManifestItemId+
                                    "PARA PEDIDO " +
                                    result[0]+
                                    " ACTUALIZADO.",
                            deliveryManifestItemId.toString(),user.getUsername());
                }
                return ResponseSuccess.builder()
                        .message(Constants.update)
                        .code(200)
                        .build();
            }catch (RuntimeException e){
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> markCollectedDeliveryManifestItem(
            UUID deliveryManifestItemId,
            Integer quantity,
            String username) {
        return CompletableFuture.supplyAsync(()->{
            User user;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
            }catch (RuntimeException e){
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            try{
                List<Object[]> orderNumber = deliveryManifestItemRepository.retrieveDeliveryManifestItemOrderNumber(deliveryManifestItemId,user.getClientId());
                for(Object[] result:orderNumber){
                    if(quantity<=(Integer)result[1]){
                        deliveryManifestItemRepository.setCollectedQuantityDeliveredManifestItem(
                                deliveryManifestItemId,
                                user.getClientId(),
                                OffsetDateTime.now(),
                                quantity
                        );
                    }else {
                        throw new BadRequestExceptions(Constants.ErrorDeliveryManifestItemCollectedQuantity);
                    }
                    iAudit.save(
                            "UPDATE_DELIVERY_MANIFEST_ITEM",
                            "ITEM DE GUIA "+
                                    deliveryManifestItemId+
                                    "PARA PEDIDO " +
                                    result[0]+
                                    " ACTUALIZADO.",
                            deliveryManifestItemId.toString(),user.getUsername());
                }
                return ResponseSuccess.builder()
                        .message(Constants.update)
                        .code(200)
                        .build();
            }catch (RuntimeException e){
                e.printStackTrace();
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
            UUID orderId,
            Long manifestNumber,
            String color,
            String size,
            String model,
            String brand,
            Boolean delivered,
            String courier,
            String courierDni,
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
            Page<DeliveryManifestItemProjectionDTO> deliveryManifestItemPage;
            UUID clientId;
            try{
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                deliveryManifestItemPage = deliveryManifestItemRepositoryCustom.searchForDeliveryManifestItem(
                        clientId,
                        quantity,
                        collected,
                        orderNumber,
                        orderId,
                        manifestNumber,
                        color,
                        size,
                        model,
                        brand,
                        delivered,
                        courier,
                        courierDni,
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
                ProductPrice productPrice = productPriceRepository.findClosestByProductIdAndDate(deliveryManifestItem.getProductId(),deliveryManifestItem.getRegistrationDate());
                Double totalPrice = null;
                if(Objects.equals(deliveryManifestItem.getDiscountName(), "PORCENTAJE")){
                    totalPrice = (productPrice.getUnitSalePrice() * deliveryManifestItem.getPreparedProducts())-((productPrice.getUnitSalePrice() * deliveryManifestItem.getPreparedProducts())*(deliveryManifestItem.getDiscountAmount()/100));
                }

                if(Objects.equals(deliveryManifestItem.getDiscountName(), "MONTO")){
                    totalPrice = (productPrice.getUnitSalePrice() * deliveryManifestItem.getPreparedProducts())-(deliveryManifestItem.getDiscountAmount());
                }

                if(Objects.equals(deliveryManifestItem.getDiscountName(), "NO APLICA")){
                    totalPrice = (productPrice.getUnitSalePrice() * deliveryManifestItem.getPreparedProducts());
                }
                return DeliveryManifestItemDTO.builder()
                        .id(deliveryManifestItem.getDeliveryManifestItemId())
                        .deliveredQuantity(deliveryManifestItem.getDeliveredQuantity())
                        .collectedQuantity(deliveryManifestItem.getCollectedQuantity())
                        .phone(deliveryManifestItem.getPhone())
                        .district(deliveryManifestItem.getDistrictName())
                        .manifestNumber(deliveryManifestItem.getManifestNumber())
                        .orderNumber(deliveryManifestItem.getOrderNumber())
                        .quantity(deliveryManifestItem.getQuantity())
                        .deliveredQuantity(deliveryManifestItem.getDeliveredQuantity())
                        .collectedQuantity(deliveryManifestItem.getCollectedQuantity())
                        .customer(deliveryManifestItem.getCustomerName())
                        .skuProduct(iUtil.buildProductSku(productPrice.getProduct()))
                        .management(deliveryManifestItem.getManagementType())
                        .paymentMethod(deliveryManifestItem.getPaymentMethod())
                        .paymentState(deliveryManifestItem.getPaymentState())
                        .orderItemAmount(totalPrice)
                        .product(productPrice.getProduct().getName())
                        .user(deliveryManifestItem.getUsername())
                        .orderId(deliveryManifestItem.getOrderId())
                        .address(deliveryManifestItem.getAddress())
                        .dni(deliveryManifestItem.getDni())
                        .build();
            }).toList();
            return new PageImpl<>(deliveryManifestItemDTOS,deliveryManifestItemPage.getPageable(),deliveryManifestItemPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<CourierProfileDTO> courierProfile(OffsetDateTime startDate, OffsetDateTime endDate, String username) {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Courier courier;
            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                courier = courierRepository.findByDniAndClientIdAndStatusTrue(user.getDni(),user.getClientId());
            }
            if(courier==null){
                throw new BadRequestExceptions(Constants.ErrorCourier);
            }
            try {
                List<Object[]> deliveredOrders = deliveryManifestItemRepository.countDeliveredOrders(courier.getId(),startDate,endDate);
                List<DeliveredOrdersCountDTO> deliveredOrdersCountDTOS = new ArrayList<>();
                Long deliveredOrderCount = 0L;
                Set<UUID> uniqueOrderNumbersDeliveredOrders = new HashSet<>();
                for(Object[] result : deliveredOrders){
                    UUID orderId = (UUID) result[1];
                    uniqueOrderNumbersDeliveredOrders.add(orderId);
                }
                for(UUID ignored :uniqueOrderNumbersDeliveredOrders){
                    deliveredOrderCount++;
                }
                List<Object[]> deliveredAndUnCollectedOrders = deliveryManifestItemRepository.findDeliveredAndUnCollectedOrders(courier.getId(),startDate,endDate);
                double unCollectedAmount = 0.00;
                List<Ordering> ordersUnCollected = new ArrayList<>();
                Set<Long> uniqueOrderNumbersUnCollected = new HashSet<>();
                for(Object[] deliveryManifestItem:deliveredAndUnCollectedOrders){
                    if(!uniqueOrderNumbersUnCollected.contains((Long) deliveryManifestItem[0])){
                        uniqueOrderNumbersUnCollected.add((Long) deliveryManifestItem[0]);
                        Ordering ordering = orderingRepository.findByClientIdAndId(user.getClientId(),(UUID) deliveryManifestItem[1]);
                        ordersUnCollected.add(ordering);
                    }
                }
                System.out.println(uniqueOrderNumbersUnCollected);
                for(Ordering order:ordersUnCollected){
                    if(Objects.equals(order.getOrderPaymentState().getName(), "POR RECAUDAR")){
                        List<OrderItem> orderItems = orderItemRepository.findAllByOrderIdAndStatusTrue(order.getId());
                        double saleAmount = 0.00;
                        for(OrderItem orderItem : orderItems){
                            ProductPrice productPrice = productPriceRepository.findByProductIdAndStatusTrue(orderItem.getProductId());
                            if(Objects.equals(orderItem.getDiscount().getName(), "PORCENTAJE")) {
                                saleAmount += (productPrice.getUnitSalePrice() * orderItem.getDeliveredProducts()) - ((productPrice.getUnitSalePrice() * orderItem.getDeliveredProducts()) * (orderItem.getDiscountAmount() / 100));
                            }
                            if(Objects.equals(orderItem.getDiscount().getName(), "MONTO")){
                                saleAmount += (productPrice.getUnitSalePrice() * orderItem.getDeliveredProducts()) - orderItem.getDiscountAmount();
                            }
                            if(Objects.equals(orderItem.getDiscount().getName(), "NO APLICA")){
                                saleAmount += (productPrice.getUnitSalePrice() * orderItem.getDeliveredProducts());
                            }
                        }
                        System.out.println("Sale Amount : -> "+saleAmount);
                        double totalDuePayment=0;
                        if(Objects.equals(order.getDiscount().getName(), "PORCENTAJE")){
                            totalDuePayment = (saleAmount-((saleAmount)*(order.getDiscountAmount()/100))+order.getDeliveryAmount())-order.getAdvancedPayment();
                        }
                        if(Objects.equals(order.getDiscount().getName(), "MONTO")){
                            totalDuePayment = (saleAmount-order.getDiscountAmount()+order.getDeliveryAmount())-order.getAdvancedPayment();
                        }
                        if(Objects.equals(order.getDiscount().getName(), "NO APLICA")){
                            totalDuePayment = (saleAmount+order.getDeliveryAmount())-order.getAdvancedPayment();
                        }
                        System.out.println(totalDuePayment);
                        unCollectedAmount+= Math.max(totalDuePayment, 0.00);
                    }
                }
                System.out.println(unCollectedAmount);
                return CourierProfileDTO.builder()
                        .deliveredOrders(deliveredOrderCount)
                        .payableAmount(unCollectedAmount)
                        .courierInfo(CourierDTO.builder()
                                .id(courier.getId())
                                .user(courier.getUser().getUsername())
                                .status(courier.getStatus())
                                .name(courier.getName())
                                .phone(courier.getPhone())
                                .address(courier.getAddress())
                                .plate(courier.getPlate())
                                .registrationDate(courier.getRegistrationDate())
                                .updateDate(courier.getUpdateDate())
                                .company(courier.getDeliveryCompany().getName())
                                .dni(courier.getDni())
                                .build())
                        .build();
            }catch (RuntimeException e){
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }


}
