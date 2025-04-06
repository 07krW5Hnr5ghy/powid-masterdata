package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.CourierDTO;
import com.proyect.masterdata.dto.CourierProfileDTO;
import com.proyect.masterdata.dto.DeliveredOrdersCountDTO;
import com.proyect.masterdata.dto.DeliveryManifestItemDTO;
import com.proyect.masterdata.dto.projections.DeliveryManifestItemDTOP;
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
import org.springframework.data.domain.PageRequest;
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
                deliveryManifestItem = deliveryManifestItemRepository.findByOrderItemIdAndProductId(orderItem.getId(),orderItem.getProductId());
            }
            if(deliveryManifestItem!=null || orderItem.getDeliveredProducts() >= orderItem.getQuantity()){
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
                deliveryManifestItem.setDeliveredQuantity(quantity);
                deliveryManifestItem.setUser(user);
                deliveryManifestItem.setUserId(user.getId());
                deliveryManifestItem.setUpdateDate(OffsetDateTime.now());
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
    public CompletableFuture<ResponseSuccess> markCollectedDeliveryManifestItem(
            UUID deliveryManifestItemId,
            Integer quantity,
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
                deliveryManifestItem.setQuantity(quantity);
                deliveryManifestItem.setUser(user);
                deliveryManifestItem.setUserId(user.getId());
                deliveryManifestItem.setUpdateDate(OffsetDateTime.now());
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
                ProductPrice productPrice = productPriceRepository.findByProductId(deliveryManifestItem.getProductId());
                Double totalPrice = null;
                if(Objects.equals(deliveryManifestItem.getOrderItem().getDiscount().getName(), "PORCENTAJE")){
                    totalPrice = (productPrice.getUnitSalePrice() * deliveryManifestItem.getOrderItem().getPreparedProducts())-((productPrice.getUnitSalePrice() * deliveryManifestItem.getOrderItem().getPreparedProducts())*(deliveryManifestItem.getOrderItem().getDiscountAmount()/100));
                }

                if(Objects.equals(deliveryManifestItem.getOrderItem().getDiscount().getName(), "MONTO")){
                    totalPrice = (productPrice.getUnitSalePrice() * deliveryManifestItem.getOrderItem().getPreparedProducts())-(deliveryManifestItem.getOrderItem().getDiscountAmount());
                }

                if(Objects.equals(deliveryManifestItem.getOrderItem().getDiscount().getName(), "NO APLICA")){
                    totalPrice = (productPrice.getUnitSalePrice() * deliveryManifestItem.getOrderItem().getPreparedProducts());
                }
                return DeliveryManifestItemDTO.builder()
                        .id(deliveryManifestItem.getId())
                        .deliveredQuantity(deliveryManifestItem.getDeliveredQuantity())
                        .collectedQuantity(deliveryManifestItem.getCollectedQuantity())
                        .phone(deliveryManifestItem.getOrderItem().getOrdering().getCustomer().getPhone())
                        .district(deliveryManifestItem.getOrderItem().getOrdering().getCustomer().getDistrict().getName())
                        .manifestNumber(deliveryManifestItem.getDeliveryManifest().getManifestNumber())
                        .orderNumber(deliveryManifestItem.getOrderItem().getOrdering().getOrderNumber())
                        .quantity(deliveryManifestItem.getQuantity())
                        .deliveredQuantity(deliveryManifestItem.getDeliveredQuantity())
                        .collectedQuantity(deliveryManifestItem.getCollectedQuantity())
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

    @Override
    public CompletableFuture<CourierProfileDTO> courierProfile(OffsetDateTime startDate, OffsetDateTime endDate, String username) {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Courier courier;
            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
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
                for(Object[] result : deliveredOrders){
                    UUID deliveryManifestId = (UUID) result[0];
                    UUID orderId = (UUID) result[1];
                    Long deliveredCount = (Long) result[2];
                    deliveredOrdersCountDTOS.add(DeliveredOrdersCountDTO.builder()
                                    .deliveredCount(deliveredCount)
                                    .orderId(orderId)
                                    .deliveredManifestId(deliveryManifestId)
                            .build());
                }
                Long deliveredOrderCount = 0L;
                for(DeliveredOrdersCountDTO deliveredOrdersCountDTO:deliveredOrdersCountDTOS){
                    deliveredOrderCount += deliveredOrdersCountDTO.getDeliveredCount();
                }
                List<DeliveryManifestItem> deliveredAndUnCollectedOrders = deliveryManifestItemRepository.findDeliveredAndUnCollectedOrders(courier.getId(),startDate,endDate);
                Double unCollectedAmount = 0.00;
                List<Ordering> orders = new ArrayList<>();
                Set<Long> uniqueOrderNumbers = new HashSet<>();
                for(DeliveryManifestItem deliveryManifestItem:deliveredAndUnCollectedOrders){
                    if(!uniqueOrderNumbers.contains(deliveryManifestItem.getOrderItem().getOrdering().getOrderNumber())){
                        uniqueOrderNumbers.add(deliveryManifestItem.getOrderItem().getOrdering().getOrderNumber());
                        orders.add(deliveryManifestItem.getOrderItem().getOrdering());
                    }
                    ProductPrice productPrice = productPriceRepository.findByProductId(deliveryManifestItem.getProductId());
                    Double totalPrice = null;
                    if(Objects.equals(deliveryManifestItem.getOrderItem().getDiscount().getName(), "PORCENTAJE")){
                        totalPrice = (productPrice.getUnitSalePrice() * deliveryManifestItem.getOrderItem().getPreparedProducts())-((productPrice.getUnitSalePrice() * deliveryManifestItem.getOrderItem().getPreparedProducts())*(deliveryManifestItem.getOrderItem().getDiscountAmount()/100));
                    }

                    if(Objects.equals(deliveryManifestItem.getOrderItem().getDiscount().getName(), "MONTO")){
                        totalPrice = (productPrice.getUnitSalePrice() * deliveryManifestItem.getOrderItem().getDeliveredProducts())-(deliveryManifestItem.getOrderItem().getDiscountAmount());
                    }

                    if(Objects.equals(deliveryManifestItem.getOrderItem().getDiscount().getName(), "NO APLICA")){
                        totalPrice = (productPrice.getUnitSalePrice() * deliveryManifestItem.getOrderItem().getDeliveredProducts());
                    }
                }
                for(Ordering order:orders){
                   if(!Objects.equals(order.getOrderPaymentState().getName(), "POR RECAUDAR")){
                       List<OrderItem> orderItems = orderItemRepository.findAllByOrderIdAndStatusTrue(order.getId());
                       double saleAmount = 0.00;

                       for(OrderItem orderItem : orderItems){
                           ProductPrice productPrice = productPriceRepository.findByProductIdAndStatusTrue(orderItem.getProductId());
                           if(Objects.equals(orderItem.getDiscount().getName(), "PORCENTAJE")) {
                               saleAmount += (productPrice.getUnitSalePrice() * orderItem.getPreparedProducts()) - ((productPrice.getUnitSalePrice() * orderItem.getPreparedProducts()) * (orderItem.getDiscountAmount() / 100));
                           }
                           if(Objects.equals(orderItem.getDiscount().getName(), "MONTO")){
                               saleAmount += (productPrice.getUnitSalePrice() * orderItem.getPreparedProducts()) - orderItem.getDiscountAmount();
                           }
                           if(Objects.equals(orderItem.getDiscount().getName(), "NO APLICA")){
                               saleAmount += (productPrice.getUnitSalePrice() * orderItem.getPreparedProducts());
                           }

                       }
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
                       unCollectedAmount+=totalDuePayment;
                   }
                }
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
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }


}
