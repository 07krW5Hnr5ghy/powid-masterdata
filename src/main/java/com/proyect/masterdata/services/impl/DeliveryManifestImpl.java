package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.DeliveryManifestCourierDTO;
import com.proyect.masterdata.dto.DeliveryManifestDTO;
import com.proyect.masterdata.dto.DeliveryManifestItemDTO;
import com.proyect.masterdata.dto.request.RequestDeliveryManifest;
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
import java.util.concurrent.ExecutionException;

@Service
@RequiredArgsConstructor
@Log4j2
public class DeliveryManifestImpl implements IDeliveryManifest {
    private final UserRepository userRepository;
    private final DeliveryManifestRepository deliveryManifestRepository;
    private final CourierRepository courierRepository;
    private final IDeliveryManifestItem iDeliveryManifestItem;
    private final WarehouseRepository warehouseRepository;
    private final DeliveryManifestItemRepository deliveryManifestItemRepository;
    private final IUtil iUtil;
    private final IAudit iAudit;
    private final DeliveryManifestRepositoryCustom deliveryManifestRepositoryCustom;
    private final IGeneralStock iGeneralStock;
    private final IWarehouseStock iWarehouseStock;
    private final IStockTransaction iStockTransaction;
    private final OrderItemRepository orderItemRepository;
    private final ProductPriceRepository productPriceRepository;
    @Override
    public CompletableFuture<ResponseSuccess> save(RequestDeliveryManifest requestDeliveryManifest) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Courier courier;
            Warehouse warehouse;
            try{
                user = userRepository.findByUsernameAndStatusTrue(requestDeliveryManifest.getUsername().toUpperCase());
                courier = courierRepository.findByNameAndStatusTrue(requestDeliveryManifest.getCourier().toUpperCase());
                warehouse = warehouseRepository.findByNameAndStatusTrue(requestDeliveryManifest.getWarehouse().toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(courier==null){
                throw new BadRequestExceptions(Constants.ErrorCourier);
            }
            if(warehouse==null){
                throw new BadRequestExceptions(Constants.ErrorWarehouse);
            }
            try{
                Long deliveryManifestNumber = deliveryManifestRepository.countByClientId(user.getClientId()) + 1L;
                List<OrderItem> orderItems = new ArrayList<>();
                for(UUID orderId:requestDeliveryManifest.getOrderUUIDs()){
                    orderItems = orderItemRepository.findOrderItemsForOrder(orderId);
                    if(orderItems.isEmpty()){
                        throw new BadRequestExceptions(Constants.ErrorDeliveryManifestNotItems);
                    }
                }
                DeliveryManifest deliveryManifest = deliveryManifestRepository.save(DeliveryManifest.builder()
                                .manifestNumber(deliveryManifestNumber)
                                .courierId(courier.getId())
                                .courier(courier)
                                .registrationDate(OffsetDateTime.now())
                                .updateDate(OffsetDateTime.now())
                                .open(true)
                                .user(user)
                                .userId(user.getId())
                                .client(user.getClient())
                                .clientId(user.getClientId())
                                .warehouse(warehouse)
                                .warehouseId(warehouse.getId())
                                .observations(requestDeliveryManifest.getObservations())
                        .build());
                System.out.println("delivery manifest -> " + deliveryManifest.getManifestNumber()+ " " + deliveryManifest.getWarehouse().getName());
                List<RequestStockTransactionItem> stockTransactionList = new ArrayList<>();
                for(OrderItem orderItem:orderItems){
                    CompletableFuture<DeliveryManifestItem> deliveryManifestItem = iDeliveryManifestItem.save(orderItem,deliveryManifest,warehouse,user);
                    stockTransactionList.add(RequestStockTransactionItem.builder()
                            .productId(deliveryManifestItem.get().getProductId())
                            .quantity(deliveryManifestItem.get().getQuantity())
                            .build());
                }
                iStockTransaction.save(
                        "DM"+deliveryManifest.getManifestNumber(),
                        deliveryManifest.getWarehouse(),
                        stockTransactionList,
                        "GUIA-COURIER",
                        user);
                iAudit.save(
                        "ADD_DELIVERY_MANIFEST",
                        "GUIA "+
                                deliveryManifest.getManifestNumber()+
                                " CREADO.",
                        deliveryManifest.getManifestNumber().toString(),user.getUsername());
                return ResponseSuccess.builder()
                        .message(Constants.register)
                        .code(200)
                        .build();
            }catch (RuntimeException | InterruptedException | ExecutionException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<DeliveryManifestDTO> getById(UUID deliveryManifestId,String username) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            DeliveryManifest deliveryManifest;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                deliveryManifest = deliveryManifestRepository.findById(deliveryManifestId).orElse(null);
            }catch (RuntimeException e){
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(deliveryManifest==null){
                throw new BadRequestExceptions(Constants.ErrorDeliveryStatusExists);
            }
            try{
                List<Ordering> orders = new ArrayList<>();
                Set<Long> uniqueOrderNumbers = new HashSet<>();
                double[] productAmountPerManifest = {0.00};
                List<DeliveryManifestItemDTO> deliveryManifestItemDTOS = deliveryManifestItemRepository.findAllById(deliveryManifest.getId())
                        .stream().map(deliveryManifestItem -> {
                            if(!uniqueOrderNumbers.contains(deliveryManifestItem.getOrderItem().getOrdering().getOrderNumber())){
                                uniqueOrderNumbers.add(deliveryManifestItem.getOrderItem().getOrdering().getOrderNumber());
                                orders.add(deliveryManifestItem.getOrderItem().getOrdering());
                            }
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
                            productAmountPerManifest[0] += (productPrice.getUnitSalePrice() * deliveryManifestItem.getOrderItem().getQuantity());
                            return DeliveryManifestItemDTO.builder()
                                    .id(deliveryManifestItem.getId())
                                    .user(deliveryManifestItem.getUser().getUsername())
                                    .manifestNumber(deliveryManifestItem.getDeliveryManifest().getManifestNumber())
                                    .phone(deliveryManifestItem.getOrderItem().getOrdering().getCustomer().getPhone())
                                    .customer(deliveryManifestItem.getOrderItem().getOrdering().getCustomer().getName())
                                    .district(deliveryManifestItem.getOrderItem().getOrdering().getCustomer().getDistrict().getName())
                                    .orderNumber(deliveryManifestItem.getOrderItem().getOrdering().getOrderNumber())
                                    .quantity(deliveryManifestItem.getQuantity())
                                    .skuProduct(iUtil.buildProductSku(deliveryManifestItem.getProduct()))
                                    .management(deliveryManifestItem.getOrderItem().getOrdering().getManagementType().getName())
                                    .paymentMethod(deliveryManifestItem.getOrderItem().getOrdering().getOrderPaymentMethod().getName())
                                    .paymentState(deliveryManifestItem.getOrderItem().getOrdering().getOrderPaymentState().getName())
                                    .orderItemAmount(totalPrice)
                                    .product(deliveryManifestItem.getProduct().getName())
                                    .build();
                        }).toList();
                double totalOrdersSaleAmount = 0.00;
                double totalOrdersDuePayment = 0.00;
                for(Ordering order:orders){
                    List<OrderItem> orderItems = orderItemRepository.findAllByOrderIdAndStatusTrue(order.getId());
                    double saleAmount = 0.00;
                    for(OrderItem orderItem : orderItems){
                        ProductPrice productPrice = productPriceRepository.findByProductIdAndStatusTrue(orderItem.getProductId());
                        if(Objects.equals(orderItem.getDiscount().getName(), "PORCENTAJE")) {
                            saleAmount += (productPrice.getUnitSalePrice() * orderItem.getQuantity()) - ((productPrice.getUnitSalePrice() * orderItem.getQuantity()) * (orderItem.getDiscountAmount() / 100));
                        }
                        if(Objects.equals(orderItem.getDiscount().getName(), "MONTO")){
                            saleAmount += (productPrice.getUnitSalePrice() * orderItem.getQuantity()) - orderItem.getDiscountAmount();
                        }
                        if(Objects.equals(orderItem.getDiscount().getName(), "NO APLICA")){
                            saleAmount += (productPrice.getUnitSalePrice() * orderItem.getQuantity());
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
                    totalOrdersSaleAmount+=saleAmount;
                    totalOrdersDuePayment+=totalDuePayment;
                }
                return DeliveryManifestDTO.builder()
                        .id(deliveryManifest.getId())
                        .user(deliveryManifest.getUser().getUsername())
                        .manifestNumber(deliveryManifest.getManifestNumber())
                        .id(deliveryManifest.getId())
                        .open(deliveryManifest.getOpen())
                        .courier(deliveryManifest.getCourier().getName())
                        .warehouse(deliveryManifest.getWarehouse().getName())
                        .registrationDate(deliveryManifest.getRegistrationDate())
                        .updateDate(deliveryManifest.getUpdateDate())
                        .deliveryManifestItemDTOS(deliveryManifestItemDTOS)
                        .pickupAddress(deliveryManifest.getWarehouse().getAddress())
                        .amount(totalOrdersSaleAmount)
                        .paidAmount(totalOrdersSaleAmount-totalOrdersDuePayment)
                        .payableAmount(totalOrdersDuePayment)
                        .observations(deliveryManifest.getObservations())
                        .courierPhone(deliveryManifest.getCourier().getPhone())
                        .courierPlate(deliveryManifest.getCourier().getPlate())
                        .productValue(productAmountPerManifest[0])
                        .build();
            }catch (RuntimeException e){
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }
    @Override
    public CompletableFuture<ResponseSuccess> closeDeliveryManifest(UUID deliveryManifestId, String username) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            DeliveryManifest deliveryManifest;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                deliveryManifest = deliveryManifestRepository.findById(deliveryManifestId).orElse(null);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(deliveryManifest==null){
                throw new BadRequestExceptions(Constants.ErrorDeliveryManifest);
            }
            try{
                deliveryManifest.setOpen(false);
                deliveryManifest.setUpdateDate(OffsetDateTime.now());
                deliveryManifest.setUser(user);
                deliveryManifest.setUserId(user.getId());
                deliveryManifestRepository.save(deliveryManifest);
                List<DeliveryManifestItem> deliveryManifestItemList = deliveryManifestItemRepository.findAllById(deliveryManifest.getId());
                List<RequestStockTransactionItem> stockTransactionList = new ArrayList<>();
                boolean returnFlag = false;
                for(DeliveryManifestItem deliveryManifestItem:deliveryManifestItemList){
                    if(!deliveryManifestItem.getDelivered()){
                        returnFlag = true;
                        stockTransactionList.add(RequestStockTransactionItem.builder()
                                .productId(deliveryManifestItem.getProduct().getId())
                                .quantity(deliveryManifestItem.getQuantity())
                                .build());
                        iGeneralStock.in(
                                deliveryManifestItem.getProduct(),
                                deliveryManifestItem.getQuantity(),
                                user.getUsername()
                        );
                        iWarehouseStock.in(
                                deliveryManifest.getWarehouse(),
                                deliveryManifestItem.getProduct(),
                                deliveryManifestItem.getQuantity(),
                                user
                        );
                    }
                }
                if(returnFlag){
                    iStockTransaction.save(
                            "DMR"+deliveryManifest.getManifestNumber(),
                            deliveryManifest.getWarehouse(),
                            stockTransactionList,
                            "GUIA-COURIER-DEVOLUCION",
                            user);
                }
                iAudit.save(
                        "DELETE_DELIVERY_MANIFEST",
                        "GUIA "+
                                deliveryManifest.getManifestNumber()+
                                " CERRADA.",
                        deliveryManifest.getManifestNumber().toString(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.update)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<DeliveryManifestDTO>> list(
            String user,
            Long manifestNumber,
            String warehouse,
            String courier,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean open) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<DeliveryManifest> deliveryManifestPage;
            UUID clientId;
            try{
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                deliveryManifestPage = deliveryManifestRepositoryCustom.searchForDeliveryManifest(
                        clientId,
                        manifestNumber,
                        warehouse,
                        courier,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateEndDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        open
                );
            }catch (RuntimeException e){
                log.error(e.getMessage());
                e.printStackTrace();
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if(deliveryManifestPage.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }
            List<Ordering> orders = new ArrayList<>();
            Set<Long> uniqueOrderNumbers = new HashSet<>();
            double[] totalProductAmountPerManifest = {0.00};
            List<DeliveryManifestDTO> deliveryManifestDTOS = deliveryManifestPage.getContent().stream().map(deliveryManifest -> {
                double[] productAmountPerManifest = {0.00};
                List<DeliveryManifestItemDTO> deliveryManifestItemDTOS = deliveryManifestItemRepository.findAllByDeliveryManifestId(deliveryManifest.getId())
                        .stream().map(deliveryManifestItem -> {
                            if(!uniqueOrderNumbers.contains(deliveryManifestItem.getOrderItem().getOrdering().getOrderNumber())){
                                uniqueOrderNumbers.add(deliveryManifestItem.getOrderItem().getOrdering().getOrderNumber());
                                orders.add(deliveryManifestItem.getOrderItem().getOrdering());
                            }
                            ProductPrice productPrice = productPriceRepository.findByProductId(deliveryManifestItem.getProduct().getId());
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
                            productAmountPerManifest[0] += (productPrice.getUnitSalePrice() * deliveryManifestItem.getOrderItem().getQuantity());
                            return DeliveryManifestItemDTO.builder()
                                    .id(deliveryManifestItem.getId())
                                    .user(deliveryManifestItem.getUser().getUsername())
                                    .manifestNumber(deliveryManifestItem.getDeliveryManifest().getManifestNumber())
                                    .phone(deliveryManifestItem.getOrderItem().getOrdering().getCustomer().getPhone())
                                    .customer(deliveryManifestItem.getOrderItem().getOrdering().getCustomer().getName())
                                    .district(deliveryManifestItem.getOrderItem().getOrdering().getCustomer().getDistrict().getName())
                                    .orderNumber(deliveryManifestItem.getOrderItem().getOrdering().getOrderNumber())
                                    .quantity(deliveryManifestItem.getQuantity())
                                    .delivered(deliveryManifestItem.isDelivered())
                                    .skuProduct(iUtil.buildProductSku(deliveryManifestItem.getProduct()))
                                    .management(deliveryManifestItem.getOrderItem().getOrdering().getManagementType().getName())
                                    .paymentMethod(deliveryManifestItem.getOrderItem().getOrdering().getOrderPaymentMethod().getName())
                                    .paymentState(deliveryManifestItem.getOrderItem().getOrdering().getOrderPaymentState().getName())
                                    .orderItemAmount(totalPrice)
                                    .product(deliveryManifestItem.getProduct().getName())
                                    .build();
                        }).toList();
                totalProductAmountPerManifest[0]+=productAmountPerManifest[0];
                double totalOrdersSaleAmount = 0.00;
                double totalOrdersDuePayment = 0.00;

                for(Ordering order:orders){
                    List<OrderItem> orderItems = orderItemRepository.findAllByOrderIdAndStatusTrue(order.getId());
                    double saleAmount = 0.00;

                    for(OrderItem orderItem : orderItems){
                        ProductPrice productPrice = productPriceRepository.findByProductIdAndStatusTrue(orderItem.getProductId());
                        if(Objects.equals(orderItem.getDiscount().getName(), "PORCENTAJE")) {
                            saleAmount += (productPrice.getUnitSalePrice() * orderItem.getQuantity()) - ((productPrice.getUnitSalePrice() * orderItem.getQuantity()) * (orderItem.getDiscountAmount() / 100));
                        }
                        if(Objects.equals(orderItem.getDiscount().getName(), "MONTO")){
                            saleAmount += (productPrice.getUnitSalePrice() * orderItem.getQuantity()) - orderItem.getDiscountAmount();
                        }
                        if(Objects.equals(orderItem.getDiscount().getName(), "NO APLICA")){
                            saleAmount += (productPrice.getUnitSalePrice() * orderItem.getQuantity());
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
                    totalOrdersSaleAmount+=saleAmount;
                    totalOrdersDuePayment+=totalDuePayment;
                }
                return DeliveryManifestDTO.builder()
                        .id(deliveryManifest.getId())
                        .user(deliveryManifest.getUser().getUsername())
                        .manifestNumber(deliveryManifest.getManifestNumber())
                        .id(deliveryManifest.getId())
                        .open(deliveryManifest.getOpen())
                        .courier(deliveryManifest.getCourier().getName())
                        .warehouse(deliveryManifest.getWarehouse().getName())
                        .registrationDate(deliveryManifest.getRegistrationDate())
                        .updateDate(deliveryManifest.getUpdateDate())
                        .deliveryManifestItemDTOS(deliveryManifestItemDTOS)
                        .pickupAddress(deliveryManifest.getWarehouse().getAddress())
                        .amount(totalOrdersSaleAmount)
                        .paidAmount(totalOrdersSaleAmount-totalOrdersDuePayment)
                        .payableAmount(totalOrdersDuePayment)
                        .observations(deliveryManifest.getObservations())
                        .courierPhone(deliveryManifest.getCourier().getPhone())
                        .courierPlate(deliveryManifest.getCourier().getPlate())
                        .productValue(totalProductAmountPerManifest[0])
                        .build();
            }).toList();
            return new PageImpl<>(deliveryManifestDTOS,deliveryManifestPage.getPageable(),deliveryManifestPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<DeliveryManifestCourierDTO> checkCourierToDeliveryManifest(UUID courierId) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Courier courier;
            DeliveryManifest deliveryManifest = null;
            User user;
            Integer paid,receivable;
            Integer delivered = 0;
            Integer quantityOrders = 0;

            List<DeliveryManifestItem> deliveryManifestItemLis = new ArrayList<>();
            try{
                courier = courierRepository.findById(courierId).get();
                user = userRepository.findByDni(courier.getDni());

                deliveryManifest = deliveryManifestRepository.findByCourierId(courierId);
                if(deliveryManifest==null){
                    return DeliveryManifestCourierDTO.builder()
                            .warehouse("Not found warehouse")
                            .courierId(courierId)
                            .manifestNumber(0L)
                            .open(false)
                            .paid(0)
                            .receivable(0)
                            .quantityOrders(0)
                            .delivered(0)
                            .observations("Not found observations")
                            .registrationDate(courier.getRegistrationDate())
                            .updateDate(courier.getUpdateDate())
                            .isExists(false)
                            .build();
                }

                deliveryManifestItemLis = deliveryManifestItemRepository.findAllById(deliveryManifest.getId());
            }catch (RuntimeException e){
                e.printStackTrace();
                throw new InternalErrorExceptions(e.getMessage());
            }

            if(courier==null){
                throw new InternalErrorExceptions("Courier not found");
            }

            if(user==null){
                throw new InternalErrorExceptions("User not found");
            }

            for(DeliveryManifestItem deliveryManifestItem : deliveryManifestItemLis){
                if(deliveryManifestItem.getDelivered())
                    delivered++;
            }

            quantityOrders = deliveryManifestItemLis.size();

            return DeliveryManifestCourierDTO.builder()
                    .deliveryManifestId(deliveryManifest.getId())
                    .courierId(courier.getId())
                    .manifestNumber(deliveryManifest.getManifestNumber())
                    .warehouse(deliveryManifest.getWarehouse().getName())
                    .open(deliveryManifest.getOpen())
                    .isExists(true)
                    .paid(0)
                    .quantityOrders(quantityOrders)
                    .receivable(0)
                    .delivered(delivered)
                    .observations(deliveryManifest.getObservations())
                    .registrationDate(deliveryManifest.getRegistrationDate())
                    .updateDate(deliveryManifest.getUpdateDate())
                    .build();
        });
    }
}
