package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.DeliveryManifestCourierDTO;
import com.proyect.masterdata.dto.DeliveryManifestDTO;
import com.proyect.masterdata.dto.DeliveryManifestItemDTO;
import com.proyect.masterdata.dto.DeliveryManifestOrderDTO;
import com.proyect.masterdata.dto.projections.DeliveryManifestItemProjection;
import com.proyect.masterdata.dto.request.RequestDeliveryManifest;
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
import org.springframework.transaction.annotation.Transactional;

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
    private final ProductRepository productRepository;
    private final OrderingRepository orderingRepository;
    private final CustomerRepository customerRepository;
    private final StockTransactionRepository stockTransactionRepository;
    private final StockTransactionItemRepository stockTransactionItemRepository;
    private final IStockTransactionItem iStockTransactionItem;
    private final DeliveryManifestOrderRepository deliveryManifestOrderRepository;
    @Override
    public CompletableFuture<ResponseSuccess> save(RequestDeliveryManifest requestDeliveryManifest) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Courier courier;
            Warehouse warehouse;
            try{
                System.out.println(requestDeliveryManifest);
                user = userRepository.findByUsernameAndStatusTrue(requestDeliveryManifest.getUsername().toUpperCase());
                courier = courierRepository.findByNameAndStatusTrue(requestDeliveryManifest.getCourier().toUpperCase());
                warehouse = warehouseRepository.findByNameAndStatusTrue(requestDeliveryManifest.getWarehouse().toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                e.printStackTrace();
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
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> addOrderDeliveryManifest(RequestDeliveryManifest requestDeliveryManifest, UUID deliveryManifestId,Long manifestNumber) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(() -> {
            User user;
            DeliveryManifest deliveryManifest;
            Warehouse warehouse;
            StockTransaction stockTransaction;
            StockTransactionItem stockTransactionItem;

            try {
                user = userRepository.findByUsernameAndStatusTrue(requestDeliveryManifest.getUsername().toUpperCase());
                deliveryManifest = deliveryManifestRepository.findById(deliveryManifestId).orElse(null);
                warehouse = warehouseRepository.findByNameAndStatusTrue(requestDeliveryManifest.getWarehouse().toUpperCase());

            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            try {
                System.out.println("Iterando en item de pedidos");
                List<OrderItem> orderItems = new ArrayList<>();
                for (UUID orderId:requestDeliveryManifest.getOrderUUIDs()){
                    orderItems = orderItemRepository.findOrderItemsForOrder(orderId);
                    if(orderItems.isEmpty()){
                        throw new BadRequestExceptions(Constants.ErrorDeliveryManifestNotItems);
                    }
                }

                List<RequestStockTransactionItem> stockTransactionList = new ArrayList<>();
                for(OrderItem orderItem:orderItems){
                    CompletableFuture<DeliveryManifestItem> deliveryManifestItem = iDeliveryManifestItem.save(orderItem,deliveryManifest,warehouse,user);
                    stockTransactionList.add(RequestStockTransactionItem.builder()
                            .productId(deliveryManifestItem.get().getProductId())
                            .quantity(deliveryManifestItem.get().getQuantity())
                            .build());
                }

                System.out.println("Stock item -> " + stockTransactionList);

                //iStockTransactionItem.save(stockTransaction,RequestStockTransactionItem.builder().build(),requestDeliveryManifest.getUsername() );
                System.out.println("Pasa el guardado de items en manifest");
                iStockTransaction.save(
                        "DMA"+deliveryManifest.getManifestNumber(),
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
                List<DeliveryManifestOrderDTO> deliveryManifestOrderDTOS = new ArrayList<>();
                List<DeliveryManifestItemDTO> deliveryManifestItemDTOS = deliveryManifestItemRepository.findAllByDeliveryManifestId(deliveryManifest.getId())
                        .stream().map(deliveryManifestItem -> {
                            if(!uniqueOrderNumbers.contains(deliveryManifestItem.getOrderItem().getOrdering().getOrderNumber())){
                                uniqueOrderNumbers.add(deliveryManifestItem.getOrderItem().getOrdering().getOrderNumber());
                                orders.add(deliveryManifestItem.getOrderItem().getOrdering());
                            }
                            ProductPrice productPrice = productPriceRepository.findByProductId(deliveryManifestItem.getOrderItem().getProductId());
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
                            productAmountPerManifest[0] += (productPrice.getUnitSalePrice() * deliveryManifestItem.getOrderItem().getPreparedProducts());
                            return DeliveryManifestItemDTO.builder()
                                    .id(deliveryManifestItem.getId())
                                    .deliveredQuantity(deliveryManifestItem.getDeliveredQuantity())
                                    .user(deliveryManifestItem.getUser().getUsername())
                                    .manifestNumber(deliveryManifestItem.getDeliveryManifest().getManifestNumber())
                                    .phone(deliveryManifestItem.getOrderItem().getOrdering().getCustomer().getPhone())
                                    .customer(deliveryManifestItem.getOrderItem().getOrdering().getCustomer().getName())
                                    .district(deliveryManifestItem.getOrderItem().getOrdering().getCustomer().getDistrict().getName())
                                    .orderNumber(deliveryManifestItem.getOrderItem().getOrdering().getOrderNumber())
                                    .quantity(deliveryManifestItem.getQuantity())
                                    .deliveredQuantity(deliveryManifestItem.getDeliveredQuantity())
                                    .collectedQuantity(deliveryManifestItem.getCollectedQuantity())
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
                    totalOrdersSaleAmount+=saleAmount;
                    totalOrdersDuePayment+=totalDuePayment;
                    DeliveryManifestOrderDTO deliveryManifestOrderDTO = DeliveryManifestOrderDTO.builder()
                            .address(order.getCustomer().getAddress())
                            .dni(order.getCustomer().getDni())
                            .customer(order.getCustomer().getName())
                            .orderNumber(order.getOrderNumber())
                            .deliveryManifestId(deliveryManifest.getId())
                            .orderState(order.getOrderState().getName())
                            .payableAmount(totalDuePayment)
                            .paymentMethod(order.getOrderPaymentMethod().getName())
                            .advancePayment(order.getAdvancedPayment())
                            .deliveryManifestItemDTOList(deliveryManifestItemDTOS.stream()
                                    .filter(item -> Objects.equals(item.getOrderNumber(), order.getOrderNumber()))
                                    .map(item -> {
                                        item.setOrderId(order.getId());
                                        return item;
                                    })
                                    .toList())
                            .orderId(order.getId())
                            .orderPaymentState(order.getOrderPaymentState().getName())
                            .phone(order.getCustomer().getPhone())
                            .district(order.getCustomer().getDistrict().getName())
                            .province(order.getCustomer().getDistrict().getProvince().getName())
                            .build();
                    DeliveryManifestOrder deliveryManifestOrder = deliveryManifestOrderRepository.findByDeliveryManifestIdAndOrderIdAndClientId(
                            deliveryManifest.getId(),
                            order.getId(),
                            user.getClientId()
                    );
                    if(deliveryManifestOrder!=null){
                        deliveryManifestOrderDTO.setReceivedAmount(deliveryManifestOrder.getReceivedAmount());
                        deliveryManifestOrderDTO.setObservations(deliveryManifestOrder.getObservations());
                        deliveryManifestOrderDTO.setDeliveryFeeCollected(deliveryManifestOrder.getDeliveryFeeCollected());
                        deliveryManifestOrderDTO.setPaymentMethod(deliveryManifestOrder.getOrderPaymentMethod().getName());
                        deliveryManifestOrderDTO.setDelivered(deliveryManifestOrder.getDelivered());
                    }else{
                        deliveryManifestOrderDTO.setReceivedAmount(0.00);
                        deliveryManifestOrderDTO.setObservations("Sin observaciones");
                        deliveryManifestOrderDTO.setDeliveryFeeCollected(false);
                        deliveryManifestOrderDTO.setPaymentMethod("SIN SELECCIONAR");
                        deliveryManifestOrderDTO.setDelivered(false);
                    }
                    deliveryManifestOrderDTOS.add(deliveryManifestOrderDTO);
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
                        .deliveryManifestOrderDTOS(deliveryManifestOrderDTOS)
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

    @Transactional
    @Override
    public CompletableFuture<ResponseSuccess> closeDeliveryManifest(UUID deliveryManifestId, String username) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            DeliveryManifest deliveryManifest;
            List<DeliveryManifestItem> deliveryManifestItems;
            List<OrderItem> orderItems = new ArrayList<>();

            try{
                System.out.println("id deliveru ->" + deliveryManifestId);
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                deliveryManifest = deliveryManifestRepository.findById(deliveryManifestId).orElse(null);
                deliveryManifestItems = deliveryManifestItemRepository.findAllById(deliveryManifest.getId());
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

            if(deliveryManifestItems.isEmpty()){
                for (DeliveryManifestItem deliveryManifestItem : deliveryManifestItems) {
                    orderItems.add(orderItemRepository.findOrderItemById(deliveryManifestItem.getOrderItemId()));
                }
            }

            try{
                deliveryManifest.setOpen(false);
                deliveryManifest.setUpdateDate(OffsetDateTime.now());
                deliveryManifest.setUser(user);
                deliveryManifest.setUserId(user.getId());
                //deliveryManifestRepository.save(deliveryManifest);


                List<DeliveryManifestItemProjection> deliveryManifestItemList = deliveryManifestItemRepository.findAllByDeliveryManifestIdAndClientId(deliveryManifest.getId(),user.getClientId());
                List<RequestStockTransactionItem> stockTransactionList = new ArrayList<>();
                boolean returnFlag = false;
                for(DeliveryManifestItemProjection deliveryManifestItem:deliveryManifestItemList){
                    if(deliveryManifestItem.getDeliveredQuantity()<1){
                        returnFlag = true;
                        int quantityReturn = deliveryManifestItem.getQuantity() - deliveryManifestItem.getDeliveredQuantity();
                        stockTransactionList.add(RequestStockTransactionItem.builder()
                                .productId(deliveryManifestItem.getProductId())
                                .quantity(quantityReturn)
                                .build());
                        Product product = productRepository.findByIdAndStatusTrue(deliveryManifestItem.getProductId());
                        iGeneralStock.in(
                                product,
                                quantityReturn,
                                user.getUsername()
                        );
                        iWarehouseStock.in(
                                deliveryManifest.getWarehouse(),
                                product,
                                quantityReturn,
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



                deliveryManifestRepository.closeDeliveriManifest(
                        deliveryManifest.getId(),
                        user.getId(),
                        false,
                        OffsetDateTime.now()
                );



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
            String courierDni,
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
                        courierDni,
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
            try {
                if(deliveryManifestPage.isEmpty()){
                    return new PageImpl<>(Collections.emptyList());
                }
                List<Ordering> orders = new ArrayList<>();
                Set<Long> uniqueOrderNumbers = new HashSet<>();
                double[] totalProductAmountPerManifest = {0.00};
                List<DeliveryManifestOrderDTO> deliveryManifestOrderDTOS = new ArrayList<>();
                List<DeliveryManifestDTO> deliveryManifestDTOS = deliveryManifestPage.getContent().stream().map(deliveryManifest -> {
                    double[] productAmountPerManifest = {0.00};

                    List<DeliveryManifestItemDTO> deliveryManifestItemDTOS = deliveryManifestItemRepository.findAllByDeliveryManifestIdAndClientId(deliveryManifest.getId(),clientId)
                            .stream().map(deliveryManifestItem -> {
                                if(!uniqueOrderNumbers.contains(deliveryManifestItem.getOrderNumber())){
                                    uniqueOrderNumbers.add(deliveryManifestItem.getOrderNumber());
                                    Ordering ordering = orderingRepository.findById(deliveryManifestItem.getOrderId()).orElse(null);
                                    orders.add(ordering);
                                }

                                ProductPrice productPrice = productPriceRepository.findByProductId(deliveryManifestItem.getProductId());
                                List<Object[]> orderItems = orderItemRepository.findOrderItemDetailsByIdAndClientId(deliveryManifestItem.getOrderItemId(),clientId);
                                Double totalPrice = null;
                                for(Object[] orderItem:orderItems){
                                    if(Objects.equals(orderItem[2], "PORCENTAJE")){
                                        totalPrice = (productPrice.getUnitSalePrice() * (Integer) orderItem[0])-((productPrice.getUnitSalePrice() * (Integer) orderItem[0])*((Double) orderItem[1]/100));
                                    }

                                    if(Objects.equals(orderItem[2], "MONTO")){
                                        totalPrice = (productPrice.getUnitSalePrice() * (Integer) orderItem[0])-((Double) orderItem[1]);
                                    }

                                    if(Objects.equals(orderItem[2], "NO APLICA")){
                                        totalPrice = (productPrice.getUnitSalePrice() * (Integer) orderItem[0]);
                                    }
                                    productAmountPerManifest[0] += (productPrice.getUnitSalePrice() * (Integer) orderItem[0]);
                                }
                                return DeliveryManifestItemDTO.builder()
                                        .id(deliveryManifestItem.getDeliveryManifestItemId())
                                        .orderId(deliveryManifestItem.getOrderId())
                                        .user(deliveryManifestItem.getUsername())
                                        .manifestNumber(deliveryManifestItem.getManifestNumber())
                                        .phone(deliveryManifestItem.getPhone())
                                        .customer(deliveryManifestItem.getCustomerName())
                                        .district(deliveryManifestItem.getDistrictName())
                                        .orderNumber(deliveryManifestItem.getOrderNumber())
                                        .deliveredQuantity(deliveryManifestItem.getDeliveredQuantity())
                                        .quantity(deliveryManifestItem.getQuantity())
                                        .collectedQuantity(deliveryManifestItem.getCollectedQuantity())
                                        .skuProduct(iUtil.buildProductSku(productPrice.getProduct()))
                                        .management(deliveryManifestItem.getManagementType())
                                        .paymentMethod(deliveryManifestItem.getPaymentMethod())
                                        .paymentState(deliveryManifestItem.getPaymentState())
                                        .orderItemAmount(totalPrice)
                                        .product(productPrice.getProduct().getName())
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
                        totalOrdersSaleAmount+=saleAmount;
                        totalOrdersDuePayment+=totalDuePayment;

                        DeliveryManifestOrderDTO deliveryManifestOrderDTO = DeliveryManifestOrderDTO.builder()
                                .address(order.getCustomer().getAddress())
                                .dni(order.getCustomer().getDni())
                                .customer(order.getCustomer().getName())
                                .deliveryManifestId(deliveryManifest.getId())
                                .orderNumber(order.getOrderNumber())
                                .orderState(order.getOrderState().getName())
                                .payableAmount(totalDuePayment)
                                .paymentMethod(order.getOrderPaymentMethod().getName())
                                .advancePayment(order.getAdvancedPayment())
                                .deliveryManifestItemDTOList(deliveryManifestItemDTOS.stream()
                                        .filter(item -> Objects.equals(item.getOrderNumber(), order.getOrderNumber())).toList())
                                .orderId(order.getId())
                                .orderPaymentState(order.getOrderPaymentState().getName())
                                .phone(order.getCustomer().getPhone())
                                .district(order.getCustomer().getDistrict().getName())
                                .province(order.getCustomer().getDistrict().getProvince().getName())
                                .build();
                        DeliveryManifestOrder deliveryManifestOrder = deliveryManifestOrderRepository.findByDeliveryManifestIdAndOrderIdAndClientId(
                                deliveryManifest.getId(),
                                order.getId(),
                                order.getClientId()
                        );

                        if(deliveryManifestOrder!=null){
                            deliveryManifestOrderDTO.setReceivedAmount(deliveryManifestOrder.getReceivedAmount());
                            deliveryManifestOrderDTO.setObservations(deliveryManifestOrder.getObservations());
                            deliveryManifestOrderDTO.setDeliveryFeeCollected(deliveryManifestOrder.getDeliveryFeeCollected());
                            deliveryManifestOrderDTO.setPaymentMethod(deliveryManifestOrder.getOrderPaymentMethod().getName());
                            deliveryManifestOrderDTO.setDelivered(deliveryManifestOrder.getDelivered());
                        }else{
                            deliveryManifestOrderDTO.setReceivedAmount(0.00);
                            deliveryManifestOrderDTO.setObservations("Sin observaciones");
                            deliveryManifestOrderDTO.setDeliveryFeeCollected(false);
                            deliveryManifestOrderDTO.setPaymentMethod("SIN SELECCIONAR");
                            deliveryManifestOrderDTO.setDelivered(false);
                        }
                        if(!deliveryManifestOrderDTO.getDeliveryManifestItemDTOList().isEmpty()){ // porque se esta obteniendo un delivery manifest con imtens vacios
                            deliveryManifestOrderDTOS.add(deliveryManifestOrderDTO);
                        }

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
                            .deliveryManifestOrderDTOS(deliveryManifestOrderDTOS.stream().filter(order -> order.getDeliveryManifestId().equals(deliveryManifest.getId())).toList())
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
            }catch (RuntimeException e){
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
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
            StockTransaction stockTransaction;

            List<DeliveryManifestItem> deliveryManifestItemLis = new ArrayList<>();
            try{
                courier = courierRepository.findById(courierId).get();
                user = userRepository.findByDni(courier.getDni());
                deliveryManifest = deliveryManifestRepository.findByCourierIdAndOpenTrue(courierId);

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
                //stockTransaction = stockTransactionRepository.findByWarehouseId(deliveryManifest.getWarehouse().getId());
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
                delivered += deliveryManifestItem.getDeliveredQuantity();
            }

            quantityOrders = deliveryManifestItemLis.size();

            return DeliveryManifestCourierDTO.builder()
                    .deliveryManifestId(deliveryManifest.getId())
                    .courierId(courier.getId())
                    //.stockTransactionId(stockTransaction.getId())
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

    @Override
    public CompletableFuture<DeliveryManifestDTO> getLastDeliveryManifestByCourier(String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Courier courier;
            DeliveryManifest lastDeliveryManifest;
            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
            }catch (RuntimeException e){
                e.printStackTrace();
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
            }else{
                lastDeliveryManifest = deliveryManifestRepository
                        .findLatestByCourier(courier.getId(), PageRequest.of(0, 1))
                        .stream()
                        .findFirst()
                        .orElse(null);
            }
            if(lastDeliveryManifest==null){
                throw new BadRequestExceptions(Constants.ErrorDeliveryManifestNoResults);
            }
            try {
                List<Ordering> orders = new ArrayList<>();
                Set<Long> uniqueOrderNumbers = new HashSet<>();
                double[] productAmountPerManifest = {0.00};
                List<DeliveryManifestOrderDTO> deliveryManifestOrderDTOS = new ArrayList<>();
                List<DeliveryManifestItemDTO> deliveryManifestItemDTOS = deliveryManifestItemRepository.findAllByDeliveryManifestId(lastDeliveryManifest.getId())
                        .stream().map(deliveryManifestItem -> {
                            if(!uniqueOrderNumbers.contains(deliveryManifestItem.getOrderItem().getOrdering().getOrderNumber())){
                                uniqueOrderNumbers.add(deliveryManifestItem.getOrderItem().getOrdering().getOrderNumber());
                                orders.add(deliveryManifestItem.getOrderItem().getOrdering());
                            }
                            ProductPrice productPrice = productPriceRepository.findByProductId(deliveryManifestItem.getOrderItem().getProductId());
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
                                    .deliveredQuantity(deliveryManifestItem.getDeliveredQuantity())
                                    .collectedQuantity(deliveryManifestItem.getCollectedQuantity())
                                    .manifestNumber(deliveryManifestItem.getDeliveryManifest().getManifestNumber())
                                    .phone(deliveryManifestItem.getOrderItem().getOrdering().getCustomer().getPhone())
                                    .customer(deliveryManifestItem.getOrderItem().getOrdering().getCustomer().getName())
                                    .district(deliveryManifestItem.getOrderItem().getOrdering().getCustomer().getDistrict().getName())
                                    .orderNumber(deliveryManifestItem.getOrderItem().getOrdering().getOrderNumber())
                                    .quantity(deliveryManifestItem.getQuantity())
                                    .deliveredQuantity(deliveryManifestItem.getDeliveredQuantity())
                                    .collectedQuantity(deliveryManifestItem.getCollectedQuantity())
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
                    totalOrdersSaleAmount+=saleAmount;
                    totalOrdersDuePayment+=totalDuePayment;

                    DeliveryManifestOrderDTO deliveryManifestOrderDTO = DeliveryManifestOrderDTO.builder()
                            .address(order.getCustomer().getAddress())
                            .dni(order.getCustomer().getDni())
                            .customer(order.getCustomer().getName())
                            .orderNumber(order.getOrderNumber())
                            .orderState(order.getOrderState().getName())
                            .payableAmount(totalDuePayment)
                            .paymentMethod(order.getOrderPaymentMethod().getName())
                            .advancePayment(order.getAdvancedPayment())
                            .deliveryManifestItemDTOList(deliveryManifestItemDTOS.stream()
                                    .filter(item -> Objects.equals(item.getOrderNumber(), order.getOrderNumber()))
                                    .map(item -> {
                                        item.setOrderId(order.getId());
                                        return item;
                                    })
                                    .toList())
                            .orderId(order.getId())
                            .orderPaymentState(order.getOrderPaymentState().getName())
                            .phone(order.getCustomer().getPhone())
                            .district(order.getCustomer().getDistrict().getName())
                            .province(order.getCustomer().getDistrict().getProvince().getName())
                            .build();

                    DeliveryManifestOrder deliveryManifestOrder = deliveryManifestOrderRepository.findByDeliveryManifestIdAndOrderIdAndClientId(
                            lastDeliveryManifest.getId(),
                            order.getId(),
                            user.getClientId()
                    );
                    if(deliveryManifestOrder!=null){
                        deliveryManifestOrderDTO.setReceivedAmount(deliveryManifestOrder.getReceivedAmount());
                        deliveryManifestOrderDTO.setObservations(deliveryManifestOrder.getObservations());
                        deliveryManifestOrderDTO.setDeliveryFeeCollected(deliveryManifestOrder.getDeliveryFeeCollected());
                        deliveryManifestOrderDTO.setPaymentMethod(deliveryManifestOrder.getOrderPaymentMethod().getName());
                        deliveryManifestOrderDTO.setDelivered(deliveryManifestOrder.getDelivered());
                    }else{
                        deliveryManifestOrderDTO.setReceivedAmount(0.00);
                        deliveryManifestOrderDTO.setObservations("Sin observaciones");
                        deliveryManifestOrderDTO.setDeliveryFeeCollected(false);
                        deliveryManifestOrderDTO.setPaymentMethod("SIN SELECCIONAR");
                        deliveryManifestOrderDTO.setDelivered(false);
                    }
                    deliveryManifestOrderDTOS.add(deliveryManifestOrderDTO);
                }
                return DeliveryManifestDTO.builder()
                        .id(lastDeliveryManifest.getId())
                        .user(lastDeliveryManifest.getUser().getUsername())
                        .manifestNumber(lastDeliveryManifest.getManifestNumber())
                        .id(lastDeliveryManifest.getId())
                        .open(lastDeliveryManifest.getOpen())
                        .courier(lastDeliveryManifest.getCourier().getName())
                        .warehouse(lastDeliveryManifest.getWarehouse().getName())
                        .registrationDate(lastDeliveryManifest.getRegistrationDate())
                        .updateDate(lastDeliveryManifest.getUpdateDate())
                        .deliveryManifestOrderDTOS(deliveryManifestOrderDTOS)
                        .pickupAddress(lastDeliveryManifest.getWarehouse().getAddress())
                        .amount(totalOrdersSaleAmount)
                        .paidAmount(totalOrdersSaleAmount-totalOrdersDuePayment)
                        .payableAmount(totalOrdersDuePayment)
                        .observations(lastDeliveryManifest.getObservations())
                        .courierPhone(lastDeliveryManifest.getCourier().getPhone())
                        .courierPlate(lastDeliveryManifest.getCourier().getPlate())
                        .productValue(productAmountPerManifest[0])
                        .build();
            }catch (RuntimeException e){
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }
}
