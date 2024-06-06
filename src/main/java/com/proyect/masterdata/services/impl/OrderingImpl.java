package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.OrderItemDTO;
import com.proyect.masterdata.dto.OrderDTO;
import com.proyect.masterdata.dto.ProductDTO;
import com.proyect.masterdata.dto.request.*;
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

import java.util.*;
import java.util.concurrent.CompletableFuture;

@RequiredArgsConstructor
@Service
@Log4j2
public class OrderingImpl implements IOrdering {

    private final UserRepository userRepository;
    private final OrderingRepository orderingRepository;
    private final OrderStateRepository orderStateRepository;
    private final ISale iSale;
    private final ICustomer iCustomer;
    private final IOrderItem iOrderItem;
    private final OrderingRepositoryCustom orderingRepositoryCustom;
    private final SaleRepository saleRepository;
    private final CustomerRepository customerRepository;
    private final OrderItemRepository orderItemRepository;
    private final ProductPriceRepository productPriceRepository;
    private final ProductRepository productRepository;
    private final OrderPaymentMethodRepository orderPaymentMethodRepository;
    private final OrderPaymentStateRepository orderPaymentStateRepository;
    private final CourierRepository courierRepository;
    private final OrderStockItemRepository orderStockItemRepository;
    private final IWarehouseStock iWarehouseStock;
    private final IGeneralStock iGeneralStock;
    private final IOrderPaymentReceipt iOrderPaymentReceipt;
    private final OrderPaymentReceiptRepository orderPaymentReceiptRepository;
    private final ICourierPicture iCourierPicture;
    private final IStockTransaction iStockTransaction;
    private final OrderStockRepository orderStockRepository;
    private final SaleChannelRepository saleChannelRepository;
    private final ManagementTypeRepository managementTypeRepository;
    private final CourierPictureRepository courierPictureRepository;
    private final ProductPictureRepository productPictureRepository;
    private final StoreRepository storeRepository;
    private final ClosingChannelRepository closingChannelRepository;
    private final CustomerTypeRepository customerTypeRepository;
    private final IAudit iAudit;
    @Override
    public ResponseSuccess save(RequestOrderSave requestOrderSave, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        OrderState orderState;
        Courier courier;
        OrderPaymentState orderPaymentState;
        SaleChannel saleChannel;
        ManagementType managementType;
        OrderPaymentMethod orderPaymentMethod;
        Store store;
        ClosingChannel closingChannel;

        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            orderState = orderStateRepository.findByNameAndStatusTrue("PENDIENTE");
            courier = courierRepository.findByNameAndStatusTrue("SIN COURIER");
            orderPaymentState = orderPaymentStateRepository.findByNameAndStatusTrue("POR RECAUDAR");
            saleChannel = saleChannelRepository.findByNameAndStatusTrue(requestOrderSave.getSaleChannel().toUpperCase());
            managementType = managementTypeRepository.findByNameAndStatusTrue(requestOrderSave.getManagementType().toUpperCase());
            orderPaymentMethod = orderPaymentMethodRepository.findByNameAndStatusTrue(requestOrderSave.getPaymentMethod().toUpperCase());
            store = storeRepository.findByNameAndStatusTrue(requestOrderSave.getStoreName().toUpperCase());
            closingChannel = closingChannelRepository.findByNameAndStatusTrue(requestOrderSave.getClosingChannel().toUpperCase());
        }catch (RuntimeException e){
            e.printStackTrace();
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(saleChannel == null){
            throw new BadRequestExceptions(Constants.ErrorSaleChannel);
        }

        if(orderPaymentMethod == null){
            throw new BadRequestExceptions(Constants.ErrorPaymentMethod);
        }

        if(managementType == null){
            throw new BadRequestExceptions(Constants.ErrorManagementType);
        }

        if(store == null){
            throw new BadRequestExceptions(Constants.ErrorStore);
        }

        if(closingChannel == null){
            throw new BadRequestExceptions(Constants.ErrorClosingChannel);
        }

        try{
            requestOrderSave.getRequestOrderItems().forEach(requestOrderItem -> {
                Product product = productRepository.findBySkuAndStatusTrue(requestOrderItem.getProductSku().toUpperCase());

                if(product == null){
                    throw new BadRequestExceptions(Constants.ErrorProduct);
                }

                if(requestOrderItem.getQuantity() < 1){
                    throw new BadRequestExceptions(Constants.ErrorOrderItemZero);
                }
            });
            Ordering ordering = orderingRepository.save(Ordering.builder()
                    .cancellation(false)
                    .orderState(orderState)
                    .orderStateId(orderState.getId())
                    .client(user.getClient())
                    .clientId(user.getClientId())
                    .courier(courier)
                    .courierId(courier.getId())
                    .saleChannel(saleChannel)
                    .saleChannelId(saleChannel.getId())
                    .orderPaymentState(orderPaymentState)
                    .paymentStateId(orderPaymentState.getId())
                    .orderPaymentMethod(orderPaymentMethod)
                    .paymentMethodId(orderPaymentMethod.getId())
                    .managementType(managementType)
                    .managementTypeId(managementType.getId())
                    .registrationDate(new Date(System.currentTimeMillis()))
                    .updateDate(new Date(System.currentTimeMillis()))
                    .store(store)
                    .storeId(store.getId())
                    .closingChannel(closingChannel)
                    .closingChannelId(closingChannel.getId())
                    .tokenUser(user.getUsername())
                    .build());

            iOrderPaymentReceipt.uploadReceipt(requestOrderSave.getReceipts(),ordering.getId(),user.getUsername());

            double saleAmount = 0.00;

            for(RequestOrderItem requestOrderItem : requestOrderSave.getRequestOrderItems()){

                Product product = productRepository.findBySkuAndStatusTrue(requestOrderItem.getProductSku().toUpperCase());

                if(product == null){
                    throw new BadRequestExceptions(Constants.ErrorProduct);
                }

                ProductPrice productPrice = productPriceRepository.findByProductId(product.getId());

                saleAmount += (productPrice.getUnitSalePrice() * requestOrderItem.getQuantity()) - ((productPrice.getUnitSalePrice() * requestOrderItem.getQuantity()) * (requestOrderItem.getDiscount()/100));
                iOrderItem.save(ordering, requestOrderItem,tokenUser);
            }

            RequestSale requestSale = RequestSale.builder()
                    .saleChannel(requestOrderSave.getSaleChannel())
                    .seller(user.getName() + " " + user.getSurname())
                    .paymentMethod(requestOrderSave.getPaymentMethod())
                    .observations(requestOrderSave.getObservations())
                    .managementType(requestOrderSave.getManagementType())
                    .deliveryAmount(requestOrderSave.getDeliveryAmount())
                    .deliveryAddress(requestOrderSave.getDeliveryAddress())
                    .saleAmount(saleAmount)
                    .advancedPayment(requestOrderSave.getAdvancedPayment())
                    .build();

            iSale.save(ordering,requestSale,tokenUser);

            RequestCustomer requestCustomer = RequestCustomer.builder()
                    .phone(requestOrderSave.getCustomerPhone())
                    .name(requestOrderSave.getCustomerName())
                    .type(requestOrderSave.getCustomerType().toUpperCase())
                    .district(requestOrderSave.getCustomerDistrict())
                    .province(requestOrderSave.getCustomerProvince())
                    .department(requestOrderSave.getCustomerDepartment())
                    .instagram(requestOrderSave.getInstagram())
                    .reference(requestOrderSave.getCustomerReference())
                    .address(requestOrderSave.getCustomerAddress())
                    .build();

            iCustomer.save(ordering,requestCustomer,tokenUser);
            iAudit.save("ADD_ORDER","ADD ORDER "+ordering.getId()+".",user.getUsername());
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
    public CompletableFuture<ResponseSuccess> saveAsync(RequestOrderSave requestOrderSave, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            OrderState orderState;
            Courier courier;
            OrderPaymentState orderPaymentState;
            SaleChannel saleChannel;
            ManagementType managementType;
            OrderPaymentMethod orderPaymentMethod;
            Store store;
            ClosingChannel closingChannel;

            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                orderState = orderStateRepository.findByNameAndStatusTrue("PENDIENTE");
                courier = courierRepository.findByNameAndStatusTrue("SIN COURIER");
                orderPaymentState = orderPaymentStateRepository.findByNameAndStatusTrue("POR RECAUDAR");
                saleChannel = saleChannelRepository.findByNameAndStatusTrue(requestOrderSave.getSaleChannel().toUpperCase());
                managementType = managementTypeRepository.findByNameAndStatusTrue(requestOrderSave.getManagementType().toUpperCase());
                orderPaymentMethod = orderPaymentMethodRepository.findByNameAndStatusTrue(requestOrderSave.getPaymentMethod().toUpperCase());
                store = storeRepository.findByNameAndStatusTrue(requestOrderSave.getStoreName().toUpperCase());
                closingChannel = closingChannelRepository.findByNameAndStatusTrue(requestOrderSave.getClosingChannel().toUpperCase());
            }catch (RuntimeException e){
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(saleChannel == null){
                throw new BadRequestExceptions(Constants.ErrorSaleChannel);
            }

            if(orderPaymentMethod == null){
                throw new BadRequestExceptions(Constants.ErrorPaymentMethod);
            }

            if(managementType == null){
                throw new BadRequestExceptions(Constants.ErrorManagementType);
            }

            if(store == null){
                throw new BadRequestExceptions(Constants.ErrorStore);
            }

            if(closingChannel == null){
                throw new BadRequestExceptions(Constants.ErrorClosingChannel);
            }

            try{
                requestOrderSave.getRequestOrderItems().forEach(requestOrderItem -> {
                    Product product = productRepository.findBySkuAndStatusTrue(requestOrderItem.getProductSku().toUpperCase());

                    if(product == null){
                        throw new BadRequestExceptions(Constants.ErrorProduct);
                    }

                    if(requestOrderItem.getQuantity() < 1){
                        throw new BadRequestExceptions(Constants.ErrorOrderItemZero);
                    }
                });
                Ordering ordering = orderingRepository.save(Ordering.builder()
                        .cancellation(false)
                        .orderState(orderState)
                        .orderStateId(orderState.getId())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .courier(courier)
                        .courierId(courier.getId())
                        .saleChannel(saleChannel)
                        .saleChannelId(saleChannel.getId())
                        .orderPaymentState(orderPaymentState)
                        .paymentStateId(orderPaymentState.getId())
                        .orderPaymentMethod(orderPaymentMethod)
                        .paymentMethodId(orderPaymentMethod.getId())
                        .managementType(managementType)
                        .managementTypeId(managementType.getId())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .updateDate(new Date(System.currentTimeMillis()))
                        .store(store)
                        .storeId(store.getId())
                        .closingChannel(closingChannel)
                        .closingChannelId(closingChannel.getId())
                        .tokenUser(user.getUsername())
                        .build());

                iOrderPaymentReceipt.uploadReceipt(requestOrderSave.getReceipts(),ordering.getId(),user.getUsername());

                double saleAmount = 0.00;

                for(RequestOrderItem requestOrderItem : requestOrderSave.getRequestOrderItems()){

                    Product product = productRepository.findBySkuAndStatusTrue(requestOrderItem.getProductSku().toUpperCase());

                    if(product == null){
                        throw new BadRequestExceptions(Constants.ErrorProduct);
                    }

                    ProductPrice productPrice = productPriceRepository.findByProductId(product.getId());

                    saleAmount += (productPrice.getUnitSalePrice() * requestOrderItem.getQuantity()) - ((productPrice.getUnitSalePrice() * requestOrderItem.getQuantity()) * (requestOrderItem.getDiscount()/100));
                    iOrderItem.save(ordering, requestOrderItem,tokenUser);
                }

                RequestSale requestSale = RequestSale.builder()
                        .saleChannel(requestOrderSave.getSaleChannel())
                        .seller(user.getName() + " " + user.getSurname())
                        .paymentMethod(requestOrderSave.getPaymentMethod())
                        .observations(requestOrderSave.getObservations())
                        .managementType(requestOrderSave.getManagementType())
                        .deliveryAmount(requestOrderSave.getDeliveryAmount())
                        .deliveryAddress(requestOrderSave.getDeliveryAddress())
                        .saleAmount(saleAmount)
                        .advancedPayment(requestOrderSave.getAdvancedPayment())
                        .build();

                iSale.save(ordering,requestSale,tokenUser);

                RequestCustomer requestCustomer = RequestCustomer.builder()
                        .phone(requestOrderSave.getCustomerPhone())
                        .name(requestOrderSave.getCustomerName())
                        .type(requestOrderSave.getCustomerType().toUpperCase())
                        .district(requestOrderSave.getCustomerDistrict())
                        .province(requestOrderSave.getCustomerProvince())
                        .department(requestOrderSave.getCustomerDepartment())
                        .instagram(requestOrderSave.getInstagram())
                        .reference(requestOrderSave.getCustomerReference())
                        .address(requestOrderSave.getCustomerAddress())
                        .build();

                iCustomer.save(ordering,requestCustomer,tokenUser);
                iAudit.save("ADD_ORDER","ADD ORDER "+ordering.getId()+".",user.getUsername());
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
    public CompletableFuture<Page<OrderDTO>> list(Long orderId,String user,String orderState,String courier,String paymentState,String paymentMethod,String saleChannel,String managementType,String storeName, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<Ordering> pageOrdering;
            Long clientId;
            Long orderStateId;
            Long courierId;
            Long paymentStateId;
            Long paymentMethodId;
            Long saleChannelId;
            Long managementTypeId;
            Long storeId;

            if(orderState != null){
                orderStateId = orderStateRepository.findByName(orderState.toUpperCase()).getId();
            }else{
                orderStateId = null;
            }

            if(courier != null){
                courierId = courierRepository.findByName(courier.toUpperCase()).getId();
            }else {
                courierId = null;
            }

            if(paymentState != null){
                paymentStateId = orderPaymentStateRepository.findByName(paymentState.toUpperCase()).getId();
            }else {
                paymentStateId = null;
            }

            if(paymentMethod != null){
                paymentMethodId = orderPaymentMethodRepository.findByName(paymentMethod.toUpperCase()).getId();
            }else {
                paymentMethodId = null;
            }

            if(saleChannel != null){
                saleChannelId = saleChannelRepository.findByName(saleChannel.toUpperCase()).getId();
            }else {
                saleChannelId = null;
            }

            if(managementType != null){
                managementTypeId = managementTypeRepository.findByName(managementType.toUpperCase()).getId();
            }else{
                managementTypeId = null;
            }

            if(storeName != null){
                storeId = storeRepository.findByNameAndStatusTrue(storeName.toUpperCase()).getId();
            }else {
                storeId = null;
            }

            try{
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClient().getId();
                pageOrdering = orderingRepositoryCustom.searchForOrdering(orderId,clientId,orderStateId,courierId,paymentStateId,paymentMethodId,saleChannelId,managementTypeId,storeId,sort,sortColumn,pageNumber,pageSize);
            }catch (RuntimeException e){
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if(pageOrdering.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }

            List<OrderDTO> orderDTOS = pageOrdering.getContent().stream().map(order -> {
                Sale sale = saleRepository.findByOrderId(order.getId());
                Customer customer = customerRepository.findByOrderId(order.getId());
                List<String> paymentReceipts = orderPaymentReceiptRepository.findAllByOrderId(order.getId()).stream().map(OrderPaymentReceipt::getPaymentReceiptUrl).toList();
                List<String> courierPictures = courierPictureRepository.findAllByOrderId(order.getId()).stream().map(CourierPicture::getPictureUrl).toList();

                return OrderDTO.builder()
                        .id(order.getId())
                        .customerName(customer.getName())
                        .customerPhone(customer.getPhone())
                        .customerType(customer.getCustomerType().getName())
                        .orderStatus(order.getOrderState().getName())
                        .department(customer.getDepartment().getName())
                        .province(customer.getProvince().getName())
                        .district(customer.getDistrict().getName())
                        .address(customer.getAddress())
                        .instagram(customer.getInstagram())
                        .deliveryAmount(sale.getDeliveryAmount())
                        .advancedPayment(sale.getAdvancePayment())
                        .managementType(order.getManagementType().getName())
                        .reference(customer.getReference())
                        .duePayment((sale.getSaleAmount()+sale.getDeliveryAmount())-sale.getAdvancePayment())
                        .saleChannel(order.getSaleChannel().getName())
                        .sellerName(sale.getSeller())
                        .registrationDate(order.getRegistrationDate())
                        .updateDate(order.getUpdateDate())
                        .paymentMethod(order.getOrderPaymentMethod().getName())
                        .deliveryAddress(sale.getDeliveryAddress())
                        .courier(order.getCourier().getName())
                        .paymentReceipts(paymentReceipts)
                        .courierPictures(courierPictures)
                        .saleAmount(sale.getSaleAmount())
                        .build();
            }).toList();

            return new PageImpl<>(orderDTOS,pageOrdering.getPageable(),pageOrdering.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<OrderDTO>> listOrder(String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Long clientId;
            List<Ordering> orderingList;
            try{
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                orderingList = orderingRepository.findAllByClientId(clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(orderingList.isEmpty()){
                return Collections.emptyList();
            }
            return orderingList.stream().map(order -> {
                Sale sale = saleRepository.findByOrderId(order.getId());
                System.out.println(order.getId());
                Customer customer = customerRepository.findByOrderId(order.getId());
                List<String> paymentReceipts = orderPaymentReceiptRepository.findAllByOrderId(order.getId()).stream().map(OrderPaymentReceipt::getPaymentReceiptUrl).toList();
                List<String> courierPictures = courierPictureRepository.findAllByOrderId(order.getId()).stream().map(CourierPicture::getPictureUrl).toList();

                return OrderDTO.builder()
                        .id(order.getId())
                        .serial(order.getId())
                        .customerName(customer.getName())
                        .customerPhone(customer.getPhone())
                        .customerType(customer.getCustomerType().getName())
                        .orderStatus(order.getOrderState().getName())
                        .department(customer.getDepartment().getName())
                        .province(customer.getProvince().getName())
                        .district(customer.getDistrict().getName())
                        .address(customer.getAddress())
                        .instagram(customer.getInstagram())
                        .deliveryAmount(sale.getDeliveryAmount())
                        .advancedPayment(sale.getAdvancePayment())
                        .managementType(order.getManagementType().getName())
                        .reference(customer.getReference())
                        .duePayment((sale.getSaleAmount()+sale.getDeliveryAmount())-sale.getAdvancePayment())
                        .saleChannel(order.getSaleChannel().getName())
                        .sellerName(sale.getSeller())
                        .registrationDate(order.getRegistrationDate())
                        .updateDate(order.getUpdateDate())
                        .paymentMethod(order.getOrderPaymentMethod().getName())
                        .paymentState(order.getOrderPaymentState().getName())
                        .deliveryAddress(sale.getDeliveryAddress())
                        .courier(order.getCourier().getName())
                        .paymentReceipts(paymentReceipts)
                        .courierPictures(courierPictures)
                        .saleAmount(sale.getSaleAmount())
                        .observations(sale.getObservations())
                        .closingChannel(order.getClosingChannel().getName())
                        .build();
            }).toList();
        });
    }

    @Override
    public ResponseSuccess update(Long orderId, RequestOrderUpdate requestOrderUpdate, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        Ordering ordering;
        OrderState orderState;
        Sale sale;
        OrderPaymentMethod orderPaymentMethod;
        OrderPaymentState orderPaymentState;
        Courier courier;
        OrderStock orderStock;

        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            ordering = orderingRepository.findById(orderId).orElse(null);
            orderState = orderStateRepository.findByNameAndStatusTrue(requestOrderUpdate.getOrderState().toUpperCase());
            orderPaymentMethod = orderPaymentMethodRepository.findByNameAndStatusTrue(requestOrderUpdate.getPaymentMethod().toUpperCase());
            orderPaymentState = orderPaymentStateRepository.findByNameAndStatusTrue(requestOrderUpdate.getPaymentState().toUpperCase());
            courier = courierRepository.findByNameAndStatusTrue(requestOrderUpdate.getCourier().toUpperCase());
        }catch (RuntimeException e){
            e.printStackTrace();
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(courier == null){
            throw new BadRequestExceptions(Constants.ErrorCourier);
        }

        if(orderPaymentState == null){
            throw new BadRequestExceptions(Constants.ErrorPaymentState);
        }

        if(ordering == null){
            throw new BadRequestExceptions(Constants.ErrorOrdering);
        }else {
            sale = saleRepository.findByOrderId(ordering.getId());
            orderStock = orderStockRepository.findByOrderId(ordering.getId());
        }

        try{

            if(!Objects.equals(orderState.getName(), ordering.getOrderState().getName())){
                ordering.setOrderState(orderState);
                ordering.setOrderStateId(orderState.getId());
            }

            if(!Objects.equals(requestOrderUpdate.getObservations(),sale.getObservations()))
            {
                sale.setObservations(requestOrderUpdate.getObservations());
            }

            if(Objects.equals(ordering.getOrderState().getName(), "ENTREGADO")){
                List<OrderItem> orderOrderItems = orderItemRepository.findAllByOrderId(ordering.getId());
                List<RequestStockTransactionItem> stockTransactionList = new ArrayList<>();
                for(OrderItem orderItem : orderOrderItems){
                    List<OrderStockItem> orderStockItemList = orderStockItemRepository.findByOrderStockIdAndOrderItemId(orderStock.getId(), orderItem.getId());
                    for(OrderStockItem orderStockItem : orderStockItemList){
                        stockTransactionList.add(RequestStockTransactionItem.builder()
                                .supplierProductSerial(orderStockItem.getSupplierProduct().getSerial())
                                .quantity(orderStockItem.getQuantity())
                                .build());
                        iWarehouseStock.out(orderStockItem.getOrderStock().getWarehouse(), orderStockItem.getSupplierProduct(), orderStockItem.getQuantity(),user);
                        iGeneralStock.out(orderStockItem.getSupplierProduct().getSerial(), orderStockItem.getQuantity(),user.getUsername());
                    }
                }
                iStockTransaction.save("O"+ordering.getId(),orderStock.getWarehouse(),stockTransactionList,"SALIDA",user);
            }

            if(!Objects.equals(orderPaymentMethod.getId(), ordering.getOrderPaymentMethod().getId())){
                ordering.setOrderPaymentMethod(orderPaymentMethod);
                ordering.setPaymentMethodId(orderPaymentMethod.getId());
            }

            if(!Objects.equals(orderPaymentState.getId(),ordering.getOrderPaymentState().getId())){
                ordering.setOrderPaymentState(orderPaymentState);
                ordering.setPaymentStateId(orderPaymentState.getId());
            }

            if(!Objects.equals(courier.getId(),ordering.getCourier().getId())){
                ordering.setCourier(courier);
                ordering.setCourierId(courier.getId());
            }

            orderingRepository.save(ordering);
            saleRepository.save(sale);
            iOrderPaymentReceipt.uploadReceipt(requestOrderUpdate.getReceipts(),ordering.getId(),user.getUsername());
            iCourierPicture.uploadPicture(requestOrderUpdate.getPictures(),ordering.getId(),user.getUsername());
            iAudit.save("UPDATE_ORDER","UPDATE ORDER "+ordering.getId()+".",user.getUsername());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.update)
                    .build();
        }catch (RuntimeException e){
            e.printStackTrace();
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public CompletableFuture<ResponseSuccess> updateAsync(Long orderId, RequestOrderUpdate requestOrderUpdate, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Ordering ordering;
            OrderState orderState;
            Sale sale;
            OrderPaymentMethod orderPaymentMethod;
            OrderPaymentState orderPaymentState;
            Courier courier;
            OrderStock orderStock;

            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                ordering = orderingRepository.findById(orderId).orElse(null);
                orderState = orderStateRepository.findByNameAndStatusTrue(requestOrderUpdate.getOrderState().toUpperCase());
                orderPaymentMethod = orderPaymentMethodRepository.findByNameAndStatusTrue(requestOrderUpdate.getPaymentMethod().toUpperCase());
                orderPaymentState = orderPaymentStateRepository.findByNameAndStatusTrue(requestOrderUpdate.getPaymentState().toUpperCase());
                courier = courierRepository.findByNameAndStatusTrue(requestOrderUpdate.getCourier().toUpperCase());
            }catch (RuntimeException e){
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(courier == null){
                throw new BadRequestExceptions(Constants.ErrorCourier);
            }

            if(orderPaymentState == null){
                throw new BadRequestExceptions(Constants.ErrorPaymentState);
            }

            if(ordering == null){
                throw new BadRequestExceptions(Constants.ErrorOrdering);
            }else {
                sale = saleRepository.findByOrderId(ordering.getId());
                orderStock = orderStockRepository.findByOrderId(ordering.getId());
            }

            try{

                if(!Objects.equals(orderState.getName(), ordering.getOrderState().getName())){
                    ordering.setOrderState(orderState);
                    ordering.setOrderStateId(orderState.getId());
                }

                if(!Objects.equals(requestOrderUpdate.getObservations(),sale.getObservations()))
                {
                    sale.setObservations(requestOrderUpdate.getObservations());
                }

                if(Objects.equals(ordering.getOrderState().getName(), "ENTREGADO")){
                    List<OrderItem> orderOrderItems = orderItemRepository.findAllByOrderId(ordering.getId());
                    List<RequestStockTransactionItem> stockTransactionList = new ArrayList<>();
                    for(OrderItem orderItem : orderOrderItems){
                        List<OrderStockItem> orderStockItemList = orderStockItemRepository.findByOrderStockIdAndOrderItemId(orderStock.getId(), orderItem.getId());
                        for(OrderStockItem orderStockItem : orderStockItemList){
                            stockTransactionList.add(RequestStockTransactionItem.builder()
                                    .supplierProductSerial(orderStockItem.getSupplierProduct().getSerial())
                                    .quantity(orderStockItem.getQuantity())
                                    .build());
                            iWarehouseStock.out(orderStockItem.getOrderStock().getWarehouse(), orderStockItem.getSupplierProduct(), orderStockItem.getQuantity(),user);
                            iGeneralStock.out(orderStockItem.getSupplierProduct().getSerial(), orderStockItem.getQuantity(),user.getUsername());
                        }
                    }
                    iStockTransaction.save("O"+ordering.getId(),orderStock.getWarehouse(),stockTransactionList,"SALIDA",user);
                }

                if(!Objects.equals(orderPaymentMethod.getId(), ordering.getOrderPaymentMethod().getId())){
                    ordering.setOrderPaymentMethod(orderPaymentMethod);
                    ordering.setPaymentMethodId(orderPaymentMethod.getId());
                }

                if(!Objects.equals(orderPaymentState.getId(),ordering.getOrderPaymentState().getId())){
                    ordering.setOrderPaymentState(orderPaymentState);
                    ordering.setPaymentStateId(orderPaymentState.getId());
                }

                if(!Objects.equals(courier.getId(),ordering.getCourier().getId())){
                    ordering.setCourier(courier);
                    ordering.setCourierId(courier.getId());
                }

                orderingRepository.save(ordering);
                saleRepository.save(sale);
                iOrderPaymentReceipt.uploadReceipt(requestOrderUpdate.getReceipts(),ordering.getId(),user.getUsername());
                iCourierPicture.uploadPicture(requestOrderUpdate.getPictures(),ordering.getId(),user.getUsername());
                iAudit.save("UPDATE_ORDER","UPDATE ORDER "+ordering.getId()+".",user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.update)
                        .build();
            }catch (RuntimeException e){
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }
}
