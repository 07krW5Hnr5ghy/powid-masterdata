package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.OrderDTO;
import com.proyect.masterdata.dto.OrderItemDTO;
import com.proyect.masterdata.dto.request.RequestOrderItem;
import com.proyect.masterdata.dto.request.RequestOrderSave;
import com.proyect.masterdata.dto.request.RequestOrderUpdate;
import com.proyect.masterdata.dto.request.RequestStockTransactionItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.*;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.io.FileUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.*;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.stream.Stream;

@RequiredArgsConstructor
@Service
@Log4j2
public class OrderingImpl implements IOrdering {

    private final UserRepository userRepository;
    private final OrderingRepository orderingRepository;
    private final OrderStateRepository orderStateRepository;
    private final IOrderItem iOrderItem;
    private final OrderingRepositoryCustom orderingRepositoryCustom;
    private final OrderItemRepository orderItemRepository;
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
    private final StoreRepository storeRepository;
    private final ClosingChannelRepository closingChannelRepository;
    private final CustomerTypeRepository customerTypeRepository;
    private final DepartmentRepository departmentRepository;
    private final ProvinceRepository provinceRepository;
    private final DistrictRepository districtRepository;
    private final IAudit iAudit;
    private final CustomerRepository customerRepository;
    private final DiscountRepository discountRepository;
    private final DeliveryPointRepository deliveryPointRepository;
    private final ProductPriceRepository productPriceRepository;
    @Override
    public ResponseSuccess save(
            RequestOrderSave requestOrderSave,
            MultipartFile[] receipts,
            String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        OrderState orderState;
        Courier courier;
        OrderPaymentState orderPaymentState;
        SaleChannel saleChannel;
        ManagementType managementType;
        OrderPaymentMethod orderPaymentMethod;
        Store store;
        ClosingChannel closingChannel;
        Customer customer;
        DeliveryPoint deliveryPoint;
        Discount discount;
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
            customer = customerRepository.findByPhone(requestOrderSave.getPhone().toUpperCase());
            deliveryPoint = deliveryPointRepository.findByNameAndStatusTrue(requestOrderSave.getDeliveryPoint().toUpperCase());
            discount = discountRepository.findByNameAndStatusTrue(requestOrderSave.getDiscount().toUpperCase());
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

        if(customer == null){
            throw new BadRequestExceptions(Constants.ErrorCustomer);
        }

        if(deliveryPoint == null){
            throw new BadRequestExceptions(Constants.ErrorDeliveryPoint);
        }

        if(discount == null){
            throw new BadRequestExceptions(Constants.ErrorDiscount);
        }

        try{
            requestOrderSave.getRequestOrderItems().forEach(requestOrderItem -> {
                Product product = productRepository.findBySkuAndStatusTrue(requestOrderItem.getProduct().toUpperCase());

                if(product == null){
                    throw new BadRequestExceptions(Constants.ErrorProduct);
                }

                if(requestOrderItem.getQuantity() < 1){
                    throw new BadRequestExceptions(Constants.ErrorOrderItemZero);
                }
            });

            Ordering ordering = orderingRepository.save(Ordering.builder()
                    .cancellation(false)
                    .seller(user.getName() + " " + user.getSurname())
                    .observations(requestOrderSave.getObservations().toUpperCase())
                    .deliveryAddress(requestOrderSave.getDeliveryAddress())
                    .deliveryAmount(requestOrderSave.getDeliveryAmount())
                    .advancedPayment(requestOrderSave.getAdvancedPayment())
                    .discount(discount)
                    .discountId(discount.getId())
                    .discountAmount(requestOrderSave.getDiscountAmount())
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
                    .customer(customer)
                    .customerId(customer.getId())
                    .tokenUser(user.getUsername())
                    .deliveryPoint(deliveryPoint)
                    .deliveryPointId(deliveryPoint.getId())
                    .build());
            iOrderPaymentReceipt.uploadReceipt(receipts,ordering.getId(),user.getUsername());

            for(RequestOrderItem requestOrderItem : requestOrderSave.getRequestOrderItems()){
                iOrderItem.save(ordering, requestOrderItem,tokenUser);
            }

            if(Objects.equals(deliveryPoint.getName(), "PROVINCIA") && !Objects.equals(requestOrderSave.getDni(), "NO APLICA")){
                customer.setDni(requestOrderSave.getDni());
                customerRepository.save(customer);
            }

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
    public CompletableFuture<ResponseSuccess> saveAsync(RequestOrderSave requestOrderSave,MultipartFile[] receipts, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        Path folder = Paths.get("src/main/resources/uploads/orders");
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
            Customer customer;
            DeliveryPoint deliveryPoint;
            Discount discount;
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
                customer = customerRepository.findByPhone(requestOrderSave.getPhone().toUpperCase());
                deliveryPoint = deliveryPointRepository.findByNameAndStatusTrue(requestOrderSave.getDeliveryPoint().toUpperCase());
                discount = discountRepository.findByNameAndStatusTrue(requestOrderSave.getDiscount().toUpperCase());
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

            if(customer == null){
                throw new BadRequestExceptions(Constants.ErrorCustomerExist);
            }

            if(discount == null){
                throw new BadRequestExceptions(Constants.ErrorDiscount);
            }

            try{
                requestOrderSave.getRequestOrderItems().forEach(requestOrderItem -> {
                    Product product = productRepository.findBySkuAndStatusTrue(requestOrderItem.getProduct().toUpperCase());

                    if(product == null){
                        throw new BadRequestExceptions(Constants.ErrorProduct);
                    }

                    if(requestOrderItem.getQuantity() < 1){
                        throw new BadRequestExceptions(Constants.ErrorOrderItemZero);
                    }
                });
                Ordering ordering = orderingRepository.save(Ordering.builder()
                        .cancellation(false)
                        .seller(user.getName() + " " + user.getSurname())
                        .observations(requestOrderSave.getObservations().toUpperCase())
                        .deliveryAddress(requestOrderSave.getDeliveryAddress())
                        .deliveryAmount(requestOrderSave.getDeliveryAmount())
                        .advancedPayment(requestOrderSave.getAdvancedPayment())
                        .discount(discount)
                        .discountId(discount.getId())
                        .discountAmount(requestOrderSave.getDiscountAmount())
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
                        .customer(customer)
                        .customerId(customer.getId())
                        .deliveryPoint(deliveryPoint)
                        .deliveryPointId(deliveryPoint.getId())
                        .build());
                List<File> fileList = new ArrayList<>();
                for(MultipartFile multipartFile : receipts){
                    if(multipartFile.isEmpty()){
                        break;
                    }
                    File convFile = new File("src/main/resources/uploads/orders/"+multipartFile.getOriginalFilename());
                    convFile.createNewFile();
                    FileOutputStream fos = new FileOutputStream(convFile);
                    fos.write(multipartFile.getBytes());
                    fos.close();
                    fileList.add(convFile);
                }

                CompletableFuture<List<String>> paymentReceipts = iOrderPaymentReceipt.uploadReceiptFileAsync(fileList,ordering.getId(),user.getUsername());

                for(RequestOrderItem requestOrderItem : requestOrderSave.getRequestOrderItems()){
                    iOrderItem.save(ordering, requestOrderItem,tokenUser);
                }

                if(Objects.equals(deliveryPoint.getName(), "PROVINCIA") && !Objects.equals(requestOrderSave.getDni(), "NO APLICA")){
                    customer.setDni(requestOrderSave.getDni());
                    customerRepository.save(customer);
                }

                if(!paymentReceipts.get().isEmpty()){
                    Stream<Path> paths = Files.list(folder);
                    paths.filter(Files::isRegularFile).forEach(path -> {
                        try {
                            Files.delete(path);
                        } catch (IOException e) {
                            throw new RuntimeException(e);
                        }
                    });
                }

                iAudit.save("ADD_ORDER","ADD ORDER "+ordering.getId()+".",user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();

            }catch (RuntimeException | IOException | ExecutionException | InterruptedException e){
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
                List<String> paymentReceipts = orderPaymentReceiptRepository.findAllByOrderId(order.getId()).stream().map(OrderPaymentReceipt::getPaymentReceiptUrl).toList();
                List<String> courierPictures = courierPictureRepository.findAllByOrderId(order.getId()).stream().map(CourierPicture::getPictureUrl).toList();
                List<OrderItem> orderItems = orderItemRepository.findAllByOrderId(order.getId());

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
                return OrderDTO.builder()
                        .id(order.getId())
                        .customerName(order.getCustomer().getName())
                        .customerPhone(order.getCustomer().getPhone())
                        .customerType(order.getCustomer().getCustomerType().getName())
                        .closingChannel(order.getClosingChannel().getName())
                        .orderStatus(order.getOrderState().getName())
                        .department(order.getCustomer().getDistrict().getProvince().getDepartment().getName())
                        .province(order.getCustomer().getDistrict().getProvince().getName())
                        .district(order.getCustomer().getDistrict().getName())
                        .address(order.getCustomer().getAddress())
                        .instagram(order.getCustomer().getInstagram())
                        .managementType(order.getManagementType().getName())
                        .reference(order.getCustomer().getReference())
                        .saleChannel(order.getSaleChannel().getName())
                        .sellerName(order.getSeller())
                        .registrationDate(order.getRegistrationDate())
                        .updateDate(order.getUpdateDate())
                        .paymentMethod(order.getOrderPaymentMethod().getName())
                        .paymentState(order.getOrderPaymentState().getName())
                        .deliveryAddress(order.getDeliveryAddress())
                        .courier(order.getCourier().getName())
                        .paymentReceipts(paymentReceipts)
                        .courierPictures(courierPictures)
                        .observations(order.getObservations())
                        .saleAmount(BigDecimal.valueOf(saleAmount).setScale(2, RoundingMode.HALF_EVEN))
                        .advancedPayment(BigDecimal.valueOf(order.getAdvancedPayment()).setScale(2, RoundingMode.HALF_EVEN))
                        .duePayment(BigDecimal.valueOf(totalDuePayment).setScale(2,RoundingMode.HALF_EVEN))
                        .deliveryAmount(BigDecimal.valueOf(order.getDeliveryAmount()).setScale(2,RoundingMode.HALF_EVEN))
                        .deliveryPoint(order.getDeliveryPoint().getName())
                        .discount(order.getDiscount().getName())
                        .discountAmount(BigDecimal.valueOf(order.getDiscountAmount()))
                        .dni(order.getCustomer().getDni())
                        .store(order.getStore().getName())
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
                List<String> paymentReceipts = orderPaymentReceiptRepository.findAllByOrderId(order.getId()).stream().map(OrderPaymentReceipt::getPaymentReceiptUrl).toList();
                List<String> courierPictures = courierPictureRepository.findAllByOrderId(order.getId()).stream().map(CourierPicture::getPictureUrl).toList();
                List<OrderItem> orderItems = orderItemRepository.findAllByOrderId(order.getId());
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
                return OrderDTO.builder()
                        .id(order.getId())
                        .customerName(order.getCustomer().getName())
                        .customerPhone(order.getCustomer().getPhone())
                        .customerType(order.getCustomer().getCustomerType().getName())
                        .orderStatus(order.getOrderState().getName())
                        .department(order.getCustomer().getDistrict().getProvince().getDepartment().getName())
                        .province(order.getCustomer().getDistrict().getProvince().getName())
                        .district(order.getCustomer().getDistrict().getName())
                        .address(order.getCustomer().getAddress())
                        .instagram(order.getCustomer().getInstagram())
                        .managementType(order.getManagementType().getName())
                        .reference(order.getCustomer().getReference())
                        .saleChannel(order.getSaleChannel().getName())
                        .sellerName(order.getSeller())
                        .registrationDate(order.getRegistrationDate())
                        .updateDate(order.getUpdateDate())
                        .paymentMethod(order.getOrderPaymentMethod().getName())
                        .paymentState(order.getOrderPaymentState().getName())
                        .deliveryAddress(order.getDeliveryAddress())
                        .courier(order.getCourier().getName())
                        .paymentReceipts(paymentReceipts)
                        .courierPictures(courierPictures)
                        .observations(order.getObservations())
                        .closingChannel(order.getClosingChannel().getName())
                        .saleAmount(BigDecimal.valueOf(saleAmount).setScale(2, RoundingMode.HALF_EVEN))
                        .advancedPayment(BigDecimal.valueOf(order.getAdvancedPayment()).setScale(2, RoundingMode.HALF_EVEN))
                        .duePayment(BigDecimal.valueOf(totalDuePayment).setScale(2,RoundingMode.HALF_EVEN))
                        .deliveryAmount(BigDecimal.valueOf(order.getDeliveryAmount()).setScale(2,RoundingMode.HALF_EVEN))
                        .deliveryPoint(order.getDeliveryPoint().getName())
                        .dni(order.getCustomer().getDni())
                        .store(order.getStore().getName())
                        .build();
            }).toList();
        });
    }

    @Override
    public ResponseSuccess update(Long orderId, RequestOrderUpdate requestOrderUpdate,MultipartFile[] receipts,MultipartFile[] courierPictures, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        Ordering ordering;
        OrderState orderState;
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
            orderStock = orderStockRepository.findByOrderId(ordering.getId());
        }

        try{

            if(!Objects.equals(orderState.getName(), ordering.getOrderState().getName())){
                ordering.setOrderState(orderState);
                ordering.setOrderStateId(orderState.getId());
            }

            if(!Objects.equals(requestOrderUpdate.getObservations().toUpperCase(),ordering.getObservations().toUpperCase()))
            {
                ordering.setObservations(requestOrderUpdate.getObservations().toUpperCase());
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
                iStockTransaction.save("O"+ordering.getId(),orderStock.getWarehouse(),stockTransactionList,"PEDIDO",user);
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
            iOrderPaymentReceipt.uploadReceipt(receipts,ordering.getId(),user.getUsername());
            iCourierPicture.uploadPicture(courierPictures,ordering.getId(),user.getUsername());
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
    public CompletableFuture<ResponseSuccess> updateAsync(Long orderId, RequestOrderUpdate requestOrderUpdate,MultipartFile[] receipts,MultipartFile[] courierPictures, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        Path folderOrders = Paths.get("src/main/resources/uploads/orders");
        Path folderCouriers = Paths.get("src/main/resources/uploads/couriers");
        return CompletableFuture.supplyAsync(()->{
            User user;
            Ordering ordering;
            OrderState orderState;
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
                orderStock = orderStockRepository.findByOrderId(ordering.getId());
            }

            try{

                if(!Objects.equals(orderState.getName(), ordering.getOrderState().getName())){
                    ordering.setOrderState(orderState);
                    ordering.setOrderStateId(orderState.getId());
                }

                if(!Objects.equals(requestOrderUpdate.getObservations().toUpperCase(),ordering.getObservations().toUpperCase()))
                {
                    ordering.setObservations(requestOrderUpdate.getObservations().toUpperCase());
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
                    iStockTransaction.save("O"+ordering.getId(),orderStock.getWarehouse(),stockTransactionList,"PEDIDO",user);
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
                List<File> receiptList = new ArrayList<>();
                for(MultipartFile multipartFile : receipts){
                    if(multipartFile.isEmpty()){
                        break;
                    }
                    File convFile = new File("src/main/resources/uploads/"+multipartFile.getOriginalFilename());
                    convFile.createNewFile();
                    FileOutputStream fos = new FileOutputStream(convFile);
                    fos.write(multipartFile.getBytes());
                    fos.close();
                    receiptList.add(convFile);
                }
                List<File> courierPictureList = new ArrayList<>();
                for(MultipartFile multipartFile:courierPictures){
                    if(multipartFile.isEmpty()){
                        break;
                    }
                    File convFile = new File("src/main/resources/uploads/"+multipartFile.getOriginalFilename());
                    convFile.createNewFile();
                    FileOutputStream fos = new FileOutputStream(convFile);
                    fos.write(multipartFile.getBytes());
                    fos.close();
                    courierPictureList.add(convFile);
                }

                CompletableFuture<List<String>> paymentReceipts = iOrderPaymentReceipt.uploadReceiptFileAsync(receiptList,ordering.getId(),user.getUsername());
                CompletableFuture<List<String>> courierPhotos = iCourierPicture.uploadPictureAsync(courierPictureList,ordering.getId(),user.getUsername());
                if(!paymentReceipts.get().isEmpty() || !courierPhotos.get().isEmpty()){
                    Stream<Path> paths = Files.list(folderOrders);
                    paths.filter(Files::isRegularFile).forEach(path -> {
                        try {
                            Files.delete(path);
                        } catch (IOException e) {
                            throw new RuntimeException(e);
                        }
                    });
                }
                if(!courierPhotos.get().isEmpty()){
                    Stream<Path> paths = Files.list(folderCouriers);
                    paths.filter(Files::isRegularFile).forEach(path -> {
                        try {
                            Files.delete(path);
                        } catch (IOException e) {
                            throw new RuntimeException(e);
                        }
                    });
                }

                iAudit.save("UPDATE_ORDER","UPDATE ORDER "+ordering.getId()+".",user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.update)
                        .build();
            }catch (RuntimeException | IOException | InterruptedException | ExecutionException e){
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<OrderDTO> selectOrder(Long orderId,String username) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Ordering ordering;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                ordering = orderingRepository.findByClientIdAndId(user.getClientId(),orderId);
            }
            try {
                List<OrderItem> orderItems = orderItemRepository.findAllByOrderIdAndStatusTrue(ordering.getId());
                double saleAmount = 0.00;
                for(OrderItem orderItem : orderItems){
                    ProductPrice productPrice = productPriceRepository.findByProductId(orderItem.getProductId());
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
                if(Objects.equals(ordering.getDiscount().getName(), "PORCENTAJE")){
                    totalDuePayment = (saleAmount-((saleAmount)*(ordering.getDiscountAmount()/100))+ordering.getDeliveryAmount())-ordering.getAdvancedPayment();
                }
                if(Objects.equals(ordering.getDiscount().getName(), "MONTO")){
                    totalDuePayment = (saleAmount-ordering.getDiscountAmount()+ordering.getDeliveryAmount())-ordering.getAdvancedPayment();
                }
                if(Objects.equals(ordering.getDiscount().getName(), "NO APLICA")){
                    totalDuePayment = (saleAmount+ordering.getDeliveryAmount())-ordering.getAdvancedPayment();
                }
                List<CourierPicture> courierPictures = courierPictureRepository.findAllByOrderId(ordering.getId());
                List<OrderPaymentReceipt> orderPaymentReceipts = orderPaymentReceiptRepository.findAllByOrderId(ordering.getId());
                return OrderDTO.builder()
                        .sellerName(ordering.getSeller())
                        .discount(ordering.getDiscount().getName())
                        .deliveryPoint(ordering.getDeliveryPoint().getName())
                        .observations(ordering.getObservations())
                        .closingChannel(ordering.getClosingChannel().getName())
                        .paymentState(ordering.getOrderPaymentState().getName())
                        .orderStatus(ordering.getOrderState().getName())
                        .courierPictures(courierPictures.stream().map(courierPicture -> courierPicture.getPictureUrl().toUpperCase()).toList())
                        .paymentMethod(ordering.getOrderPaymentMethod().getName())
                        .paymentReceipts(orderPaymentReceipts.stream().map(OrderPaymentReceipt::getPaymentReceiptUrl).toList())
                        .courier(ordering.getCourier().getName())
                        .address(ordering.getDeliveryAddress().toUpperCase())
                        .saleChannel(ordering.getSaleChannel().getName())
                        .managementType(ordering.getManagementType().getName())
                        .instagram(ordering.getCustomer().getInstagram())
                        .district(ordering.getCustomer().getDistrict().getName())
                        .province(ordering.getCustomer().getDistrict().getProvince().getName())
                        .department(ordering.getCustomer().getDistrict().getProvince().getDepartment().getName())
                        .customerType(ordering.getCustomer().getCustomerType().getName())
                        .reference(ordering.getCustomer().getReference())
                        .customerName(ordering.getCustomer().getName())
                        .customerPhone(ordering.getCustomer().getPhone())
                        .deliveryAddress(ordering.getDeliveryAddress().toUpperCase())
                        .advancedPayment(BigDecimal.valueOf(ordering.getAdvancedPayment()))
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .updateDate(new Date(System.currentTimeMillis()))
                        .deliveryAmount(BigDecimal.valueOf(ordering.getDeliveryAmount()))
                        .discountAmount(BigDecimal.valueOf(ordering.getDiscountAmount()))
                        .duePayment(BigDecimal.valueOf(totalDuePayment).setScale(2,RoundingMode.HALF_EVEN))
                        .saleAmount(BigDecimal.valueOf(saleAmount).setScale(2,RoundingMode.HALF_EVEN))
                        .paymentState(ordering.getOrderPaymentState().getName())
                        .closingChannel(ordering.getClosingChannel().getName())
                        .dni(ordering.getCustomer().getDni())
                        .store(ordering.getStore().getName())
                        .orderItemDTOS(orderItems.stream().map(orderItem -> {
                            ProductPrice productPrice = productPriceRepository.findByProductId(orderItem.getProductId());
                            Double totalPrice = null;
                            if(Objects.equals(orderItem.getDiscount().getName(), "PORCENTAJE")){
                                totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity())-((productPrice.getUnitSalePrice() * orderItem.getQuantity())*(orderItem.getDiscountAmount()/100));
                            }

                            if(Objects.equals(orderItem.getDiscount().getName(), "MONTO")){
                                totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity())-(orderItem.getDiscountAmount());
                            }

                            if(Objects.equals(orderItem.getDiscount().getName(), "NO APLICA")){
                                totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity());
                            }
                            return OrderItemDTO.builder()
                                    .orderId(orderItem.getOrderId())
                                    .discountAmount(orderItem.getDiscountAmount())
                                    .sku(orderItem.getProduct().getSku())
                                    .unit(orderItem.getProduct().getUnit().getName())
                                    .observations(orderItem.getObservations())
                                    .quantity(orderItem.getQuantity())
                                    .size(orderItem.getProduct().getSize().getName())
                                    .discount(orderItem.getDiscount().getName())
                                    .pictures(new ArrayList<>())
                                    .unitPrice(productPrice.getUnitSalePrice())
                                    .totalPrice(totalPrice)
                                    .color(orderItem.getProduct().getColor().getName())
                                    .category(orderItem.getProduct().getCategoryProduct().getName())
                                    .registrationDate(orderItem.getRegistrationDate())
                                    .updateDate(orderItem.getUpdateDate())
                                    .build();
                        }).toList())
                        .id(ordering.getId())
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }
}
