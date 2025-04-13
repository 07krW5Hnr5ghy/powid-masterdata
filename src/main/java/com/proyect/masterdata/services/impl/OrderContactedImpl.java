package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.OrderContactedDTO;
import com.proyect.masterdata.dto.OrderDTO;
import com.proyect.masterdata.dto.OrderItemDTO;
import com.proyect.masterdata.dto.UserDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IOrderContacted;
import com.proyect.masterdata.services.IOrderLog;
import com.proyect.masterdata.services.IUtil;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.OffsetDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Log4j2
public class OrderContactedImpl implements IOrderContacted {
    private final UserRepository userRepository;
    private final OrderingRepository orderingRepository;
    private final OrderContactedRepository orderContactedRepository;
    private final IAudit iAudit;
    private final OrderContactedRepositoryCustom orderContactedRepositoryCustom;
    private final OrderPaymentReceiptRepository orderPaymentReceiptRepository;
    private final CourierPictureRepository courierPictureRepository;
    private final OrderItemRepository orderItemRepository;
    private final ProductPriceRepository productPriceRepository;
    private final ProductPictureRepository productPictureRepository;
    private final IUtil iUtil;
    private final IOrderLog iOrderLog;
    private final OrderStateRepository orderStateRepository;
    private final CourierRepository courierRepository;
    private final DeliveryZoneDistrictRepository deliveryZoneDistrictRepository;
    private final DistrictRepository districtRepository;
    private final DeliveryZoneRepository deliveryZoneRepository;
    @Override
    public CompletableFuture<ResponseSuccess> save(UUID orderId, String username,String observations) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Ordering ordering;
            OrderContacted orderContacted;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                ordering = orderingRepository.findById(orderId).orElse(null);
                orderContacted = orderContactedRepository.findByOrderId(orderId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(ordering==null){
                throw new BadRequestExceptions(Constants.ErrorOrdering);
            }
            if(orderContacted!=null){
                throw new BadRequestExceptions(Constants.ErrorOrderContactedExists);
            }
            try{
                District district = districtRepository.findByNameAndProvinceId(ordering.getCustomer().getDistrict().getName(),ordering.getCustomer().getDistrict().getProvinceId());
                DeliveryZoneDistrict deliveryZoneDistrict = deliveryZoneDistrictRepository.findByDistrictId(district.getId());
                OrderContacted newOrderContacted = OrderContacted.builder()
                                .orderId(ordering.getId())
                                .ordering(ordering)
                                .contacted(false)
                                .agentUser(user)
                                .agentUserId(user.getId())
                                .registrationDate(OffsetDateTime.now())
                                .updateDate(OffsetDateTime.now())
                                .user(user)
                                .userId(user.getId())
                                .client(user.getClient())
                                .clientId(user.getClientId())
                        .build();
                if(deliveryZoneDistrict==null){
                    DeliveryZone deliveryZone = deliveryZoneRepository.findByNameAndClientId("PROVINCIA",user.getClientId());
                    newOrderContacted.setDeliveryZone(deliveryZone);
                    newOrderContacted.setDeliveryZoneId(deliveryZone.getId());
                }else{
                    newOrderContacted.setDeliveryZone(deliveryZoneDistrict.getDeliveryZone());
                    newOrderContacted.setDeliveryZoneId(deliveryZoneDistrict.getDeliveryZoneId());
                }
                orderContactedRepository.save(newOrderContacted);
                newOrderContacted.setObservations(Objects.requireNonNullElse(observations, "sin observaciones"));
                iOrderLog.save(
                        user,
                        newOrderContacted.getOrdering(),
                        OffsetDateTime.now()+
                                " - "+
                                user.getUsername()+
                                " "+"Estado contactado : No Contactado"
                );
                iAudit.save(
                        "ADD_ORDER_CONTACTED",
                        "PEDIDO "+
                                newOrderContacted.getOrdering().getOrderNumber()+
                                " AGREGADO A CONTACT CENTER.",
                        newOrderContacted.getOrdering().getOrderNumber().toString(),
                        user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Transactional
    @Override
    public CompletableFuture<ResponseSuccess> markContacted(UUID orderId, String username,String observations) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            OrderContacted orderContacted;
            Ordering ordering;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                orderContacted = orderContactedRepository.findByOrderId(orderId);

            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(orderContacted==null){
                throw new BadRequestExceptions(Constants.ErrorOrderContacted);
            }else{
                ordering = orderingRepository.findById(orderContacted.getOrderId()).orElse(null);
            }
            if(ordering==null){
                throw new BadRequestExceptions(Constants.ErrorOrdering);
            }
            try{
                orderingRepository.save(ordering);
//                orderContacted.setContacted(true);
//                orderContacted.setUpdateDate(OffsetDateTime.now());
//                orderContacted.setUser(user);
//                orderContacted.setUserId(user.getId());
//                orderContacted.setClient(user.getClient());
//                orderContacted.setClientId(user.getClientId());

//                if(observations != null){
//                    orderContacted.setObservations(orderContacted.getObservations() + " " + observations);
//                }
                orderContactedRepository.markContacted(
                        true,
                        OffsetDateTime.now(),
                        user.getId(),
                        user.getClientId(),
                        orderContacted.getObservations() + " " + observations,
                        orderId
                );

                iOrderLog.save(
                        user,
                        orderContacted.getOrdering(),
                        OffsetDateTime.now()+
                                " - "+
                                user.getUsername()+
                                " "+"Estado contactado : Contactado"
                );
                iAudit.save(
                        "ADD_ORDER_CONTACTED",
                        "PEDIDO "+
                                orderContacted.getOrdering().getOrderNumber()+
                                " AGREGADO A CONTACT CENTER.",
                        orderContacted.getOrdering().getOrderNumber().toString(),
                        user.getUsername());
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
    public CompletableFuture<Page<OrderContactedDTO>> list(
            String username,
            Long orderNumber,
            String deliveryZone,
            Boolean contacted,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) {
        return CompletableFuture.supplyAsync(()->{
            Page<OrderContacted> orderContactedPage;
            UUID clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(username.toUpperCase()).getClientId();
                orderContactedPage = orderContactedRepositoryCustom.searchForContactedOrder(
                        clientId,
                        orderNumber,
                        deliveryZone,
                        contacted,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateEndDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize);
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if (orderContactedPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }
            List<OrderContactedDTO> orderContactedDTOS = new ArrayList<>(orderContactedPage.stream().map(orderContacted -> {
                List<String> paymentReceipts = orderPaymentReceiptRepository.findAllByOrderId(orderContacted.getOrderId()).stream().map(OrderPaymentReceipt::getPaymentReceiptUrl).toList();
                List<String> courierPictures = courierPictureRepository.findAllByOrderId(orderContacted.getOrderId()).stream().map(CourierPicture::getPictureUrl).toList();
                List<OrderItem> orderItems = orderItemRepository.findAllByOrderIdAndStatusTrue(orderContacted.getOrderId());

                double saleAmount = 0.00;
                for (OrderItem orderItem : orderItems) {
                    ProductPrice productPrice = productPriceRepository.findByProductIdAndStatusTrue(orderItem.getProductId());
                    if (Objects.equals(orderItem.getDiscount().getName(), "PORCENTAJE")) {
                        saleAmount += (productPrice.getUnitSalePrice() * orderItem.getQuantity()) - ((productPrice.getUnitSalePrice() * orderItem.getQuantity()) * (orderItem.getDiscountAmount() / 100));
                    }
                    if (Objects.equals(orderItem.getDiscount().getName(), "MONTO")) {
                        saleAmount += (productPrice.getUnitSalePrice() * orderItem.getQuantity()) - orderItem.getDiscountAmount();
                    }
                    if (Objects.equals(orderItem.getDiscount().getName(), "NO APLICA")) {
                        saleAmount += (productPrice.getUnitSalePrice() * orderItem.getQuantity());
                    }
                }
                double totalDuePayment = 0;
                if (Objects.equals(orderContacted.getOrdering().getDiscount().getName(), "PORCENTAJE")) {
                    totalDuePayment = (saleAmount - ((saleAmount) * (orderContacted.getOrdering().getDiscountAmount() / 100)) + orderContacted.getOrdering().getDeliveryAmount()) - orderContacted.getOrdering().getAdvancedPayment();
                }
                if (Objects.equals(orderContacted.getOrdering().getDiscount().getName(), "MONTO")) {
                    totalDuePayment = (saleAmount - orderContacted.getOrdering().getDiscountAmount() + orderContacted.getOrdering().getDeliveryAmount()) - orderContacted.getOrdering().getAdvancedPayment();
                }
                if (Objects.equals(orderContacted.getOrdering().getDiscount().getName(), "NO APLICA")) {
                    totalDuePayment = (saleAmount + orderContacted.getOrdering().getDeliveryAmount()) - orderContacted.getOrdering().getAdvancedPayment();
                }
                return OrderContactedDTO.builder()
                        .user(orderContacted.getUser().getUsername())
                        .orderContactedId(orderContacted.getId())
                        .orderId(orderContacted.getOrdering().getId())
                        .orderNumber(orderContacted.getOrdering().getOrderNumber())
                        .customerName(orderContacted.getOrdering().getCustomer().getName())
                        .customerPhone(orderContacted.getOrdering().getCustomer().getPhone())
                        .customerType(orderContacted.getOrdering().getCustomer().getCustomerType().getName())
                        .closingChannel(orderContacted.getOrdering().getClosingChannel().getName())
                        .orderStatus(orderContacted.getOrdering().getOrderState().getName())
                        .department(orderContacted.getOrdering().getCustomer().getDistrict().getProvince().getDepartment().getName())
                        .province(orderContacted.getOrdering().getCustomer().getDistrict().getProvince().getName())
                        .district(orderContacted.getOrdering().getCustomer().getDistrict().getName())
                        .address(orderContacted.getOrdering().getCustomer().getAddress())
                        .customerAddress(orderContacted.getOrdering().getCustomer().getAddress())
                        .instagram(orderContacted.getOrdering().getCustomer().getInstagram())
                        .managementType(orderContacted.getOrdering().getManagementType().getName())
                        .reference(orderContacted.getOrdering().getCustomer().getReference())
                        .saleChannel(orderContacted.getOrdering().getSaleChannel().getName())
                        .sellerName(orderContacted.getOrdering().getSeller())
                        .registrationDate(orderContacted.getOrdering().getRegistrationDate())
                        .updateDate(orderContacted.getOrdering().getUpdateDate())
                        .paymentMethod(orderContacted.getOrdering().getOrderPaymentMethod().getName())
                        .paymentState(orderContacted.getOrdering().getOrderPaymentState().getName())
                        .deliveryAddress(orderContacted.getOrdering().getDeliveryAddress())
                        .courier(orderContacted.getOrdering().getCourier().getName())
                        .paymentReceipts(paymentReceipts)
                        .contactedObservations(orderContacted.getObservations())
                        .courierPictures(courierPictures)
                        .observations(orderContacted.getOrdering().getObservations())
                        .saleAmount(BigDecimal.valueOf(saleAmount).setScale(2, RoundingMode.HALF_EVEN))
                        .advancedPayment(BigDecimal.valueOf(orderContacted.getOrdering().getAdvancedPayment()).setScale(2, RoundingMode.HALF_EVEN))
                        .duePayment(BigDecimal.valueOf(totalDuePayment).setScale(2, RoundingMode.HALF_EVEN))
                        .deliveryAmount(BigDecimal.valueOf(orderContacted.getOrdering().getDeliveryAmount()).setScale(2, RoundingMode.HALF_EVEN))
                        .deliveryPoint(orderContacted.getOrdering().getDeliveryPoint().getName())
                        .discount(orderContacted.getOrdering().getDiscount().getName())
                        .discountAmount(BigDecimal.valueOf(orderContacted.getOrdering().getDiscountAmount()))
                        .dni(orderContacted.getOrdering().getCustomer().getDni())
                        .store(orderContacted.getOrdering().getStore().getName())
                        .receiptFlag(orderContacted.getOrdering().getReceiptFlag())
                        .deliveryFlag(orderContacted.getOrdering().getDeliveryFlag())
                        .orderStateColor(orderContacted.getOrdering().getOrderState().getHexColor())
                        .contacted(orderContacted.getContacted())
                        .orderItemDTOS(orderItems.stream().map(orderItem -> {
                            ProductPrice productPrice = productPriceRepository.findByProductIdAndStatusTrue(orderItem.getProductId());
                            List<ProductPicture> productPictures = productPictureRepository.findAlByClientIdAndProductId(clientId, orderItem.getProductId());
                            Double totalPrice = null;
                            if (Objects.equals(orderItem.getDiscount().getName(), "PORCENTAJE")) {
                                totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity()) - ((productPrice.getUnitSalePrice() * orderItem.getQuantity()) * (orderItem.getDiscountAmount() / 100));
                            }

                            if (Objects.equals(orderItem.getDiscount().getName(), "MONTO")) {
                                totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity()) - (orderItem.getDiscountAmount());
                            }

                            if (Objects.equals(orderItem.getDiscount().getName(), "NO APLICA")) {
                                totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity());
                            }
                            String finalSku = iUtil.buildProductSku(orderItem.getProduct());
                            return OrderItemDTO.builder()
                                    .id(orderItem.getId())
                                    .orderId(orderContacted.getOrderId())
                                    .productId(orderItem.getProductId())
                                    .user(orderItem.getUser().getUsername())
                                    .status(orderItem.getStatus())
                                    .model(orderItem.getProduct().getModel().getName())
                                    .discountAmount(orderItem.getDiscountAmount())
                                    .sku(finalSku)
                                    .unit(orderItem.getProduct().getUnit().getName())
                                    .observations(orderItem.getObservations())
                                    .quantity(orderItem.getQuantity())
                                    .size(orderItem.getProduct().getSize().getName())
                                    .discount(orderItem.getDiscount().getName())
                                    .selectOrderStatus(orderItem.getStatus())
                                    .pictures(productPictures.stream().map(ProductPicture::getProductPictureUrl).toList())
                                    .unitPrice(productPrice.getUnitSalePrice())
                                    .totalPrice(totalPrice)
                                    .color(orderItem.getProduct().getColor().getName())
                                    .category(orderItem.getProduct().getSubCategoryProduct().getCategoryProduct().getName())
                                    .subCategory(orderItem.getProduct().getSubCategoryProduct().getName())
                                    .registrationDate(orderItem.getRegistrationDate())
                                    .updateDate(orderItem.getUpdateDate())
                                    .build();
                        }).toList())
                        .orderLogs(iOrderLog.listLogByOrder(orderContacted.getOrderId()))
                        .deliveryZone(orderContacted.getDeliveryZone().getName())
                        .build();
            }).toList());
            List<String> zoneOrder = List.of("CENTRO", "SUR", "NORTE", "CALLAO", "ESTE 1", "PERIFERICA", "PROVINCIA");
            orderContactedDTOS.sort(Comparator.comparing(dto -> zoneOrder.indexOf(dto.getDeliveryZone())));
            return new PageImpl<>(orderContactedDTOS,
                    orderContactedPage.getPageable(), orderContactedPage.getTotalElements());
            });
    }

    @Override
    public CompletableFuture<UserDTO> lisUserAgent(String userName) throws BadRequestExceptions {
        return null;
                //CompletableFuture.supplyAsync(()->{
//            UUID clientId;
//            List<User> usersAgent;
        //});
    }

    @Transactional
    @Override
    public CompletableFuture<ResponseSuccess> selectAgent(UUID orderId, String username, String agentUsername, String observations) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            User agent;
            OrderContacted orderContacted;
            OrderState orderState;
            Ordering ordering;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                orderContacted = orderContactedRepository.findByOrderId(orderId);
                agent = userRepository.findByUsernameAndStatusTrue(agentUsername.toUpperCase());
                orderState = orderStateRepository.findByNameAndStatusTrue("LLAMADO");
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(orderContacted==null){
                throw new BadRequestExceptions(Constants.ErrorOrderContacted);
            }else{
                ordering = orderingRepository.findById(orderContacted.getOrderId()).orElse(null);
            }
            if(ordering==null){
                throw new BadRequestExceptions(Constants.ErrorOrdering);
            }
            if(agent==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(orderState==null){
                throw new BadRequestExceptions(Constants.ErrorOrderState);
            }
            try{
                ordering.setOrderState(orderState);
                ordering.setOrderStateId(orderState.getId());
                orderingRepository.save(ordering);
//                orderContacted.setUpdateDate(OffsetDateTime.now());
//                orderContacted.setUser(user);
//                orderContacted.setUserId(user.getId());
//                orderContacted.setAgent(agent);
//                orderContacted.setAgentId(agent.getId());

//                if(observations != null){
//                    orderContacted.setObservations(orderContacted.getObservations() + " " + observations);
//                }

                //orderContactedRepository.save(orderContacted);

                orderContactedRepository.selectAgentOrderContact(
                        orderId,
                        user.getId(),
                        agent.getId(),
                        OffsetDateTime.now(),
                        orderContacted.getObservations() + " " + observations
                );

                iAudit.save(
                        "ADD_ORDER_CONTACTED",
                        "PEDIDO "+
                                orderContacted.getOrdering().getOrderNumber()+
                                " AGREGADO A CONTACT CENTER.",
                        orderContacted.getOrdering().getOrderNumber().toString(),
                        user.getUsername());
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

    @Transactional
    @Override
    public CompletableFuture<ResponseSuccess> selectCourier(UUID orderId, String username, String courierName, String observations) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Courier courier;
            OrderContacted orderContacted;
            OrderState orderState;
            Ordering ordering;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                orderContacted = orderContactedRepository.findByOrderId(orderId);

                orderState = orderStateRepository.findByNameAndStatusTrue("EN RUTA");
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                courier = courierRepository.findByNameAndClientIdAndStatusTrue(courierName.toUpperCase(),user.getClientId());
            }
            if(orderContacted==null){
                throw new BadRequestExceptions(Constants.ErrorOrderContacted);
            }else{
                ordering = orderingRepository.findById(orderContacted.getOrderId()).orElse(null);
            }
            if(ordering==null){
                throw new BadRequestExceptions(Constants.ErrorOrdering);
            }
            if(courier==null){
                throw new BadRequestExceptions(Constants.ErrorCourier);
            }
            if(orderState==null){
                throw new BadRequestExceptions(Constants.ErrorOrderState);
            }
            try{
                ordering.setOrderState(orderState);
                ordering.setOrderStateId(orderState.getId());
                ordering.setCourier(courier);
                ordering.setCourierId(courier.getId());
                orderingRepository.save(ordering); // no se esta setendo el id de los couiers
//                orderContacted.setUpdateDate(OffsetDateTime.now());
//                orderContacted.setUser(user);
//                orderContacted.setUserId(user.getId());
//                if(observations != null){
//                    orderContacted.setObservations(orderContacted.getObservations() + " " + observations);
//                }
                orderContactedRepository.selectCourierQuery(
                        OffsetDateTime.now(),
                        user.getId(),
                        orderContacted.getObservations() + " " + observations,
                        orderId
                );
                iOrderLog.save(user,ordering,
                        OffsetDateTime.now()+
                                " - "+
                                user.getUsername()+
                                " "+ordering.getOrderState().getName());
                iAudit.save(
                        "ADD_ORDER_CONTACTED",
                        "PEDIDO "+
                                orderContacted.getOrdering().getOrderNumber()+
                                " AGREGADO A CONTACT CENTER.",
                        orderContacted.getOrdering().getOrderNumber().toString(),
                        user.getUsername());
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
