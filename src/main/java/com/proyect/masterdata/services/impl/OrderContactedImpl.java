package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.OrderContactedDTO;
import com.proyect.masterdata.dto.OrderDTO;
import com.proyect.masterdata.dto.OrderItemDTO;
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

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.OffsetDateTime;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

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
    @Override
    public CompletableFuture<OrderContacted> save(UUID orderId, String username) throws BadRequestExceptions, InternalErrorExceptions {
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
                OrderContacted newOrderContacted = orderContactedRepository.save(OrderContacted.builder()
                                .orderId(ordering.getId())
                                .ordering(ordering)
                                .contacted(false)
                                .registrationDate(OffsetDateTime.now())
                                .updateDate(OffsetDateTime.now())
                                .user(user)
                                .userId(user.getId())
                                .client(user.getClient())
                                .clientId(user.getClientId())
                        .build());
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
                return newOrderContacted;
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> markContacted(UUID orderId, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            OrderContacted orderContacted;
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
            }
            try{
                orderContacted.setContacted(true);
                orderContacted.setUpdateDate(OffsetDateTime.now());
                orderContacted.setUser(user);
                orderContacted.setUserId(user.getId());
                orderContacted.setClient(user.getClient());
                orderContacted.setClientId(user.getClientId());
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
            List<OrderContactedDTO> orderContactedDTOS = orderContactedPage.stream().map(orderContacted -> {
                List<String> paymentReceipts = orderPaymentReceiptRepository.findAllByOrderId(orderContacted.getOrderId()).stream().map(OrderPaymentReceipt::getPaymentReceiptUrl).toList();
                List<String> courierPictures = courierPictureRepository.findAllByOrderId(orderContacted.getOrderId()).stream().map(CourierPicture::getPictureUrl).toList();
                List<OrderItem> orderItems = orderItemRepository.findAllByOrderIdAndStatusTrue(orderContacted.getOrderId());

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
                if(Objects.equals(orderContacted.getOrdering().getDiscount().getName(), "PORCENTAJE")){
                    totalDuePayment = (saleAmount-((saleAmount)*(orderContacted.getOrdering().getDiscountAmount()/100))+orderContacted.getOrdering().getDeliveryAmount())-orderContacted.getOrdering().getAdvancedPayment();
                }
                if(Objects.equals(orderContacted.getOrdering().getDiscount().getName(), "MONTO")){
                    totalDuePayment = (saleAmount-orderContacted.getOrdering().getDiscountAmount()+orderContacted.getOrdering().getDeliveryAmount())-orderContacted.getOrdering().getAdvancedPayment();
                }
                if(Objects.equals(orderContacted.getOrdering().getDiscount().getName(), "NO APLICA")){
                    totalDuePayment = (saleAmount+orderContacted.getOrdering().getDeliveryAmount())-orderContacted.getOrdering().getAdvancedPayment();
                }
                return OrderContactedDTO.builder()
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
                        .courierPictures(courierPictures)
                        .observations(orderContacted.getOrdering().getObservations())
                        .saleAmount(BigDecimal.valueOf(saleAmount).setScale(2, RoundingMode.HALF_EVEN))
                        .advancedPayment(BigDecimal.valueOf(orderContacted.getOrdering().getAdvancedPayment()).setScale(2, RoundingMode.HALF_EVEN))
                        .duePayment(BigDecimal.valueOf(totalDuePayment).setScale(2,RoundingMode.HALF_EVEN))
                        .deliveryAmount(BigDecimal.valueOf(orderContacted.getOrdering().getDeliveryAmount()).setScale(2,RoundingMode.HALF_EVEN))
                        .deliveryPoint(orderContacted.getOrdering().getDeliveryPoint().getName())
                        .discount(orderContacted.getOrdering().getDiscount().getName())
                        .discountAmount(BigDecimal.valueOf(orderContacted.getOrdering().getDiscountAmount()))
                        .dni(orderContacted.getOrdering().getCustomer().getDni())
                        .store(orderContacted.getOrdering().getStore().getName())
                        .receiptFlag(orderContacted.getOrdering().getReceiptFlag())
                        .deliveryFlag(orderContacted.getOrdering().getDeliveryFlag())
                        .orderStateColor(orderContacted.getOrdering().getOrderState().getHexColor())
                        .orderItemDTOS(orderItems.stream().map(orderItem -> {
                            ProductPrice productPrice = productPriceRepository.findByProductId(orderItem.getProductId());
                            List<ProductPicture> productPictures = productPictureRepository.findAlByClientIdAndProductId(clientId,orderItem.getProductId());
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
                            String finalSku = iUtil.buildProductSku(orderItem.getProduct());
                            return OrderItemDTO.builder()
                                    .orderId(orderItem.getOrderId())
                                    .model(orderItem.getProduct().getModel().getName())
                                    .discountAmount(orderItem.getDiscountAmount())
                                    .sku(finalSku)
                                    .unit(orderItem.getProduct().getUnit().getName())
                                    .observations(orderItem.getObservations())
                                    .quantity(orderItem.getQuantity())
                                    .size(orderItem.getProduct().getSize().getName())
                                    .discount(orderItem.getDiscount().getName())
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
                        .build();
            }).toList();
            return new PageImpl<>(orderContactedDTOS,
                    orderContactedPage.getPageable(), orderContactedPage.getTotalElements());
            });
    }
}
