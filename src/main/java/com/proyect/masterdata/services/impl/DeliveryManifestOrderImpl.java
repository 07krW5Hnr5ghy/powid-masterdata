package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.request.RequestDeliveryManifestOrder;
import com.proyect.masterdata.dto.request.RequestDeliveryManifestOrderMark;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IDeliveryManifestOrder;
import com.proyect.masterdata.services.IOrderLog;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class DeliveryManifestOrderImpl implements IDeliveryManifestOrder {
    private final DeliveryManifestOrderRepository deliveryManifestOrderRepository;
    private final DeliveryManifestRepository deliveryManifestRepository;
    private final UserRepository userRepository;
    private final OrderingRepository orderingRepository;
    private final IOrderLog iOrderLog;
    private final OrderPaymentMethodRepository orderPaymentMethodRepository;
    private final OrderDeliveryStatusRepository orderDeliveryStatusRepository;
    @Override
    public CompletableFuture<ResponseSuccess> save(RequestDeliveryManifestOrder requestDeliveryManifestOrder) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            DeliveryManifestOrder deliveryManifestOrder;
            DeliveryManifest deliveryManifest;
            User user;
            Ordering ordering;
            OrderPaymentMethod orderPaymentMethod;
            OrderDeliveryStatus orderDeliveryStatus;

            try {
                user = userRepository.findByUsernameAndStatusTrue(requestDeliveryManifestOrder.getUsername().toUpperCase());
                deliveryManifest = deliveryManifestRepository.findById(requestDeliveryManifestOrder.getDeliveryManifestId()).orElse(null);
                ordering = orderingRepository.findById(requestDeliveryManifestOrder.getOrderId()).orElse(null);
                orderPaymentMethod = orderPaymentMethodRepository.findByNameAndStatusTrue(requestDeliveryManifestOrder.getPaymentMethod().toUpperCase());
            }catch (RuntimeException e){
                e.printStackTrace();
                log.error(e.getMessage());
                throw new  InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                orderDeliveryStatus = orderDeliveryStatusRepository.findByNameAndClientIdAndStatusTrue("POR ENTREGAR",user.getClientId());
            }
            if(ordering==null){
                throw new BadRequestExceptions(Constants.ErrorOrdering);
            }
            if(deliveryManifest==null){
                throw new BadRequestExceptions(Constants.ErrorDeliveryManifest);
            }else{
                deliveryManifestOrder = deliveryManifestOrderRepository.findByDeliveryManifestIdAndOrderIdAndClientId(deliveryManifest.getId(),ordering.getId(),user.getClientId());
            }
            if(orderPaymentMethod==null){
                throw new BadRequestExceptions(Constants.ErrorPaymentMethod);
            }
            if(orderDeliveryStatus==null){
                throw new BadRequestExceptions(Constants.ErrorOrderDeliveryStatus);
            }
            try {
                if(deliveryManifestOrder==null){
                    deliveryManifestOrder = DeliveryManifestOrder.builder()
                            .orderId(ordering.getId())
                            .ordering(ordering)
                            .deliveryManifestId(deliveryManifest.getId())
                            .deliveryManifest(deliveryManifest)
                            .registrationDate(OffsetDateTime.now())
                            .updateDate(OffsetDateTime.now())
                            .userId(user.getId())
                            .user(user)
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .observations(requestDeliveryManifestOrder.getObservations()!=null? requestDeliveryManifestOrder.getObservations():"Sin observaciones")
                            .receivedAmount(requestDeliveryManifestOrder.getReceivedAmount()!=null? requestDeliveryManifestOrder.getReceivedAmount():0.00)
                            .deliveryFeeCollected(requestDeliveryManifestOrder.getDeliveryFeeCollected())
                            .orderPaymentMethod(orderPaymentMethod)
                            .paymentMethodId(orderPaymentMethod.getId())
                            .delivered(false)
                            .orderDeliveryStatus(orderDeliveryStatus)
                            .orderDeliveryStatusId(orderDeliveryStatus.getId())
                            .build();
                }else{
                    deliveryManifestOrder.setOrdering(ordering);
                    deliveryManifestOrder.setOrderId(ordering.getId());
                    deliveryManifestOrder.setDeliveryManifest(deliveryManifest);
                    deliveryManifestOrder.setDeliveryManifestId(deliveryManifest.getId());
                    deliveryManifestOrder.setDelivered(false);
                    deliveryManifestOrder.setOrderDeliveryStatus(orderDeliveryStatus);
                    deliveryManifestOrder.setOrderDeliveryStatusId(orderDeliveryStatus.getId());
                    deliveryManifestOrder.setObservations(requestDeliveryManifestOrder.getObservations()!=null? requestDeliveryManifestOrder.getObservations():"Sin observaciones");
                    deliveryManifestOrder.setUpdateDate(OffsetDateTime.now());
                    deliveryManifestOrder.setUser(user);
                    deliveryManifestOrder.setUserId(user.getId());
                    deliveryManifestOrder.setClient(user.getClient());
                    deliveryManifestOrder.setClientId(user.getClientId());
                    deliveryManifestOrder.setReceivedAmount(requestDeliveryManifestOrder.getReceivedAmount()!=null? requestDeliveryManifestOrder.getReceivedAmount():0.00);
                    deliveryManifestOrder.setDeliveryFeeCollected(requestDeliveryManifestOrder.getDeliveryFeeCollected());
                    deliveryManifestOrder.setOrderPaymentMethod(orderPaymentMethod);
                    deliveryManifestOrder.setPaymentMethodId(orderPaymentMethod.getId());
                }
                if(requestDeliveryManifestOrder.getObservations() != null && requestDeliveryManifestOrder.getReceivedAmount() != null){
                    iOrderLog.save(user,ordering,
                            "Observaciones de guia de motorizado # "
                                    + deliveryManifest.getManifestNumber() + " registradas : "
                                    + requestDeliveryManifestOrder.getObservations() + " y monto recibido de : $"
                                    + requestDeliveryManifestOrder.getReceivedAmount() + " ."
                    );
                }else if(requestDeliveryManifestOrder.getObservations()!=null){
                    iOrderLog.save(user,ordering,
                            "Observaciones de guia de motorizado # "
                                    + deliveryManifest.getManifestNumber() + " registradas : "
                                    + requestDeliveryManifestOrder.getObservations() + " ."
                    );
                }else if(requestDeliveryManifestOrder.getReceivedAmount() != null){
                    iOrderLog.save(user,ordering,
                            "Monto recibido en guia de motorizado # "
                                    + deliveryManifest.getManifestNumber() + " de : $"
                                    + requestDeliveryManifestOrder.getReceivedAmount() + " ."
                    );
                }
                deliveryManifestOrderRepository.save(deliveryManifestOrder);
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
    public CompletableFuture<ResponseSuccess> markDeliveredOperationsOrders(RequestDeliveryManifestOrderMark requestDeliveryManifestOrderMark) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            OrderDeliveryStatus orderDeliveryStatus;
            DeliveryManifest deliveryManifest;
            DeliveryManifestOrder deliveryManifestOrder = null;
            try{
                user = userRepository.findByUsernameAndStatusTrue(requestDeliveryManifestOrderMark.getUsername().toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                orderDeliveryStatus = orderDeliveryStatusRepository.findByNameAndClientIdAndStatusTrue(
                        "POR ENTREGAR",
                        user.getClientId()
                );
                deliveryManifest = deliveryManifestRepository.findById(requestDeliveryManifestOrderMark.getDeliveryManifestId()).orElse(null);
            }
            if(deliveryManifest==null){
                throw new BadRequestExceptions(Constants.ErrorDeliveryManifest);
            }
            try {
                for(UUID orderId: requestDeliveryManifestOrderMark.getOrderIds()){
                    deliveryManifestOrder = deliveryManifestOrderRepository.findByDeliveryManifestIdAndOrderIdAndClientId(
                            requestDeliveryManifestOrderMark.getDeliveryManifestId(),
                            orderId,
                            user.getClientId()
                    );
                    Ordering ordering = orderingRepository.findByClientIdAndId(user.getClientId(),orderId);

                    if(deliveryManifestOrder!=null&&!deliveryManifestOrder.getDelivered()){
                        deliveryManifestOrderRepository.setDeliveryManifestOrderDeliveredTrue(
                                orderId,
                                deliveryManifest.getId(),
                                user.getClientId()
                        );
                    }
                    if(deliveryManifestOrder==null){
                        deliveryManifestOrderRepository.save(DeliveryManifestOrder.builder()
                                        .orderDeliveryStatus(orderDeliveryStatus)
                                        .orderDeliveryStatusId(orderDeliveryStatus.getId())
                                        .delivered(true)
                                        .orderId(ordering.getId())
                                        .ordering(ordering)
                                        .deliveryManifest(deliveryManifest)
                                        .deliveryManifestId(deliveryManifest.getId())
                                        .deliveryFeeCollected(false)
                                        .clientId(user.getClientId())
                                        .client(user.getClient())
                                        .userId(user.getId())
                                        .user(user)
                                        .receivedAmount(0.00)
                                        .registrationDate(OffsetDateTime.now())
                                        .observations("SIN OBSERVACIONES")
                                        .orderPaymentMethod(ordering.getOrderPaymentMethod())
                                        .paymentMethodId(ordering.getPaymentMethodId())
                                .build());
                    }
                    if(deliveryManifestOrder!= null && deliveryManifestOrder.getObservations() != null && deliveryManifestOrder.getReceivedAmount() != null){
                        iOrderLog.save(user,ordering,
                                "Observaciones de guia de motorizado # "
                                        + deliveryManifest.getManifestNumber() + " registradas : "
                                        + deliveryManifestOrder.getObservations() + " y monto recibido de : $"
                                        + deliveryManifestOrder.getReceivedAmount() + " ."
                        );
                    }else if(deliveryManifestOrder!= null && deliveryManifestOrder.getObservations()!=null){
                        iOrderLog.save(user,ordering,
                                "Observaciones de guia de motorizado # "
                                        + deliveryManifest.getManifestNumber() + " registradas : "
                                        + deliveryManifestOrder.getObservations() + " ."
                        );
                    }else if(deliveryManifestOrder!= null && deliveryManifestOrder.getReceivedAmount() != null){
                        iOrderLog.save(user,ordering,
                                "Monto recibido en guia de motorizado # "
                                        + deliveryManifest.getManifestNumber() + " de : $"
                                        + deliveryManifestOrder.getReceivedAmount() + " ."
                        );
                    }
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
}
