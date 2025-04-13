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
            if(deliveryManifestOrder!=null){
                throw new BadRequestExceptions(Constants.ErrorDeliveryManifestOrderExist);
            }
            if(orderPaymentMethod==null){
                throw new BadRequestExceptions(Constants.ErrorPaymentMethod);
            }
            if(orderDeliveryStatus==null){
                throw new BadRequestExceptions(Constants.ErrorOrderDeliveryStatus);
            }
            try {
                DeliveryManifestOrder newDeliveryManifestOrder = DeliveryManifestOrder.builder()
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
                        .delivered(requestDeliveryManifestOrder.getDelivered())
                        .orderDeliveryStatus(orderDeliveryStatus)
                        .orderDeliveryStatusId(orderDeliveryStatus.getId())
                        .build();
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
                deliveryManifestOrderRepository.save(newDeliveryManifestOrder);
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
            try{
                user = userRepository.findByUsernameAndStatusTrue(requestDeliveryManifestOrderMark.getUsername().toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            try {
                for(UUID orderId: requestDeliveryManifestOrderMark.getOrderIds()){
                    DeliveryManifestOrder deliveryManifestOrder = deliveryManifestOrderRepository.findByDeliveryManifestIdAndOrderIdAndClientId(
                            requestDeliveryManifestOrderMark.getDeliveryManifestId(),
                            orderId,
                            user.getClientId()
                    );
                    deliveryManifestOrder.setDelivered(true);
                    deliveryManifestOrderRepository.save(deliveryManifestOrder);
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
