package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.request.RequestDeliveryManifest;
import com.proyect.masterdata.dto.request.RequestDeliveryManifestItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.CourierRepository;
import com.proyect.masterdata.repository.DeliveryManifestRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.repository.WarehouseRepository;
import com.proyect.masterdata.services.IDeliveryManifest;
import com.proyect.masterdata.services.IDeliveryManifestItem;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class DeliveryManifestImpl implements IDeliveryManifest {
    private final UserRepository userRepository;
    private final DeliveryManifestRepository deliveryManifestRepository;
    private final CourierRepository courierRepository;
    private final IDeliveryManifestItem iDeliveryManifestItem;
    private final WarehouseRepository warehouseRepository;
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
                DeliveryManifest deliveryManifest = deliveryManifestRepository.save(DeliveryManifest.builder()
                                .manifestNumber(deliveryManifestNumber)
                                .courier(courier)
                                .registrationDate(OffsetDateTime.now())
                                .updateDate(OffsetDateTime.now())
                                .user(user)
                                .userId(user.getId())
                                .client(user.getClient())
                                .clientId(user.getClientId())
                        .build());
                for(RequestDeliveryManifestItem requestDeliveryManifestItem:requestDeliveryManifest.getRequestDeliveryManifestItems()){
                    iDeliveryManifestItem.save(requestDeliveryManifestItem,deliveryManifest,warehouse,user);
                }
                return ResponseSuccess.builder()
                        .message(Constants.register)
                        .code(200)
                        .build();
            }catch (RuntimeException | InterruptedException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }
}
