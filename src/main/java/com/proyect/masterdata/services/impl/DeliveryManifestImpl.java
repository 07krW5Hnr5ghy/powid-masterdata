package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.DeliveryManifestDTO;
import com.proyect.masterdata.dto.DeliveryManifestItemDTO;
import com.proyect.masterdata.dto.request.RequestDeliveryManifest;
import com.proyect.masterdata.dto.request.RequestDeliveryManifestItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IDeliveryManifest;
import com.proyect.masterdata.services.IDeliveryManifestItem;
import com.proyect.masterdata.services.IUtil;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
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
    private final DeliveryManifestItemRepository deliveryManifestItemRepository;
    private final IUtil iUtil;
    private final DeliveryStatusRepository deliveryStatusRepository;
    @Override
    public CompletableFuture<ResponseSuccess> save(RequestDeliveryManifest requestDeliveryManifest) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Courier courier;
            Warehouse warehouse;
            DeliveryStatus deliveryStatus;
            try{
                user = userRepository.findByUsernameAndStatusTrue(requestDeliveryManifest.getUsername().toUpperCase());
                courier = courierRepository.findByNameAndStatusTrue(requestDeliveryManifest.getCourier().toUpperCase());
                warehouse = warehouseRepository.findByNameAndStatusTrue(requestDeliveryManifest.getWarehouse().toUpperCase());
                deliveryStatus = deliveryStatusRepository.findByName("PENDIENTE");
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
                                .deliveryStatus(deliveryStatus)
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

    @Override
    public CompletableFuture<DeliveryManifestDTO> getById(UUID deliveryManifestId,String username) throws InternalErrorExceptions, BadRequestExceptions {
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
                throw new BadRequestExceptions(Constants.ErrorDeliveryStatusExists);
            }
            try{
                List<DeliveryManifestItemDTO> deliveryManifestItemList = deliveryManifestItemRepository.findAllById(deliveryManifest.getId())
                        .stream().map(deliveryManifestItem -> DeliveryManifestItemDTO.builder()
                                .quantity(deliveryManifestItem.getQuantity())
                                .skuInventory(iUtil.buildInventorySku(deliveryManifestItem.getSupplierProduct()))
                                .skuProduct(iUtil.buildProductSku(deliveryManifestItem.getOrderItem().getProduct()))
                                .orderNumber(deliveryManifestItem.getOrderItem().getOrdering().getOrderNumber())
                                .district(deliveryManifestItem.getOrderItem().getOrdering().getCustomer().getDistrict().getName())
                                .customer(deliveryManifestItem.getOrderItem().getOrdering().getCustomer().getName())
                                .phone(deliveryManifestItem.getOrderItem().getOrdering().getCustomer().getPhone())
                                .build()).toList();
                return DeliveryManifestDTO.builder()
                        .id(deliveryManifest.getId())
                        .manifestNumber(deliveryManifest.getManifestNumber())
                        .registrationDate(deliveryManifest.getRegistrationDate())
                        .updateDate(deliveryManifest.getUpdateDate())
                        .courier(deliveryManifest.getCourier().getName())
                        .deliveryStatus(deliveryManifest.getDeliveryStatus().getName())
                        .deliveryManifestItemDTOS(deliveryManifestItemList)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }
}
