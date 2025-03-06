package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
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
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
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
                                .open(true)
                                .user(user)
                                .userId(user.getId())
                                .client(user.getClient())
                                .clientId(user.getClientId())
                                .warehouse(warehouse)
                                .warehouseId(warehouse.getId())
                        .build());
                List<RequestStockTransactionItem> stockTransactionList = new ArrayList<>();
                for(RequestDeliveryManifestItem requestDeliveryManifestItem:requestDeliveryManifest.getRequestDeliveryManifestItems()){
                    CompletableFuture<DeliveryManifestItem> deliveryManifestItem = iDeliveryManifestItem.save(requestDeliveryManifestItem,deliveryManifest,warehouse,user);
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
                        .open(deliveryManifest.getOpen())
                        .deliveryManifestItemDTOS(deliveryManifestItemList)
                        .warehouse(deliveryManifest.getWarehouse().getName())
                        .build();
            }catch (RuntimeException e){
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
            List<DeliveryManifestDTO> deliveryManifestDTOS = deliveryManifestPage.getContent().stream().map(deliveryManifest -> {
                List<DeliveryManifestItemDTO> deliveryManifestItemDTOS = deliveryManifestItemRepository.findAllById(deliveryManifest.getId())
                        .stream().map(deliveryManifestItem -> DeliveryManifestItemDTO.builder()
                                .id(deliveryManifestItem.getId())
                                .manifestNumber(deliveryManifestItem.getDeliveryManifest().getManifestNumber())
                                .phone(deliveryManifestItem.getOrderItem().getOrdering().getCustomer().getPhone())
                                .customer(deliveryManifestItem.getOrderItem().getOrdering().getCustomer().getName())
                                .district(deliveryManifestItem.getOrderItem().getOrdering().getCustomer().getDistrict().getName())
                                .orderNumber(deliveryManifestItem.getOrderItem().getOrdering().getOrderNumber())
                                .quantity(deliveryManifestItem.getQuantity())
                                .skuProduct(iUtil.buildProductSku(deliveryManifestItem.getProduct()))
                                .management(deliveryManifestItem.getOrderItem().getOrdering().getManagementType().getName())
                                .build()).toList();
                return DeliveryManifestDTO.builder()
                        .manifestNumber(deliveryManifest.getManifestNumber())
                        .id(deliveryManifest.getId())
                        .open(deliveryManifest.getOpen())
                        .courier(deliveryManifest.getCourier().getName())
                        .warehouse(deliveryManifest.getWarehouse().getName())
                        .registrationDate(deliveryManifest.getRegistrationDate())
                        .updateDate(deliveryManifest.getUpdateDate())
                        .deliveryManifestItemDTOS(deliveryManifestItemDTOS)
                        .build();
            }).toList();
            return new PageImpl<>(deliveryManifestDTOS,deliveryManifestPage.getPageable(),deliveryManifestPage.getTotalElements());
        });
    }
}
