package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Courier;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.Warehouse;
import com.proyect.masterdata.domain.WarehouseOutput;
import com.proyect.masterdata.dto.WarehouseOutputDTO;
import com.proyect.masterdata.dto.WarehouseOutputItemDTO;
import com.proyect.masterdata.dto.request.RequestWarehouseOutput;
import com.proyect.masterdata.dto.request.RequestWarehouseOutputItem;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IUtil;
import com.proyect.masterdata.services.IWarehouseOutput;
import com.proyect.masterdata.services.IWarehouseOutputItem;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class WarehouseOutputImpl implements IWarehouseOutput {
    private final WarehouseOutputRepository warehouseOutputRepository;
    private final UserRepository userRepository;
    private final WarehouseRepository warehouseRepository;
    private final CourierRepository courierRepository;
    private final IWarehouseOutputItem iWarehouseOutputItem;
    private final IAudit iAudit;
    private final WarehouseOutputRepositoryCustom warehouseOutputRepositoryCustom;
    private final WarehouseOutputItemRepository warehouseOutputItemRepository;
    private final IUtil iUtil;
    @Override
    public CompletableFuture<ResponseSuccess> save(RequestWarehouseOutput requestWarehouseOutput) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Warehouse warehouse;
            Courier courier;
            try{
                user = userRepository.findByUsernameAndStatusTrue(requestWarehouseOutput.getUsername().toUpperCase());
                warehouse = warehouseRepository.findByNameAndStatusTrue(requestWarehouseOutput.getWarehouse().toUpperCase());
                courier = courierRepository.findByNameAndStatusTrue(requestWarehouseOutput.getCourier().toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }

            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(warehouse==null){
                throw new BadRequestExceptions(Constants.ErrorWarehouse);
            }

            if(courier==null){
                throw new BadRequestExceptions(Constants.ErrorCourier);
            }

            try{
                Long orderNumber = warehouseOutputRepository.countByClientId(user.getClientId())+1L;
                WarehouseOutput warehouseOutput = warehouseOutputRepository.save(WarehouseOutput.builder()
                                .ref(requestWarehouseOutput.getRef())
                                .warehouse(warehouse)
                                .warehouseId(warehouse.getId())
                                .courier(courier)
                                .courierId(courier.getId())
                                .user(user)
                                .userId(user.getId())
                                .orderNumber(orderNumber)
                                .registrationDate(OffsetDateTime.now())
                                .updateDate(OffsetDateTime.now())
                                .client(user.getClient())
                                .clientId(user.getClientId())
                                .status(true)
                        .build());
                iAudit.save("ADD_WAREHOUSE_OUTPUT","SALIDA DE ALMACEN "+warehouseOutput.getOrderNumber()+" CREADA.",warehouseOutput.getOrderNumber().toString(),user.getUsername());
                for(RequestWarehouseOutputItem requestWarehouseOutputItem: requestWarehouseOutput.getRequestWarehouseOutputItemList()){
                    iWarehouseOutputItem.save(requestWarehouseOutputItem,warehouseOutput,user);
                }
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }

        });
    }
    @Override
    public CompletableFuture<ResponseDelete> close(String username, UUID warehouseOutputId) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            WarehouseOutput warehouseOutput;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                warehouseOutput = warehouseOutputRepository.findById(warehouseOutputId).orElse(null);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
            if(warehouseOutput==null){
                throw new BadRequestExceptions(Constants.ErrorWarehouseOutput);
            }
            try {
                warehouseOutput.setStatus(false);
                warehouseOutput.setUpdateDate(OffsetDateTime.now());
                warehouseOutput.setUser(user);
                warehouseOutput.setUserId(user.getId());
                warehouseOutputRepository.save(warehouseOutput);
                return ResponseDelete.builder()
                        .code(200)
                        .message(Constants.delete)
                        .build();
            }catch (RuntimeException e){
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> reactivate(String username, UUID warehouseOutputId) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            WarehouseOutput warehouseOutput;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                warehouseOutput = warehouseOutputRepository.findById(warehouseOutputId).orElse(null);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
            if(warehouseOutput==null){
                throw new BadRequestExceptions(Constants.ErrorWarehouseOutput);
            }
            try {
                warehouseOutput.setStatus(true);
                warehouseOutput.setUpdateDate(OffsetDateTime.now());
                warehouseOutput.setUser(user);
                warehouseOutput.setUserId(user.getId());
                warehouseOutputRepository.save(warehouseOutput);
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            }catch (RuntimeException e){
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<WarehouseOutputDTO>> list(String user, Long orderNumber, String ref, String courier, String warehouse, OffsetDateTime registrationStartDate, OffsetDateTime registrationEndDate, OffsetDateTime updateStartDate, OffsetDateTime updateEndDate, String sort, String sortColumn, Integer pageNumber, Integer pageSize, Boolean status) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<WarehouseOutput> warehouseOutputPage;
            UUID clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                warehouseOutputPage = warehouseOutputRepositoryCustom.searchForWarehouseOutput(
                        clientId,
                        orderNumber,
                        ref,
                        courier,
                        warehouse,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateEndDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        status);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (warehouseOutputPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<WarehouseOutputDTO> warehouseOutputDTOs = warehouseOutputPage.getContent().stream().map(warehouseOutput -> {
                List<WarehouseOutputItemDTO> warehouseOutputItemDTOList = warehouseOutputItemRepository.findByWarehouseOutputId(warehouseOutput.getId()).stream().map(warehouseOutputItem->WarehouseOutputItemDTO.builder()
                        .id(warehouseOutputItem.getId())
                        .courier(warehouseOutput.getCourier().getName())
                        .orderNumber(warehouseOutput.getOrderNumber())
                        .ref(warehouseOutput.getRef())
                        .warehouse(warehouseOutput.getWarehouse().getName())
                        .quantity(warehouseOutputItem.getQuantity())
                        .productSku(iUtil.buildProductSku(warehouseOutputItem.getProduct()))
                        .productId(warehouseOutputItem.getProductId())
                        .updateDate(warehouseOutputItem.getUpdateDate())
                        .registrationDate(warehouseOutputItem.getRegistrationDate())
                        .user(warehouseOutputItem.getUser().getUsername())
                        .build()).toList();
                return WarehouseOutputDTO.builder()
                        .id(warehouseOutput.getId())
                        .courier(warehouseOutput.getCourier().getName())
                        .orderNumber(warehouseOutput.getOrderNumber())
                        .ref(warehouseOutput.getRef())
                        .warehouse(warehouseOutput.getWarehouse().getName())
                        .registrationDate(warehouseOutput.getRegistrationDate())
                        .updateDate(warehouseOutput.getUpdateDate())
                        .status(warehouseOutput.getStatus())
                        .user(warehouseOutput.getUser().getUsername())
                        .warehouseOutputItemDTOList(warehouseOutputItemDTOList)
                        .build();
            }).toList();

            return new PageImpl<>(warehouseOutputDTOs, warehouseOutputPage.getPageable(), warehouseOutputPage.getTotalElements());
        });
    }
}
