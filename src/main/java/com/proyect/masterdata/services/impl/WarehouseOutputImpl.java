package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Courier;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.Warehouse;
import com.proyect.masterdata.domain.WarehouseOutput;
import com.proyect.masterdata.dto.request.RequestWarehouseOutput;
import com.proyect.masterdata.dto.request.RequestWarehouseOutputItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.CourierRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.repository.WarehouseOutputRepository;
import com.proyect.masterdata.repository.WarehouseRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IWarehouseOutput;
import com.proyect.masterdata.services.IWarehouseOutputItem;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
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
}
