package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.OrderStock;
import com.proyect.masterdata.domain.Ordering;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.Warehouse;
import com.proyect.masterdata.dto.OrderStockDTO;
import com.proyect.masterdata.dto.request.RequestOrderStockItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IOrderStock;
import com.proyect.masterdata.services.IOrderStockItem;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class OrderStockImpl implements IOrderStock {
    private final UserRepository userRepository;
    private final WarehouseRepository warehouseRepository;
    private final OrderStockRepository orderStockRepository;
    private final OrderingRepository orderingRepository;
    private final IOrderStockItem iOrderStockItem;
    private final OrderStockRepositoryCustom orderStockRepositoryCustom;
    @Override
    public ResponseSuccess save(Long orderId, String warehouse, List<RequestOrderStockItem> requestOrderStockItemList, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {

        User user;
        Warehouse warehouseData;
        Ordering ordering;

        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            warehouseData = warehouseRepository.findByNameAndStatusTrue(warehouse.toUpperCase());
            ordering = orderingRepository.findById(orderId).orElse(null);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(warehouseData == null){
            throw new BadRequestExceptions(Constants.ErrorWarehouse);
        }

        if(ordering == null){
            throw new BadRequestExceptions(Constants.ErrorOrdering);
        }

        try{
            for(RequestOrderStockItem requestOrderStockItem : requestOrderStockItemList){
                Boolean existsStock = iOrderStockItem.checkWarehouseItemStock(ordering.getId(),warehouseData,requestOrderStockItem);
                if(!existsStock){
                    throw new BadRequestExceptions(Constants.ErrorOrderStockQuantity);
                }
            }
            OrderStock orderStock = orderStockRepository.save(OrderStock.builder()
                    .ordering(ordering)
                    .orderId(ordering.getId())
                    .status(true)
                    .warehouse(warehouseData)
                    .warehouseId(warehouseData.getId())
                    .registrationDate(new Date(System.currentTimeMillis()))
                    .updateDate(new Date(System.currentTimeMillis()))
                    .client(user.getClient())
                    .clientId(user.getClientId())
                    .tokenUser(user.getUsername())
                    .build());
            for(RequestOrderStockItem requestOrderStockItem : requestOrderStockItemList){
                iOrderStockItem.save(orderStock.getOrderId(),requestOrderStockItem,user.getUsername());
            }
            return ResponseSuccess.builder()
                    .message(Constants.register)
                    .code(200)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            e.printStackTrace();
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public CompletableFuture<ResponseSuccess> saveAsync(Long orderId, String warehouse, List<RequestOrderStockItem> requestOrderStockItemList, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Warehouse warehouseData;
            Ordering ordering;

            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                warehouseData = warehouseRepository.findByNameAndStatusTrue(warehouse.toUpperCase());
                ordering = orderingRepository.findById(orderId).orElse(null);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(warehouseData == null){
                throw new BadRequestExceptions(Constants.ErrorWarehouse);
            }

            if(ordering == null){
                throw new BadRequestExceptions(Constants.ErrorOrdering);
            }

            try{
                for(RequestOrderStockItem requestOrderStockItem : requestOrderStockItemList){
                    Boolean existsStock = iOrderStockItem.checkWarehouseItemStock(ordering.getId(),warehouseData,requestOrderStockItem);
                    if(!existsStock){
                        throw new BadRequestExceptions(Constants.ErrorOrderStockQuantity);
                    }
                }
                OrderStock orderStock = orderStockRepository.save(OrderStock.builder()
                        .ordering(ordering)
                        .orderId(ordering.getId())
                        .status(true)
                        .warehouse(warehouseData)
                        .warehouseId(warehouseData.getId())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .updateDate(new Date(System.currentTimeMillis()))
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .tokenUser(user.getUsername())
                        .build());
                for(RequestOrderStockItem requestOrderStockItem : requestOrderStockItemList){
                    iOrderStockItem.save(orderStock.getOrderId(),requestOrderStockItem,user.getUsername());
                }
                return ResponseSuccess.builder()
                        .message(Constants.register)
                        .code(200)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<OrderStockDTO>> list(String warehouse, Long orderId, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<OrderStock> pageOrderStock;
            Long warehouseId;
            Long clientId;

            if(warehouse != null){
                warehouseId = warehouseRepository.findByName(warehouse.toUpperCase()).getId();
            }else{
                warehouseId = null;
            }

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                pageOrderStock = orderStockRepositoryCustom.searchForOrderStock(warehouseId,orderId,clientId,sort,sortColumn,pageNumber,pageSize,true);
                System.out.println(pageOrderStock.getContent());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if(pageOrderStock.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }

            List<OrderStockDTO> orderStockDTOS = pageOrderStock.getContent().stream().map(orderStock -> OrderStockDTO.builder()
                    .orderId(orderStock.getId())
                    .warehouse(orderStock.getWarehouse().getName())
                    .registrationDate(orderStock.getRegistrationDate())
                    .build()
            ).toList();

            return new PageImpl<>(orderStockDTOS,pageOrderStock.getPageable(),pageOrderStock.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<OrderStockDTO>> listOrderStock(String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<OrderStock> orderStocks;
            Long clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                orderStocks = orderStockRepository.findAllByClientId(clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (orderStocks.isEmpty()){
                return Collections.emptyList();
            }

            return orderStocks.stream().map(orderStock -> OrderStockDTO.builder()
                    .orderId(orderStock.getId())
                    .warehouse(orderStock.getWarehouse().getName())
                    .registrationDate(orderStock.getRegistrationDate())
                    .build()
            ).toList();
        });
    }

    @Override
    public CompletableFuture<List<OrderStockDTO>> listOrderStockFalse(String user) throws BadRequestExceptions, InternalErrorExceptions {
        return null;
    }
}
