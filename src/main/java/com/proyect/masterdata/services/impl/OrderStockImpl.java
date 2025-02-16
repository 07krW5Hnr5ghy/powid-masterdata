package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.OrderStockDTO;
import com.proyect.masterdata.dto.request.RequestOrderStockItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IOrderStock;
import com.proyect.masterdata.services.IOrderStockItem;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.stream.Collectors;

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
    private final ProductRepository productRepository;
    private final OrderItemRepository orderItemRepository;
    private final OrderStateRepository orderStateRepository;
    private final IAudit iAudit;
    @Override
    public ResponseSuccess save(UUID orderId, String warehouseName, List<RequestOrderStockItem> requestOrderStockItemList, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {

        User user;
        Warehouse warehouse;
        Ordering ordering;
        OrderState orderState;
        OrderStock orderStock;

        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            warehouse = warehouseRepository.findByNameAndStatusTrue(warehouseName.toUpperCase());
            ordering = orderingRepository.findById(orderId).orElse(null);
            orderState = orderStateRepository.findByNameAndStatusTrue("PREPARADO");
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }else{
            orderStock = orderStockRepository.findByOrderIdAndClientId(orderId,user.getClientId());
        }

        if(warehouse == null){
            throw new BadRequestExceptions(Constants.ErrorWarehouse);
        }

        if(ordering == null){
            throw new BadRequestExceptions(Constants.ErrorOrdering);
        }

        if(orderState == null){
            throw new BadRequestExceptions(Constants.ErrorOrderState);
        }

        if(orderStock != null){
            throw new BadRequestExceptions(Constants.ErrorOrderStockExists);
        }

        try{
            for(RequestOrderStockItem requestOrderStockItem : requestOrderStockItemList){
                if(requestOrderStockItem.getQuantity() < 1){
                    throw new BadRequestExceptions(Constants.ErrorOrderStockItemZero);
                }
                Boolean existsStock = iOrderStockItem.checkWarehouseItemStock(ordering.getId(),warehouse,requestOrderStockItem).get();
                if(!existsStock){
                    throw new BadRequestExceptions(Constants.ErrorOrderStockQuantity);
                }
            }
            Map<String,Integer> checkCount = requestOrderStockItemList.stream().collect(Collectors.groupingBy(RequestOrderStockItem::getProduct,Collectors.summingInt(RequestOrderStockItem::getQuantity)));
            checkCount.forEach((key,value)->{
                Product product = productRepository.findBySkuAndStatusTrue(key);
                OrderItem orderItem = orderItemRepository.findByOrderIdAndProductId(ordering.getId(),product.getId());
                if(value > orderItem.getQuantity()){
                    throw new BadRequestExceptions(Constants.ErrorOrderStockProductQuantity);
                }
            });
            OrderStock newOrderStock = orderStockRepository.save(OrderStock.builder()
                    .ordering(ordering)
                    .orderId(ordering.getId())
                    .status(true)
                    .warehouse(warehouse)
                    .warehouseId(warehouse.getId())
                    .registrationDate(OffsetDateTime.now())
                    .updateDate(OffsetDateTime.now())
                    .client(user.getClient())
                    .clientId(user.getClientId())
                    .user(user)
                            .userId(user.getId())
                    .build());
            for(RequestOrderStockItem requestOrderStockItem : requestOrderStockItemList){
                iOrderStockItem.save(newOrderStock.getOrderId(),requestOrderStockItem,user.getUsername());
            }
            ordering.setOrderState(orderState);
            ordering.setOrderStateId(orderState.getId());
            orderingRepository.save(ordering);
            iAudit.save("ADD_ORDER_STOCK","PREPARACION DE PEDIDO "+newOrderStock.getOrderId()+" CREADA.",newOrderStock.getOrderId().toString(),user.getUsername());
            return ResponseSuccess.builder()
                    .message(Constants.register)
                    .code(200)
                    .build();
        }catch (RuntimeException | ExecutionException | InterruptedException e){
            log.error(e.getMessage());
            e.printStackTrace();
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public CompletableFuture<ResponseSuccess> saveAsync(UUID orderId, String warehouseName, List<RequestOrderStockItem> requestOrderStockItemList, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Warehouse warehouse;
            Ordering ordering;
            OrderState orderState;
            OrderStock orderStock;

            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                warehouse = warehouseRepository.findByNameAndStatusTrue(warehouseName.toUpperCase());
                ordering = orderingRepository.findById(orderId).orElse(null);
                orderState = orderStateRepository.findByNameAndStatusTrue("PREPARADO");
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                orderStock = orderStockRepository.findByOrderIdAndClientId(orderId,user.getClientId());
            }

            if(warehouse == null){
                throw new BadRequestExceptions(Constants.ErrorWarehouse);
            }

            if(ordering == null){
                throw new BadRequestExceptions(Constants.ErrorOrdering);
            }

            if(orderState == null){
                throw new BadRequestExceptions(Constants.ErrorOrderState);
            }

            if(orderStock !=  null){
                throw new BadRequestExceptions(Constants.ErrorOrderStock);
            }

            try{
                for(RequestOrderStockItem requestOrderStockItem : requestOrderStockItemList){
                    if(requestOrderStockItem.getQuantity() < 1){
                        throw new BadRequestExceptions(Constants.ErrorOrderStockItemZero);
                    }
                    Boolean existsStock = iOrderStockItem.checkWarehouseItemStock(ordering.getId(),warehouse,requestOrderStockItem).get();
                    if(!existsStock){
                        throw new BadRequestExceptions(Constants.ErrorOrderStockQuantity);
                    }
                }
                Map<String,Integer> checkCount = requestOrderStockItemList.stream().collect(Collectors.groupingBy(RequestOrderStockItem::getProduct,Collectors.summingInt(RequestOrderStockItem::getQuantity)));
                checkCount.forEach((key,value)->{
                    Product product = productRepository.findBySkuAndStatusTrue(key);
                    OrderItem orderItem = orderItemRepository.findByOrderIdAndProductId(ordering.getId(),product.getId());
                    if(value > orderItem.getQuantity()){
                        throw new BadRequestExceptions(Constants.ErrorOrderStockProductQuantity);
                    }
                });
                OrderStock newOrderStock = orderStockRepository.save(OrderStock.builder()
                        .ordering(ordering)
                        .orderId(ordering.getId())
                        .status(true)
                        .warehouse(warehouse)
                        .warehouseId(warehouse.getId())
                        .registrationDate(OffsetDateTime.now())
                        .updateDate(OffsetDateTime.now())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .user(user)
                        .userId(user.getId())
                        .build());
                for(RequestOrderStockItem requestOrderStockItem : requestOrderStockItemList){
                    iOrderStockItem.save(newOrderStock.getOrderId(),requestOrderStockItem,user.getUsername());
                }
                ordering.setOrderState(orderState);
                ordering.setOrderStateId(orderState.getId());
                orderingRepository.save(ordering);
                iAudit.save("ADD_ORDER_STOCK","PREPARACION DE PEDIDO "+newOrderStock.getOrderId()+" CREADA.",newOrderStock.getOrderId().toString(),user.getUsername());
                return ResponseSuccess.builder()
                        .message(Constants.register)
                        .code(200)
                        .build();
            }catch (RuntimeException | ExecutionException | InterruptedException e){
                log.error(e.getMessage());
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<OrderStockDTO>> list(
            String user,
            UUID orderId,
            List<String> warehouses,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<OrderStock> pageOrderStock;
            List<UUID> warehouseIds;
            UUID clientId;

            if(warehouses != null && !warehouses.isEmpty()){
                warehouseIds = warehouseRepository.findByNameIn(
                        warehouses.stream().map(String::toUpperCase).toList()
                ).stream().map(Warehouse::getId).toList();
            }else{
                warehouseIds = new ArrayList<>();
            }

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                pageOrderStock = orderStockRepositoryCustom.searchForOrderStock(
                        clientId,
                        orderId,
                        warehouseIds,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        true);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if(pageOrderStock.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }

            List<OrderStockDTO> orderStockDTOS = pageOrderStock.getContent().stream().map(orderStock -> OrderStockDTO.builder()
                    .orderId(orderStock.getOrderId())
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
            UUID clientId;
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
                    .orderId(orderStock.getOrderId())
                    .warehouse(orderStock.getWarehouse().getName())
                    .registrationDate(orderStock.getRegistrationDate())
                    .build()
            ).toList();
        });
    }

    @Override
    public CompletableFuture<List<OrderStockDTO>> listOrderStockFalse(String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<OrderStock> orderStocks;
            UUID clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusFalse(user.toUpperCase()).getClientId();
                orderStocks = orderStockRepository.findAllByClientId(clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (orderStocks.isEmpty()){
                return Collections.emptyList();
            }

            return orderStocks.stream().map(orderStock -> OrderStockDTO.builder()
                    .orderId(orderStock.getOrderId())
                    .warehouse(orderStock.getWarehouse().getName())
                    .registrationDate(orderStock.getRegistrationDate())
                    .build()
            ).toList();
        });
    }

    @Override
    public CompletableFuture<List<OrderStockDTO>> listFilter(String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<OrderStock> orderStocks;
            UUID clientId;
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
                    .orderId(orderStock.getOrderId())
                    .warehouse(orderStock.getWarehouse().getName())
                    .registrationDate(orderStock.getRegistrationDate())
                    .build()
            ).toList();
        });
    }

    @Override
    public CompletableFuture<OrderStockDTO> listOrderStock(UUID id,String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            OrderStock orderStock;
            UUID clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                orderStock = orderStockRepository.findByOrderIdAndClientId(id,clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            return OrderStockDTO.builder()
                    .orderId(orderStock.getOrderId())
                    .warehouse(orderStock.getWarehouse().getName())
                    .registrationDate(orderStock.getRegistrationDate())
                    .build();
        });
    }
}
