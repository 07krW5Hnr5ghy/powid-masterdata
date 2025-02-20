package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.OrderReturnDTO;
import com.proyect.masterdata.dto.request.RequestOrderReturnItem;
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
import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Log4j2
public class OrderReturnImpl implements IOrderReturn {
    private final OrderReturnRepository orderReturnRepository;
    private final OrderStockRepository orderStockRepository;
    private final UserRepository userRepository;
    private final IOrderReturnItem iOrderReturnItem;
    private final SupplierProductRepository supplierProductRepository;
    private final OrderStockItemRepository orderStockItemRepository;
    private final IStockTransaction iStockTransaction;
    private final ProductRepository productRepository;
    private final OrderReturnTypeRepository orderReturnTypeRepository;
    private final OrderReturnRepositoryCustom orderReturnRepositoryCustom;
    private final IAudit iAudit;
    private final WarehouseRepository warehouseRepository;
    private final IUtil iUtil;
    @Override
    public ResponseSuccess save(UUID orderId, List<RequestOrderReturnItem> requestOrderReturnItemList, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        User user;
        OrderReturn orderReturn;
        OrderStock orderStock;
        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            orderReturn = orderReturnRepository.findByOrderId(orderId);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }else{
            orderStock = orderStockRepository.findByOrderIdAndClientId(orderId,user.getClientId());
        }
        if(orderReturn != null){
            throw new BadRequestExceptions(Constants.ErrorOrderReturnExists);
        }
        if(orderStock == null){
            throw new BadRequestExceptions(Constants.ErrorOrderStock);
        }
        try {
            requestOrderReturnItemList.forEach(requestOrderReturnItem -> {
                Product product = productRepository.findByIdAndStatusTrue(requestOrderReturnItem.getProductId());
                if(product == null){
                    throw new BadRequestExceptions(Constants.ErrorProduct);
                }
                SupplierProduct supplierProduct = supplierProductRepository.findBySupplierIdAndProductIdAndStatusTrue(requestOrderReturnItem.getSupplierId(),requestOrderReturnItem.getProductId());
                if(supplierProduct==null){
                    throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
                }
                if(requestOrderReturnItem.getQuantity() < 1){
                    throw new BadRequestExceptions(Constants.ErrorOrderReturnItemZero);
                }
                OrderReturnType orderReturnType = orderReturnTypeRepository.findByNameAndStatusTrue(requestOrderReturnItem.getOrderReturnType().toUpperCase());
                if(orderReturnType==null){
                    throw new BadRequestExceptions(Constants.ErrorOrderReturnType);
                }
            });
            Map<UUID,Integer> checkCount = requestOrderReturnItemList.stream().collect(
                    Collectors.groupingBy(
                            item -> supplierProductRepository.findBySupplierIdAndProductId(item.getSupplierId(),item.getProductId()).getId(),
                            Collectors.summingInt(RequestOrderReturnItem::getQuantity)
                    )
            );
            checkCount.forEach((key,value)->{
                SupplierProduct supplierProduct = supplierProductRepository.findByIdAndStatusTrue(key);
                OrderStockItem orderStockItem = orderStockItemRepository.findByOrderStockIdAndSupplierProductIdAndStatusTrue(orderStock.getId(),supplierProduct.getId());
                if(value > orderStockItem.getQuantity()){
                    throw new BadRequestExceptions(Constants.ErrorOrderStockProductQuantity);
                }
            });
            OrderReturn newOrderReturn = orderReturnRepository.save(OrderReturn.builder()
                    .order(orderStock.getOrdering())
                    .orderId(orderStock.getOrderId())
                    .orderStock(orderStock)
                    .orderStockId(orderStock.getId())
                    .user(user)
                            .userId(user.getId())
                    .client(user.getClient())
                    .clientId(user.getClientId())
                    .status(true)
                    .build());
            List<RequestStockTransactionItem> requestStockTransactionItemList = new ArrayList<>();
            for(RequestOrderReturnItem requestOrderReturnItem : requestOrderReturnItemList){
                RequestStockTransactionItem requestStockTransactionItem = RequestStockTransactionItem.builder().build();
                iOrderReturnItem.save(orderStock.getOrderId(),requestOrderReturnItem,tokenUser);
                requestStockTransactionItem.setQuantity(requestOrderReturnItem.getQuantity());
                requestStockTransactionItem.setSupplierProductId(supplierProductRepository.findBySupplierIdAndProductId(requestOrderReturnItem.getSupplierId(),requestOrderReturnItem.getProductId()).getId());
                requestStockTransactionItemList.add(requestStockTransactionItem);
            }
            iStockTransaction.save("OR"+orderStock.getOrdering().getId(),orderStock.getWarehouse(),requestStockTransactionItemList,"DEVOLUCION-COMPRADOR",user);
            iAudit.save("ADD_ORDER_RETURN","DEVOLUCION DE PEDIDO "+newOrderReturn.getOrderId()+" CREADA.",newOrderReturn.getOrderId().toString(),user.getUsername());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            e.printStackTrace();
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public CompletableFuture<ResponseSuccess> saveAsync(UUID orderId, List<RequestOrderReturnItem> requestOrderReturnItemList, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            OrderReturn orderReturn;
            OrderStock orderStock;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                orderReturn = orderReturnRepository.findByOrderId(orderId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                orderStock = orderStockRepository.findByOrderIdAndClientId(orderId,user.getClientId());
            }
            if(orderReturn != null){
                throw new BadRequestExceptions(Constants.ErrorOrderReturnExists);
            }
            if(orderStock == null){
                throw new BadRequestExceptions(Constants.ErrorOrderStock);
            }
            try {
                requestOrderReturnItemList.forEach(requestOrderReturnItem -> {
                    Product product = productRepository.findByIdAndStatusTrue(requestOrderReturnItem.getProductId());
                    if(product == null){
                        throw new BadRequestExceptions(Constants.ErrorProduct);
                    }
                    SupplierProduct supplierProduct = supplierProductRepository.findBySupplierIdAndProductIdAndStatusTrue(requestOrderReturnItem.getSupplierId(),requestOrderReturnItem.getProductId());
                    if(supplierProduct==null){
                        throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
                    }
                    if(requestOrderReturnItem.getQuantity() < 1){
                        throw new BadRequestExceptions(Constants.ErrorOrderReturnItemZero);
                    }
                    OrderReturnType orderReturnType = orderReturnTypeRepository.findByNameAndStatusTrue(requestOrderReturnItem.getOrderReturnType().toUpperCase());
                    if(orderReturnType==null){
                        throw new BadRequestExceptions(Constants.ErrorOrderReturnType);
                    }
                });
                Map<UUID,Integer> checkCount = requestOrderReturnItemList.stream().collect(
                        Collectors.groupingBy(
                                item -> supplierProductRepository.findBySupplierIdAndProductId(item.getSupplierId(),item.getProductId()).getId(),
                                Collectors.summingInt(RequestOrderReturnItem::getQuantity)
                        )
                );
                checkCount.forEach((key,value)->{
                    SupplierProduct supplierProduct = supplierProductRepository.findByIdAndStatusTrue(key);
                    OrderStockItem orderStockItem = orderStockItemRepository.findByOrderStockIdAndSupplierProductIdAndStatusTrue(orderStock.getId(),supplierProduct.getId());
                    if(value > orderStockItem.getQuantity()){
                        throw new BadRequestExceptions(Constants.ErrorOrderStockProductQuantity);
                    }
                });
                OrderReturn newOrderReturn = orderReturnRepository.save(OrderReturn.builder()
                        .order(orderStock.getOrdering())
                        .orderId(orderStock.getOrderId())
                        .orderStock(orderStock)
                        .orderStockId(orderStock.getId())
                        .user(user)
                                .userId(user.getId())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .status(true)
                        .build());
                List<RequestStockTransactionItem> requestStockTransactionItemList = new ArrayList<>();
                for(RequestOrderReturnItem requestOrderReturnItem : requestOrderReturnItemList){
                    RequestStockTransactionItem requestStockTransactionItem = RequestStockTransactionItem.builder().build();
                    iOrderReturnItem.save(orderStock.getOrderId(),requestOrderReturnItem,tokenUser);
                    requestStockTransactionItem.setQuantity(requestOrderReturnItem.getQuantity());
                    requestStockTransactionItem.setSupplierProductId(supplierProductRepository.findBySupplierIdAndProductId(requestOrderReturnItem.getSupplierId(),requestOrderReturnItem.getProductId()).getId());
                    requestStockTransactionItemList.add(requestStockTransactionItem);
                }
                iStockTransaction.save("OR"+orderStock.getOrdering().getId(),orderStock.getWarehouse(),requestStockTransactionItemList,"DEVOLUCION-COMPRADOR",user);
                iAudit.save("ADD_ORDER_RETURN","DEVOLUCION DE PEDIDO "+newOrderReturn.getOrderId()+" CREADA.",newOrderReturn.getOrderId().toString(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<List<OrderReturnDTO>> list(String user) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<OrderReturn> orderReturns;
            UUID clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                orderReturns = orderReturnRepository.findAllByClientIdAndStatusTrue(clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(orderReturns.isEmpty()){
                return Collections.emptyList();
            }
            return orderReturns.stream().map(orderReturn -> OrderReturnDTO.builder()
                    .registrationDate(orderReturn.getRegistrationDate())
                    .updateDate(orderReturn.getUpdateDate())
                    .orderId(orderReturn.getOrderId())
                    .warehouse(orderReturn.getOrderStock().getWarehouse().getName())
                    .build()).toList();
        });
    }

    @Override
    public CompletableFuture<Page<OrderReturnDTO>> listPagination(
            String user,
            List<UUID> orders,
            List<String> warehouses,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<OrderReturn> orderReturnPage;
            List<UUID> orderIds;
            List<UUID> warehouseIds;
            UUID clientId;
            if(orders != null && !orders.isEmpty()){
                orderIds = orders;
            }else{
                orderIds = new ArrayList<>();
            }
            if(warehouses != null && !warehouses.isEmpty()){
                warehouseIds = warehouseRepository.findByNameIn(
                        warehouses.stream().map(String::toUpperCase).toList()
                ).stream().map(Warehouse::getId).toList();
            }else{
                warehouseIds = new ArrayList<>();
            }
            try{
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                orderReturnPage = orderReturnRepositoryCustom.searchForOrderReturn(
                        clientId,
                        orderIds,
                        warehouseIds,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateEndDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        true
                );
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if(orderReturnPage.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }

            List<OrderReturnDTO> orderReturnDTOS = orderReturnPage.getContent().stream().map(orderReturn -> OrderReturnDTO.builder()
                    .orderId(orderReturn.getOrderId())
                    .warehouse(orderReturn.getOrderStock().getWarehouse().getName())
                    .registrationDate(orderReturn.getRegistrationDate())
                    .updateDate(orderReturn.getUpdateDate())
                    .build()).toList();

            return new PageImpl<>(orderReturnDTOS,orderReturnPage.getPageable(),orderReturnPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<Page<OrderReturnDTO>> listFalse(
            String user,
            List<UUID> orders,
            List<String> warehouses,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<OrderReturn> orderReturnPage;
            List<UUID> orderIds;
            List<UUID> warehouseIds;
            UUID clientId;
            if(orders != null && !orders.isEmpty()){
                orderIds = orders;
            }else{
                orderIds = new ArrayList<>();
            }
            if(warehouses != null && !warehouses.isEmpty()){
                warehouseIds = warehouseRepository.findByNameIn(
                        warehouses.stream().map(String::toUpperCase).toList()
                ).stream().map(Warehouse::getId).toList();
            }else{
                warehouseIds = new ArrayList<>();
            }
            try{
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                orderReturnPage = orderReturnRepositoryCustom.searchForOrderReturn(
                        clientId,
                        orderIds,
                        warehouseIds,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateEndDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        false
                );
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if(orderReturnPage.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }

            List<OrderReturnDTO> orderReturnDTOS = orderReturnPage.getContent().stream().map(orderReturn -> OrderReturnDTO.builder()
                    .orderId(orderReturn.getOrderId())
                    .warehouse(orderReturn.getOrderStock().getWarehouse().getName())
                    .registrationDate(orderReturn.getRegistrationDate())
                    .updateDate(orderReturn.getUpdateDate())
                    .build()).toList();

            return new PageImpl<>(orderReturnDTOS,orderReturnPage.getPageable(),orderReturnPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<OrderReturnDTO>> listFilter(String user) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<OrderReturn> orderReturns;
            UUID clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                orderReturns = orderReturnRepository.findAllByClientId(clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(orderReturns.isEmpty()){
                return Collections.emptyList();
            }
            return orderReturns.stream().map(orderReturn -> OrderReturnDTO.builder()
                    .registrationDate(orderReturn.getRegistrationDate())
                    .updateDate(orderReturn.getUpdateDate())
                    .orderId(orderReturn.getOrderId())
                    .warehouse(orderReturn.getOrderStock().getWarehouse().getName())
                    .build()).toList();
        });
    }

    @Override
    public OrderReturn getOrderReturnItemByOrderIdAndClientId(UUID orderId, UUID clientId) {
        return orderReturnRepository.findByOrderIdAndClientId(orderId,clientId);
    }
}
