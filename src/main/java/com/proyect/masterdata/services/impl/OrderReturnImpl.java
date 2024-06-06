package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.OrderReturnDTO;
import com.proyect.masterdata.dto.request.RequestOrderReturnItem;
import com.proyect.masterdata.dto.request.RequestStockTransactionItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IOrderReturn;
import com.proyect.masterdata.services.IOrderReturnItem;
import com.proyect.masterdata.services.IStockTransaction;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
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
    private final IAudit iAudit;
    @Override
    public ResponseSuccess save(Long orderId, List<RequestOrderReturnItem> requestOrderReturnItemList, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        User user;
        OrderReturn orderReturn;
        OrderStock orderStock;
        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            orderReturn = orderReturnRepository.findByOrderId(orderId);
            orderStock = orderStockRepository.findByOrderId(orderId);
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }
        if(orderReturn != null){
            throw new BadRequestExceptions(Constants.ErrorOrderReturnExists);
        }
        if(orderStock == null){
            throw new BadRequestExceptions(Constants.ErrorOrderStock);
        }
        try {
            requestOrderReturnItemList.forEach(requestOrderReturnItem -> {
                Product product = productRepository.findBySkuAndStatusTrue(requestOrderReturnItem.getProductSku().toUpperCase());
                if(product == null){
                    throw new BadRequestExceptions(Constants.ErrorProduct);
                }
                SupplierProduct supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(requestOrderReturnItem.getSupplierProductSerial().toUpperCase());
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
            Map<String,Integer> checkCount = requestOrderReturnItemList.stream().collect(
                    Collectors.groupingBy(
                            RequestOrderReturnItem::getSupplierProductSerial,
                            Collectors.summingInt(RequestOrderReturnItem::getQuantity)
                    )
            );
            checkCount.forEach((key,value)->{
                SupplierProduct supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(key);
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
                    .tokenUser(user.getUsername())
                    .client(user.getClient())
                    .clientId(user.getClientId())
                    .status(true)
                    .build());
            List<RequestStockTransactionItem> requestStockTransactionItemList = new ArrayList<>();
            for(RequestOrderReturnItem requestOrderReturnItem : requestOrderReturnItemList){
                RequestStockTransactionItem requestStockTransactionItem = RequestStockTransactionItem.builder().build();
                iOrderReturnItem.save(orderStock.getOrderId(),requestOrderReturnItem,tokenUser);
                requestStockTransactionItem.setQuantity(requestOrderReturnItem.getQuantity());
                requestStockTransactionItem.setSupplierProductSerial(requestOrderReturnItem.getSupplierProductSerial());
                requestStockTransactionItemList.add(requestStockTransactionItem);
            }
            iStockTransaction.save("OR"+orderStock.getOrdering().getId(),orderStock.getWarehouse(),requestStockTransactionItemList,"DEVOLUCION-COMPRADOR",user);
            iAudit.save("ADD_ORDER_RETURN","ADD ORDER RETURN "+newOrderReturn.getOrderId()+".",user.getUsername());
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
    public CompletableFuture<ResponseSuccess> saveAsync(Long orderId, List<RequestOrderReturnItem> requestOrderReturnItemList, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            OrderReturn orderReturn;
            OrderStock orderStock;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                orderReturn = orderReturnRepository.findByOrderId(orderId);
                orderStock = orderStockRepository.findByOrderId(orderId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(orderReturn != null){
                throw new BadRequestExceptions(Constants.ErrorOrderReturnExists);
            }
            if(orderStock == null){
                throw new BadRequestExceptions(Constants.ErrorOrderStock);
            }
            try {
                requestOrderReturnItemList.forEach(requestOrderReturnItem -> {
                    Product product = productRepository.findBySkuAndStatusTrue(requestOrderReturnItem.getProductSku().toUpperCase());
                    if(product == null){
                        throw new BadRequestExceptions(Constants.ErrorProduct);
                    }
                    SupplierProduct supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(requestOrderReturnItem.getSupplierProductSerial().toUpperCase());
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
                Map<String,Integer> checkCount = requestOrderReturnItemList.stream().collect(
                        Collectors.groupingBy(
                                RequestOrderReturnItem::getSupplierProductSerial,
                                Collectors.summingInt(RequestOrderReturnItem::getQuantity)
                        )
                );
                checkCount.forEach((key,value)->{
                    SupplierProduct supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(key);
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
                        .tokenUser(user.getUsername())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .status(true)
                        .build());
                List<RequestStockTransactionItem> requestStockTransactionItemList = new ArrayList<>();
                for(RequestOrderReturnItem requestOrderReturnItem : requestOrderReturnItemList){
                    RequestStockTransactionItem requestStockTransactionItem = RequestStockTransactionItem.builder().build();
                    iOrderReturnItem.save(orderStock.getOrderId(),requestOrderReturnItem,tokenUser);
                    requestStockTransactionItem.setQuantity(requestOrderReturnItem.getQuantity());
                    requestStockTransactionItem.setSupplierProductSerial(requestOrderReturnItem.getSupplierProductSerial());
                    requestStockTransactionItemList.add(requestStockTransactionItem);
                }
                iStockTransaction.save("OR"+orderStock.getOrdering().getId(),orderStock.getWarehouse(),requestStockTransactionItemList,"DEVOLUCION-COMPRADOR",user);
                iAudit.save("ADD_ORDER_RETURN","ADD ORDER RETURN "+newOrderReturn.getOrderId()+".",user.getUsername());
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
            Long clientId;
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
}
