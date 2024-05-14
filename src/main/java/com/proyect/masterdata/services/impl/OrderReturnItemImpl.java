package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.OrderReturnItemDTO;
import com.proyect.masterdata.dto.request.RequestOrderReturnItem;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IGeneralStock;
import com.proyect.masterdata.services.IOrderReturnItem;
import com.proyect.masterdata.services.IWarehouseStock;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class OrderReturnItemImpl implements IOrderReturnItem {
    private final UserRepository userRepository;
    private final OrderReturnItemRepository orderReturnItemRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final OrderReturnTypeRepository orderReturnTypeRepository;
    private final ProductRepository productRepository;
    private final OrderingRepository orderingRepository;
    private final OrderItemRepository orderItemRepository;
    private final OrderReturnRepository orderReturnRepository;
    private final OrderStockRepository orderStockRepository;
    private final OrderStockItemRepository orderStockItemRepository;
    private final IGeneralStock iGeneralStock;
    private final IWarehouseStock iWarehouseStock;
    @Override
    public CompletableFuture<ResponseSuccess> save(Long orderReturnId, Long orderId, RequestOrderReturnItem requestOrderReturnItem, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            SupplierProduct supplierProduct;
            OrderReturnType orderReturnType;
            OrderReturnItem orderReturnItem;
            OrderReturn orderReturn;
            OrderItem orderItem;
            Product product;
            OrderStock orderStock;
            OrderStockItem orderStockItem;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(requestOrderReturnItem.getSupplierProductSerial().toUpperCase());
                orderReturnType = orderReturnTypeRepository.findByNameAndStatusTrue(requestOrderReturnItem.getOrderReturnType().toUpperCase());
                product = productRepository.findBySkuAndStatusTrue(requestOrderReturnItem.getProductSku().toUpperCase());
                orderReturn = orderReturnRepository.findByOrderId(orderReturnId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(supplierProduct == null){
                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
            }
            if(product == null){
                throw new BadRequestExceptions(Constants.ErrorProduct);
            }
            if(orderReturnType==null){
                throw new BadRequestExceptions(Constants.ErrorOrderReturnType);
            }else{
                orderItem = orderItemRepository.findByOrderIdAndProductId(orderId, product.getId());
            }
            if(orderItem == null){
                throw new BadRequestExceptions(Constants.ErrorOrderItem);
            }else{
                orderStock = orderStockRepository.findByOrderId(orderItem.getOrderId());
            }
            if(orderStock == null){
                throw new BadRequestExceptions(Constants.ErrorOrderStock);
            }else{
                orderStockItem = orderStockItemRepository.findByOrderStockIdAndSupplierProductIdAndStatusTrue(orderStock.getId(),supplierProduct.getId());
            }
            if(orderStockItem == null){
                throw new BadRequestExceptions(Constants.ErrorOrderStockItem);
            }else{
                orderReturnItem = orderReturnItemRepository.findByClientIdAndOrderIdAndSupplierProductIdAndStatusTrue(user.getClientId(),orderId,supplierProduct.getId());
            }
            if(orderReturnItem != null){
                throw new BadRequestExceptions(Constants.ErrorOrderItemExists);
            }
            if(orderReturn == null){
                throw new BadRequestExceptions(Constants.ErrorOrderReturn);
            }
            if(requestOrderReturnItem.getQuantity() > orderStockItem.getQuantity()){
                throw new BadRequestExceptions(Constants.ErrorOrderReturnItemQuantity);
            }
            try {
                orderReturnItemRepository.save(OrderReturnItem.builder()
                        .orderReturn(orderReturn)
                        .orderReturnId(orderReturn.getId())
                        .orderReturnType(orderReturnType)
                        .orderReturnTypeId(orderReturnType.getId())
                        .product(product)
                        .orderId(orderStockItem.getOrderId())
                        .productId(product.getId())
                        .quantity(requestOrderReturnItem.getQuantity())
                        .supplierProduct(supplierProduct)
                        .supplierProductId(supplierProduct.getId())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .updateDate(new Date(System.currentTimeMillis()))
                        .tokenUser(user.getUsername())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .status(true)
                        .build());
                iGeneralStock.in(supplierProduct.getSerial(), requestOrderReturnItem.getQuantity(), user.getUsername());
                iWarehouseStock.in(orderStock.getWarehouse(),supplierProduct, requestOrderReturnItem.getQuantity(), user);
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
    public CompletableFuture<ResponseDelete> delete(Long orderId, String supplierProductSerial, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            OrderReturn orderReturn;
            SupplierProduct supplierProduct;
            OrderReturnItem orderReturnItem;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                orderReturn = orderReturnRepository.findByOrderId(orderId);
                supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(supplierProductSerial.toUpperCase());
            }catch (RuntimeException e){
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(supplierProduct == null){
                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
            }
            if(orderReturn==null){
                throw new BadRequestExceptions(Constants.ErrorOrderReturn);
            }else{
                orderReturnItem = orderReturnItemRepository.findBySupplierProductIdAndOrderId(supplierProduct.getId(), orderReturn.getOrderId());
            }
            try{
                orderReturnItem.setStatus(false);
                orderReturnItem.setUpdateDate(new Date(System.currentTimeMillis()));
                orderReturnItemRepository.save(orderReturnItem);
                return ResponseDelete.builder()
                        .message(Constants.delete)
                        .code(200)
                        .build();
            }catch (RuntimeException e){
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<List<OrderReturnItemDTO>> list(String user, Long orderId) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Long clientId;
            List<OrderReturnItem> orderReturnItemList;
            try{
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                if(orderId!=null){
                    orderReturnItemList = orderReturnItemRepository.findAllByClientIdAndOrderIdAndStatusTrue(clientId,orderId);
                }else {
                    orderReturnItemList = orderReturnItemRepository.findAllByClientIdAndStatusTrue(clientId);
                }
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(orderReturnItemList.isEmpty()){
                return Collections.emptyList();
            }
            return orderReturnItemList.stream().map(orderReturnItem -> OrderReturnItemDTO.builder()
                    .orderId(orderReturnItem.getOrderId())
                    .productSku(orderReturnItem.getProduct().getSku())
                    .supplierProduct(orderReturnItem.getSupplierProduct().getSerial())
                    .returnType(orderReturnItem.getOrderReturnType().getName())
                    .registrationDate(new Date(System.currentTimeMillis()))
                    .updateDate(new Date(System.currentTimeMillis()))
                    .quantity(orderReturnItem.getQuantity())
                    .warehouse(orderReturnItem.getOrderReturn().getOrderStock().getWarehouse().getName())
                    .build()).toList();
        });
    }
}
