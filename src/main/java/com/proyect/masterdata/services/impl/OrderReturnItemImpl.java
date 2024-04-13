package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.request.RequestOrderReturnItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IOrderReturnItem;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.util.Date;

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
    @Override
    public ResponseSuccess save(Long orderReturnId, Long orderId, RequestOrderReturnItem requestOrderReturnItem, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
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
            orderReturnType = orderReturnTypeRepository.findByNameAndStatusTrue(requestOrderReturnItem.getOrderReturnType());
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
        }else {
            orderReturnItem = orderReturnItemRepository.findBySupplierProductIdAndProductIdAndOrderId(supplierProduct.getId(), product.getId(),orderId);
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
                            .orderItem(orderItem)
                            .orderItemId(orderItem.getId())
                            .orderReturn(orderReturn)
                            .orderReturnId(orderReturn.getId())
                            .orderStockItem(orderStockItem)
                            .orderStockItemId(orderStockItem.getId())
                            .orderReturnType(orderReturnType)
                            .orderReturnTypeId(orderReturnType.getId())
                            .product(product)
                            .productId(product.getId())
                            .quantity(requestOrderReturnItem.getQuantity())
                            .supplierProduct(supplierProduct)
                            .supplierProductId(supplierProduct.getId())
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .updateDate(new Date(System.currentTimeMillis()))
                            .status(true)
                    .build());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }
}
