package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.CheckStockItemDTO;
import com.proyect.masterdata.dto.OrderItemDTO;
import com.proyect.masterdata.dto.request.RequestOrderItem;
import com.proyect.masterdata.dto.response.ResponseCheckStockItem;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IOrderItem;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class OrderItemImpl implements IOrderItem {

    private final UserRepository userRepository;
    private final OrderItemRepository orderItemRepository;
    private final ProductRepository productRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final WarehouseStockRepository warehouseStockRepository;
    private final OrderingRepository orderingRepository;
    private final SaleRepository saleRepository;
    private final ProductPriceRepository productPriceRepository;
    private final ProductPictureRepository productPictureRepository;
    private final OrderItemRepositoryCustom orderItemRepositoryCustom;
    private final IAudit iAudit;
    @Override
    public ResponseSuccess save(Ordering ordering, RequestOrderItem requestOrderItem, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        Product product;

        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            product = productRepository.findBySkuAndStatusTrue(requestOrderItem.getProductSku());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (product == null){
            throw new BadRequestExceptions(Constants.ErrorProduct);
        }

        if(requestOrderItem.getQuantity()<1){
            throw new BadRequestExceptions(Constants.ErrorOrderItemZero);
        }

        try{
            OrderItem newOrderItem = orderItemRepository.save(OrderItem.builder()
                            .discount(requestOrderItem.getDiscount())
                            .ordering(ordering)
                            .orderId(ordering.getId())
                            .quantity(requestOrderItem.getQuantity())
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .product(product)
                            .productId(product.getId())
                            .observations(requestOrderItem.getObservations().toUpperCase())
                            .status(true)
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .updateDate(new Date(System.currentTimeMillis()))
                            .tokenUser(user.getUsername())
                    .build());
            iAudit.save("ADD_ORDER_ITEM","ADD ORDER ITEM "+newOrderItem.getProduct().getSku()+" FOR ORDER "+newOrderItem.getOrderId()+" WITH "+newOrderItem.getQuantity()+" UNITS.",user.getUsername());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public CompletableFuture<ResponseSuccess> saveAsync(Ordering ordering, RequestOrderItem requestOrderItem, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Product product;

            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                product = productRepository.findBySkuAndStatusTrue(requestOrderItem.getProductSku());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (product == null){
                throw new BadRequestExceptions(Constants.ErrorProduct);
            }

            if(requestOrderItem.getQuantity()<1){
                throw new BadRequestExceptions(Constants.ErrorOrderItemZero);
            }

            try{
                OrderItem newOrderItem = orderItemRepository.save(OrderItem.builder()
                        .discount(requestOrderItem.getDiscount())
                        .ordering(ordering)
                        .orderId(ordering.getId())
                        .quantity(requestOrderItem.getQuantity())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .product(product)
                        .productId(product.getId())
                        .observations(requestOrderItem.getObservations().toUpperCase())
                        .status(true)
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .updateDate(new Date(System.currentTimeMillis()))
                        .tokenUser(user.getUsername())
                        .build());
                iAudit.save("ADD_ORDER_ITEM","ADD ORDER ITEM "+newOrderItem.getProduct().getSku()+" FOR ORDER "+newOrderItem.getOrderId()+" WITH "+newOrderItem.getQuantity()+" UNITS.",user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseCheckStockItem> checkStock(String productSku, Integer quantity, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Product product;
            List<SupplierProduct> supplierProductList;
            List<CheckStockItemDTO> checkStockItemDTOList = new ArrayList<>();

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                product = productRepository.findBySkuAndStatusTrue(productSku.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(product == null){
                throw new BadRequestExceptions(Constants.ErrorProduct);
            }else {
                supplierProductList = supplierProductRepository.findAllByProductIdAndStatusTrue(product.getId());
            }

            if(supplierProductList.isEmpty()){
                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
            }

            try{

                Integer stockUnits = 0;

                for(SupplierProduct supplierProduct : supplierProductList){
                    List<WarehouseStock> warehouseStockList = warehouseStockRepository.findAllBySupplierProductId(supplierProduct.getId());
                    for(WarehouseStock warehouseStock : warehouseStockList){
                        stockUnits += warehouseStock.getQuantity();
                        checkStockItemDTOList.add(CheckStockItemDTO.builder()
                                .stockQuantity(warehouseStock.getQuantity())
                                .warehouse(warehouseStock.getWarehouse().getName())
                                .build());
                    }
                }

                if(stockUnits >= quantity){
                    return ResponseCheckStockItem.builder()
                            .itemStockList(checkStockItemDTOList)
                            .pendingStock(false)
                            .pendingQuantity(0)
                            .build();
                }else {
                    return ResponseCheckStockItem.builder()
                            .pendingQuantity(quantity-stockUnits)
                            .pendingStock(true)
                            .build();
                }
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> delete(Long orderId, String productSku, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Ordering ordering;
            OrderItem orderItem;
            Product product;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                ordering = orderingRepository.findById(orderId).orElse(null);
                product = productRepository.findBySku(productSku.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(ordering == null){
                throw new BadRequestExceptions(Constants.ErrorOrdering);
            }

            if(product == null){
                throw new InternalErrorExceptions(Constants.ErrorProduct);
            }else {
                orderItem = orderItemRepository.findByOrderIdAndProductId(ordering.getId(),product.getId());
            }

            if(orderItem == null){
                throw new BadRequestExceptions(Constants.ErrorOrderItem);
            }

            try{
                Sale sale = saleRepository.findByOrderId(ordering.getId());
                orderItem.setStatus(false);
                orderItem.setUpdateDate(new Date(System.currentTimeMillis()));
                orderItem.setTokenUser(user.getUsername());
                orderItemRepository.save(orderItem);
                List<OrderItem> orderItemList = orderItemRepository.findAllByOrderIdAndStatusTrue(ordering.getId());
                double newSaleAmount = 0.00;
                for(OrderItem orderProduct : orderItemList ){
                    ProductPrice productPrice = productPriceRepository.findByProductId(orderProduct.getProductId());
                    newSaleAmount += (productPrice.getUnitSalePrice() * orderProduct.getQuantity()) - ((productPrice.getUnitSalePrice() * orderProduct.getQuantity())*(orderProduct.getDiscount()/100));
                }
                sale.setSaleAmount(newSaleAmount);
                sale.setDuePayment((newSaleAmount + sale.getDeliveryAmount()) - sale.getAdvancePayment());
                saleRepository.save(sale);
                iAudit.save("DELETE_ORDER_ITEM","DELETE ORDER ITEM "+orderItem.getProduct().getSku()+" FOR ORDER "+orderItem.getOrderId()+".",user.getUsername());
                return ResponseDelete.builder()
                        .message(Constants.delete)
                        .code(200)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> add(Long orderId,RequestOrderItem requestOrderItem, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Ordering ordering;
            Product product;
            OrderItem orderItem;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                ordering = orderingRepository.findById(orderId).orElse(null);
                product = productRepository.findBySkuAndStatusTrue(requestOrderItem.getProductSku().toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(ordering == null){
                throw new BadRequestExceptions(Constants.ErrorOrdering);
            }

            if(product == null){
                throw new BadRequestExceptions(Constants.ErrorProduct);
            }else {
                orderItem  = orderItemRepository.findByOrderIdAndProductId(ordering.getId(),product.getId());
            }

            if(orderItem != null ){
                throw new BadRequestExceptions(Constants.ErrorOrderItemExists);
            }

            try{
                Sale sale = saleRepository.findByOrderId(ordering.getId());
                OrderItem newOrderItem = orderItemRepository.save(OrderItem.builder()
                        .ordering(ordering)
                        .orderId(ordering.getId())
                        .status(true)
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .discount(requestOrderItem.getDiscount())
                        .observations(requestOrderItem.getObservations().toUpperCase())
                        .product(product)
                        .productId(product.getId())
                        .quantity(requestOrderItem.getQuantity())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .updateDate(new Date(System.currentTimeMillis()))
                        .tokenUser(user.getUsername())
                        .build());
                List<OrderItem> orderItemList = orderItemRepository.findAllByOrderIdAndStatusTrue(ordering.getId());
                double newSaleAmount = 0.00;
                for(OrderItem orderProduct : orderItemList ){
                    ProductPrice productPrice = productPriceRepository.findByProductId(orderProduct.getProductId());
                    newSaleAmount += (productPrice.getUnitSalePrice() * orderProduct.getQuantity()) - ((productPrice.getUnitSalePrice() * orderProduct.getQuantity())*(requestOrderItem.getDiscount()/100));
                }
                sale.setSaleAmount(newSaleAmount);
                sale.setDuePayment((newSaleAmount + sale.getDeliveryAmount()) - sale.getAdvancePayment());
                saleRepository.save(sale);
                iAudit.save("ADD_ORDER_ITEM","ADD ORDER ITEM "+newOrderItem.getProduct().getSku()+" FOR ORDER "+newOrderItem.getOrderId()+" WITH "+newOrderItem.getQuantity()+" UNITS.",user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> update(Long orderId, RequestOrderItem requestOrderItem, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Ordering ordering;
            Product product;
            OrderItem orderItem;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                ordering = orderingRepository.findById(orderId).orElse(null);
                product = productRepository.findBySkuAndStatusTrue(requestOrderItem.getProductSku().toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(ordering == null){
                throw new BadRequestExceptions(Constants.ErrorOrdering);
            }

            if(product == null){
                throw new BadRequestExceptions(Constants.ErrorProduct);
            }else {
                orderItem  = orderItemRepository.findByOrderIdAndProductId(ordering.getId(),product.getId());
            }

            if(orderItem == null ){
                throw new BadRequestExceptions(Constants.ErrorOrderItem);
            }

            try{
                Sale sale = saleRepository.findByOrderId(ordering.getId());
                orderItem.setQuantity(requestOrderItem.getQuantity());
                orderItem.setDiscount(requestOrderItem.getDiscount());
                orderItem.setUpdateDate(new Date(System.currentTimeMillis()));
                orderItem.setObservations(requestOrderItem.getObservations().toUpperCase());
                orderItemRepository.save(orderItem);
                List<OrderItem> orderItemList = orderItemRepository.findAllByOrderIdAndStatusTrue(ordering.getId());
                double newSaleAmount = 0.00;
                for(OrderItem orderProduct : orderItemList ){
                    ProductPrice productPrice = productPriceRepository.findByProductId(orderProduct.getProductId());
                    newSaleAmount += (productPrice.getUnitSalePrice() * orderProduct.getQuantity()) - ((productPrice.getUnitSalePrice() * orderProduct.getQuantity())*(requestOrderItem.getDiscount()/100));
                }
                sale.setSaleAmount(newSaleAmount);
                sale.setDuePayment((newSaleAmount + sale.getDeliveryAmount()) - sale.getAdvancePayment());
                saleRepository.save(sale);
                iAudit.save("UPDATE_ORDER_ITEM","UPDATE ORDER ITEM "+orderItem.getProduct().getSku()+" FOR ORDER "+orderItem.getOrderId()+".",user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<OrderItemDTO>> listOrderItems(String user, Long orderId, String productSku, Integer quantity, Double discount, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Long clientId;
            Page<OrderItem> pageOrderItem;
            Long productId;
            if(productSku != null){
                productId = productRepository.findBySku(productSku.toUpperCase()).getId();
            }else{
                productId = null;
            }
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                pageOrderItem = orderItemRepositoryCustom.searchForOrderItem(clientId,orderId,productId,quantity,discount,sort,sortColumn,pageNumber,pageSize,true);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(pageOrderItem.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }
            List<OrderItemDTO> orderItemDTOS = pageOrderItem.getContent().stream().map(orderItem -> {
                List<String> productPictures = productPictureRepository.findAlByClientIdAndProductId(clientId,orderItem.getProductId()).stream().map(ProductPicture::getProductPictureUrl).toList();
                ProductPrice productPrice = productPriceRepository.findByProductId(orderItem.getProductId());
                Double totalPrice = (productPrice.getUnitSalePrice() * orderItem.getQuantity())-((productPrice.getUnitSalePrice() * orderItem.getQuantity())*(orderItem.getDiscount()/100));
                return OrderItemDTO.builder()
                        .productId(orderItem.getProductId())
                        .orderItemId(orderItem.getId())
                        .unit(orderItem.getProduct().getUnit().getName())
                        .color(orderItem.getProduct().getColor().getName())
                        .size(orderItem.getProduct().getSize().getName())
                        .pictures(productPictures)
                        .category(orderItem.getProduct().getCategoryProduct().getName())
                        .sku(orderItem.getProduct().getSku())
                        .unitPrice(productPrice.getUnitSalePrice())
                        .discount(orderItem.getDiscount())
                        .quantity(orderItem.getQuantity())
                        .orderId(orderItem.getOrderId())
                        .totalPrice(totalPrice)
                        .observations(orderItem.getObservations())
                        .build();
            }).toList();
            return new PageImpl<>(orderItemDTOS,pageOrderItem.getPageable(),pageOrderItem.getTotalElements());
        });
    }
}
