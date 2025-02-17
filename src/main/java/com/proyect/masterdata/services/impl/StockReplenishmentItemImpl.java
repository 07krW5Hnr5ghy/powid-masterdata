package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.StockReplenishmentItemDTO;
import com.proyect.masterdata.dto.request.RequestStockReplenishmentItem;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IStockReplenishmentItem;
import com.proyect.masterdata.services.IUtil;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.UUID;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class StockReplenishmentItemImpl implements IStockReplenishmentItem {
    private final StockReplenishmentItemRepository stockReplenishmentItemRepository;
    private final StockReplenishmentItemRepositoryCustom stockReplenishmentItemRepositoryCustom;
    private final UserRepository userRepository;
    private final ProductRepository productRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final OrderItemRepository orderItemRepository;
    private final StockReplenishmentRepository stockReplenishmentRepository;
    private final IAudit iAudit;
    private final IUtil iUtil;
    @Override
    public StockReplenishmentItem save(OrderItem orderItem, RequestStockReplenishmentItem requestStockReplenishmentItem, User user, StockReplenishment stockReplenishment) throws InternalErrorExceptions, BadRequestExceptions {
        try{
            StockReplenishmentItem newStockReplenishmentItem = stockReplenishmentItemRepository.save(StockReplenishmentItem.builder()
                            .stockReplenishment(stockReplenishment)
                            .stockReplenishmentId(stockReplenishment.getId())
                            .ordering(orderItem.getOrdering())
                            .orderId(orderItem.getOrderId())
                            .user(user)
                            .userId(user.getId())
                            .product(orderItem.getProduct())
                            .productId(orderItem.getProductId())
                            .quantity(requestStockReplenishmentItem.getQuantity())
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .registrationDate(OffsetDateTime.now())
                            .updateDate(OffsetDateTime.now())
                            .status(true)
                    .build());
            iAudit.save("ADD_STOCK_REPLENISHMENT_ITEM","PRODUCTO "+newStockReplenishmentItem.getProduct() + " EN PEDIDO "+newStockReplenishmentItem.getOrderId() + " CREADO EN RESTOCKAJE.",newStockReplenishmentItem.getOrderId().toString(),user.getUsername());
            return newStockReplenishmentItem;
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public CompletableFuture<StockReplenishmentItem> saveAsync(OrderItem orderItem, RequestStockReplenishmentItem requestStockReplenishmentItem, User user, StockReplenishment stockReplenishment) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            try{
                StockReplenishmentItem newStockReplenishmentItem = stockReplenishmentItemRepository.save(StockReplenishmentItem.builder()
                        .stockReplenishment(stockReplenishment)
                        .stockReplenishmentId(stockReplenishment.getId())
                        .ordering(orderItem.getOrdering())
                        .orderId(orderItem.getOrderId())
                        .user(user)
                                .userId(user.getId())
                        .product(orderItem.getProduct())
                        .productId(orderItem.getProductId())
                        .quantity(requestStockReplenishmentItem.getQuantity())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .registrationDate(OffsetDateTime.now())
                        .updateDate(OffsetDateTime.now())
                        .status(true)
                        .build());
                iAudit.save("ADD_STOCK_REPLENISHMENT_ITEM","PRODUCTO "+newStockReplenishmentItem.getProduct() + " EN PEDIDO "+newStockReplenishmentItem.getOrderId() + " CREADO EN RESTOCKAJE.",newStockReplenishmentItem.getOrderId().toString(),user.getUsername());
                return newStockReplenishmentItem;
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> add(UUID orderId, RequestStockReplenishmentItem requestStockReplenishmentItem, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Product product;
            OrderItem orderItem;
            StockReplenishment stockReplenishment;
            StockReplenishmentItem stockReplenishmentItem;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                product = productRepository.findBySkuAndStatusTrue(requestStockReplenishmentItem.getProductSku().toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(requestStockReplenishmentItem.getQuantity()<1){
                throw new BadRequestExceptions(Constants.ErrorStockReplenishmentItemZero);
            }
            if(product==null){
                throw new BadRequestExceptions(Constants.ErrorProduct);
            }else{
                orderItem = orderItemRepository.findByOrderIdAndProductId(orderId, product.getId());
            }
            if(orderItem==null){
                throw new BadRequestExceptions(Constants.ErrorOrderItem);
            }else{
                stockReplenishment = stockReplenishmentRepository.findByOrderId(orderItem.getOrderId());
                stockReplenishmentItem = stockReplenishmentItemRepository.findByOrderIdAndProductId(orderId, product.getId());
            }
            if(requestStockReplenishmentItem.getQuantity()>orderItem.getQuantity()){
                throw new BadRequestExceptions(Constants.ErrorStockReplenishmentItemQuantity);
            }
            if(stockReplenishmentItem!=null){
                throw new BadRequestExceptions(Constants.ErrorStockReplenishmentItemExists);
            }
            try {
                StockReplenishmentItem newStockReplenishmentItem = stockReplenishmentItemRepository.save(StockReplenishmentItem.builder()
                                .quantity(requestStockReplenishmentItem.getQuantity())
                                .ordering(orderItem.getOrdering())
                                .orderId(orderItem.getOrderId())
                                .stockReplenishmentId(stockReplenishment.getId())
                                .stockReplenishment(stockReplenishment)
                                .registrationDate(OffsetDateTime.now())
                                .client(user.getClient())
                                .clientId(user.getClientId())
                                .product(product)
                                .productId(product.getId())
                        .build());
                iAudit.save("ADD_STOCK_REPLENISHMENT_ITEM","PRODUCTO "+newStockReplenishmentItem.getProduct() + " EN PEDIDO "+newStockReplenishmentItem.getOrderId() + " AGREGADO EN RESTOCKAJE.",newStockReplenishmentItem.getOrderId().toString(),user.getUsername());
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
    public CompletableFuture<ResponseDelete> delete(UUID orderId, String productSku, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Product product;
            OrderItem orderItem;
            StockReplenishmentItem stockReplenishmentItem;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                product = productRepository.findBySkuAndStatusTrue(productSku.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(product==null){
                throw new BadRequestExceptions(Constants.ErrorProduct);
            }else{
                stockReplenishmentItem = stockReplenishmentItemRepository.findByOrderIdAndProductIdAndStatusTrue(orderId, product.getId());
            }
            if(stockReplenishmentItem==null){
                throw new BadRequestExceptions(Constants.ErrorStockReplenishment);
            }
            try {
                stockReplenishmentItem.setStatus(false);
                stockReplenishmentItem.setUpdateDate(OffsetDateTime.now());
                stockReplenishmentItem.setUser(user);
                stockReplenishmentItem.setUserId(user.getId());
                stockReplenishmentItemRepository.save(stockReplenishmentItem);
                iAudit.save("DELETE_STOCK_REPLENISHMENT_ITEM","PRODUCTO "+iUtil.buildProductSku(stockReplenishmentItem.getProduct())+" EN PEDIDO "+stockReplenishmentItem.getOrderId()+" DESACTIVADO EN RESTOCKAJE.",stockReplenishmentItem.getOrderId().toString(),user.getUsername());
                return ResponseDelete.builder()
                        .code(200)
                        .message(Constants.delete)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> update(UUID orderId, String productSku, Integer quantity, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Product product;
            OrderItem orderItem;
            StockReplenishmentItem stockReplenishmentItem;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                product = productRepository.findBySkuAndStatusTrue(productSku.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(product==null){
                throw new BadRequestExceptions(Constants.ErrorProduct);
            }else{
                orderItem = orderItemRepository.findByOrderIdAndProductId(orderId, product.getId());
                stockReplenishmentItem = stockReplenishmentItemRepository.findByOrderIdAndProductIdAndStatusTrue(orderId, product.getId());
            }
            if(stockReplenishmentItem==null){
                throw new BadRequestExceptions(Constants.ErrorStockReplenishment);
            }
            if(orderItem==null){
                throw new BadRequestExceptions(Constants.ErrorOrderItem);
            }
            if(quantity<1){
                throw new BadRequestExceptions(Constants.ErrorStockReplenishmentItemZero);
            }
            if(quantity > orderItem.getQuantity()){
                throw new BadRequestExceptions(Constants.ErrorStockReplenishmentItemQuantity);
            }
            try {
                stockReplenishmentItem.setQuantity(quantity);
                stockReplenishmentItem.setUpdateDate(OffsetDateTime.now());
                stockReplenishmentItem.setUser(user);
                stockReplenishmentItem.setUserId(user.getId());
                stockReplenishmentItemRepository.save(stockReplenishmentItem);
                iAudit.save("UPDATE_STOCK_REPLENISHMENT_ITEM","PRODUCTO "+iUtil.buildProductSku(stockReplenishmentItem.getProduct())+" EN PEDIDO "+stockReplenishmentItem.getOrderId()+" ACTUALIZADO EN RESTOCKAJE.",stockReplenishmentItem.getOrderId().toString(),user.getUsername());
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
    public CompletableFuture<ResponseSuccess> activate(UUID orderId, String productSku, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Product product;
            StockReplenishmentItem stockReplenishmentItem;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                product = productRepository.findBySkuAndStatusTrue(productSku.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(product==null){
                throw new BadRequestExceptions(Constants.ErrorProduct);
            }else{
                stockReplenishmentItem = stockReplenishmentItemRepository.findByOrderIdAndProductIdAndStatusFalse(orderId, product.getId());
            }
            if(stockReplenishmentItem==null){
                throw new BadRequestExceptions(Constants.ErrorStockReplenishment);
            }
            try {
                stockReplenishmentItem.setStatus(true);
                stockReplenishmentItem.setUpdateDate(OffsetDateTime.now());
                stockReplenishmentItem.setUser(user);
                stockReplenishmentItem.setUserId(user.getId());
                stockReplenishmentItemRepository.save(stockReplenishmentItem);
                iAudit.save("ACTIVATE_STOCK_REPLENISHMENT_ITEM","PRODUCTO "+iUtil.buildProductSku(stockReplenishmentItem.getProduct())+" EN PEDIDO "+stockReplenishmentItem.getOrderId()+" ACTIVADO EN RESTOCKAJE.",stockReplenishmentItem.getOrderId().toString(),user.getUsername());
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
    public CompletableFuture<Page<StockReplenishmentItemDTO>> list(
            String user,
            List<UUID> orders,
            List<String> productSkus,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) {
        return CompletableFuture.supplyAsync(()->{
            Page<StockReplenishmentItem> pageStockReplenishmentItem;
            UUID clientId;
            List<UUID> orderIds;
            List<UUID> productIds;

            if(orders != null && !orders.isEmpty()){
                orderIds = orders;
            }else{
                orderIds = new ArrayList<>();
            }

            if(productSkus != null && !productSkus.isEmpty()){
                productIds = productRepository.findBySkuIn(
                        productSkus.stream().map(String::toUpperCase).toList()
                ).stream().map(Product::getId).toList();
            }else{
                productIds = new ArrayList<>();
            }

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                pageStockReplenishmentItem = stockReplenishmentItemRepositoryCustom.searchForStockReplenishmentItem(
                        clientId,
                        orderIds,
                        productIds,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        true);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if(pageStockReplenishmentItem.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }

            List<StockReplenishmentItemDTO> stockReplenishmentItemDTOS = pageStockReplenishmentItem.getContent().stream().map(stockReplenishmentItem -> StockReplenishmentItemDTO.builder()
                    .product(iUtil.buildProductSku(stockReplenishmentItem.getProduct()))
                    .orderId(stockReplenishmentItem.getOrderId())
                    .quantity(stockReplenishmentItem.getQuantity())
                    .registrationDate(stockReplenishmentItem.getRegistrationDate())
                    .updateDate(stockReplenishmentItem.getUpdateDate())
                    .build()).toList();

            return new PageImpl<>(stockReplenishmentItemDTOS,pageStockReplenishmentItem.getPageable(),pageStockReplenishmentItem.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<StockReplenishmentItemDTO>> listStockReplenishmentItem(String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<StockReplenishmentItem> stockReplenishmentItems;
            UUID clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                stockReplenishmentItems = stockReplenishmentItemRepository.findAllByClientIdAndStatusTrue(clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(stockReplenishmentItems.isEmpty()){
                return Collections.emptyList();
            }
            return stockReplenishmentItems.stream().map(stockReplenishmentItem -> StockReplenishmentItemDTO.builder()
                    .product(iUtil.buildProductSku(stockReplenishmentItem.getProduct()))
                    .orderId(stockReplenishmentItem.getOrderId())
                    .quantity(stockReplenishmentItem.getQuantity())
                    .registrationDate(stockReplenishmentItem.getRegistrationDate())
                    .updateDate(stockReplenishmentItem.getUpdateDate())
                    .build()).toList();
        });
    }

    @Override
    public CompletableFuture<List<StockReplenishmentItemDTO>> listStockReplenishmentItemFalse(String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<StockReplenishmentItem> stockReplenishmentItems;
            UUID clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                stockReplenishmentItems = stockReplenishmentItemRepository.findAllByClientIdAndStatusFalse(clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(stockReplenishmentItems.isEmpty()){
                return Collections.emptyList();
            }
            return stockReplenishmentItems.stream().map(stockReplenishmentItem -> StockReplenishmentItemDTO.builder()
                    .product(iUtil.buildProductSku(stockReplenishmentItem.getProduct()))
                    .orderId(stockReplenishmentItem.getOrderId())
                    .quantity(stockReplenishmentItem.getQuantity())
                    .registrationDate(stockReplenishmentItem.getRegistrationDate())
                    .updateDate(stockReplenishmentItem.getUpdateDate())
                    .build()).toList();
        });
    }
}
