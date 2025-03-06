package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.WarehouseOutputDTO;
import com.proyect.masterdata.dto.WarehouseOutputItemDTO;
import com.proyect.masterdata.dto.request.RequestWarehouseOutputItem;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IUtil;
import com.proyect.masterdata.services.IWarehouseOutputItem;
import com.proyect.masterdata.services.IWarehouseStock;
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
public class WarehouseOutputItemImpl implements IWarehouseOutputItem {
    ProductRepository productRepository;
    UserRepository userRepository;
    WarehouseOutputItemRepository warehouseOutputItemRepository;
    WarehouseOutputRepository warehouseOutputRepository;
    WarehouseOutputItemRepositoryCustom warehouseOutputItemRepositoryCustom;
    WarehouseStockRepository warehouseStockRepository;
    IUtil iUtil;
    IAudit iAudit;
    IWarehouseStock iWarehouseStock;
    @Override
    public WarehouseOutputItem save(RequestWarehouseOutputItem requestWarehouseOutputItem, WarehouseOutput warehouseOutput, User user) throws BadRequestExceptions, InternalErrorExceptions {
        Product product;
        WarehouseOutputItem warehouseOutputItem;
        WarehouseStock warehouseStock;
        try{
            product = productRepository.findByIdAndStatusTrue(requestWarehouseOutputItem.getProductId());
            warehouseOutputItem = warehouseOutputItemRepository.findByProductIdAndWarehouseOutputId(requestWarehouseOutputItem.getProductId(),warehouseOutput.getId());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
        if(product==null){
            throw new BadRequestExceptions(Constants.ErrorProduct);
        }
        if(warehouseOutputItem!=null){
            throw new BadRequestExceptions(Constants.ErrorWarehouseOutputItemExists);
        }
        if(requestWarehouseOutputItem.getQuantity()<1){
            throw new BadRequestExceptions(Constants.ErrorWarehouseOutputItemZero);
        }else{
            warehouseStock = warehouseStockRepository.findByWarehouseIdAndProductId(warehouseOutput.getWarehouse().getId(),product.getId());
        }
        if(requestWarehouseOutputItem.getQuantity()>warehouseStock.getQuantity()){
            throw new BadRequestExceptions(Constants.ErrorWarehouseStockLess);
        }
        try{
            WarehouseOutputItem newWarehouseOutputItem = warehouseOutputItemRepository.save(WarehouseOutputItem.builder()
                            .warehouseOutput(warehouseOutput)
                            .warehouseOutputId(warehouseOutput.getId())
                            .registrationDate(OffsetDateTime.now())
                            .updateDate(OffsetDateTime.now())
                            .status(true)
                            .user(user)
                            .userId(user.getId())
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .quantity(requestWarehouseOutputItem.getQuantity())
                            .product(product)
                            .productId(product.getId())
                    .build());
            iWarehouseStock.out(warehouseOutput.getWarehouse(),product,newWarehouseOutputItem.getQuantity(),user);
            iAudit.save("ADD_WAREHOUSE_OUTPUT_ITEM",
                    "ITEM " +
                        iUtil.buildProductSku(product) +
                    " DE SALIDA DE ALMACEN "+
                            warehouseOutput.getOrderNumber()+
                            " GUARDADO.",
                    warehouseOutput.getOrderNumber().toString(),user.getUsername());
            return newWarehouseOutputItem;
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public CompletableFuture<ResponseSuccess> add(RequestWarehouseOutputItem requestWarehouseOutputItem, UUID warehouseOutputId, String username) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Product product;
            WarehouseOutputItem warehouseOutputItem;
            WarehouseOutput warehouseOutput;
            User user;
            WarehouseStock warehouseStock;
            try{
                product = productRepository.findByIdAndStatusTrue(requestWarehouseOutputItem.getProductId());
                warehouseOutput = warehouseOutputRepository.findById(warehouseOutputId).orElse(null);
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(product==null){
                throw new BadRequestExceptions(Constants.ErrorProduct);
            }
            if(warehouseOutput==null){
                throw new BadRequestExceptions(Constants.ErrorWarehouseOutput);
            }else{
                warehouseOutputItem = warehouseOutputItemRepository.findByProductIdAndWarehouseOutputId(requestWarehouseOutputItem.getProductId(),warehouseOutput.getId());
            }
            if(warehouseOutputItem!=null){
                throw new BadRequestExceptions(Constants.ErrorWarehouseOutputItemExists);
            }
            if(requestWarehouseOutputItem.getQuantity()<1){
                throw new BadRequestExceptions(Constants.ErrorWarehouseOutputItemZero);
            }else{
                warehouseStock = warehouseStockRepository.findByWarehouseIdAndProductId(warehouseOutput.getWarehouse().getId(),product.getId());
            }
            if(requestWarehouseOutputItem.getQuantity()>warehouseStock.getQuantity()){
                throw new BadRequestExceptions(Constants.ErrorWarehouseStockLess);
            }
            if(!warehouseOutput.getStatus()){
                throw new BadRequestExceptions(Constants.ErrorWarehouseOutputInactive);
            }
            try{
                WarehouseOutputItem newWarehouseOutputItem = warehouseOutputItemRepository.save(WarehouseOutputItem.builder()
                        .warehouseOutput(warehouseOutput)
                        .warehouseOutputId(warehouseOutput.getId())
                        .registrationDate(OffsetDateTime.now())
                        .updateDate(OffsetDateTime.now())
                        .status(true)
                        .user(user)
                        .userId(user.getId())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .quantity(requestWarehouseOutputItem.getQuantity())
                        .product(product)
                        .productId(product.getId())
                        .build());
                iWarehouseStock.out(warehouseOutput.getWarehouse(),product,newWarehouseOutputItem.getQuantity(),user);
                iAudit.save("ADD_WAREHOUSE_OUTPUT_ITEM",
                        "ITEM " +
                                iUtil.buildProductSku(product) +
                                " DE SALIDA DE ALMACEN "+
                                warehouseOutput.getOrderNumber()+
                                " GUARDADO.",
                        warehouseOutput.getOrderNumber().toString(),user.getUsername());
                return ResponseSuccess.builder()
                        .message(Constants.register)
                        .code(200)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<WarehouseOutputItemDTO>> list(
            String user,
            Long orderNumber,
            String ref,
            String courier,
            String warehouse,
            Integer quantity,
            String model,
            String product,
            String color,
            String size,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status) {

        return CompletableFuture.supplyAsync(()->{
            Page<WarehouseOutputItem> warehouseOutputItemPage;
            UUID clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                warehouseOutputItemPage = warehouseOutputItemRepositoryCustom.searchForWarehouseOutputItem(
                        clientId,
                        orderNumber,
                        ref,
                        courier,
                        warehouse,
                        quantity,
                        model,
                        product,
                        color,
                        size,
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

            if (warehouseOutputItemPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<WarehouseOutputItemDTO> warehouseOutputItemDTOs = warehouseOutputItemPage.getContent().stream().map(warehouseOutputItem -> WarehouseOutputItemDTO.builder()
                    .courier(warehouseOutputItem.getWarehouseOutput().getCourier().getName())
                    .orderNumber(warehouseOutputItem.getWarehouseOutput().getOrderNumber())
                    .ref(warehouseOutputItem.getWarehouseOutput().getRef())
                    .warehouse(warehouseOutputItem.getWarehouseOutput().getWarehouse().getName())
                    .quantity(warehouseOutputItem.getQuantity())
                        .productSku(iUtil.buildProductSku(warehouseOutputItem.getProduct()))
                        .productId(warehouseOutputItem.getProductId())
                        .updateDate(warehouseOutputItem.getUpdateDate())
                        .registrationDate(warehouseOutputItem.getRegistrationDate())
                        .user(warehouseOutputItem.getUser().getUsername())
                        .build()).toList();

            return new PageImpl<>(warehouseOutputItemDTOs, warehouseOutputItemPage.getPageable(), warehouseOutputItemPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> delete(UUID productId, UUID warehouseOutputId, String username) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Product product;
            WarehouseOutputItem warehouseOutputItem;
            WarehouseOutput warehouseOutput;
            User user;
            try{
                product = productRepository.findByIdAndStatusTrue(productId);
                warehouseOutput = warehouseOutputRepository.findById(warehouseOutputId).orElse(null);
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(product==null){
                throw new BadRequestExceptions(Constants.ErrorProduct);
            }
            if(warehouseOutput==null){
                throw new BadRequestExceptions(Constants.ErrorWarehouseOutput);
            }else{
                warehouseOutputItem = warehouseOutputItemRepository.findByProductIdAndWarehouseOutputIdAndStatusTrue(product.getId(),warehouseOutput.getId());
            }
            if(warehouseOutputItem==null){
                throw new BadRequestExceptions(Constants.ErrorWarehouseOutputItem);
            }
            if(!warehouseOutput.getStatus()){
                throw new BadRequestExceptions(Constants.ErrorWarehouseOutputInactive);
            }
            try{
                warehouseOutputItem.setStatus(false);
                warehouseOutputItem.setUpdateDate(OffsetDateTime.now());
                warehouseOutputItem.setUser(user);
                warehouseOutputItem.setUserId(user.getId());
                warehouseOutputItemRepository.save(warehouseOutputItem);
                iWarehouseStock.out(warehouseOutput.getWarehouse(),product,warehouseOutputItem.getQuantity(),user);
                iAudit.save("ADD_WAREHOUSE_OUTPUT_ITEM",
                        "ITEM " +
                                iUtil.buildProductSku(product) +
                                " DE SALIDA DE ALMACEN "+
                                warehouseOutput.getOrderNumber()+
                                " ELIMINADO.",
                        warehouseOutput.getOrderNumber().toString(),user.getUsername());
                return ResponseDelete.builder()
                        .message(Constants.delete)
                        .code(200)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(UUID productId, UUID warehouseOutputId, String username) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Product product;
            WarehouseOutputItem warehouseOutputItem;
            WarehouseOutput warehouseOutput;
            User user;
            try{
                product = productRepository.findByIdAndStatusTrue(productId);
                warehouseOutput = warehouseOutputRepository.findById(warehouseOutputId).orElse(null);
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(product==null){
                throw new BadRequestExceptions(Constants.ErrorProduct);
            }
            if(warehouseOutput==null){
                throw new BadRequestExceptions(Constants.ErrorWarehouseOutput);
            }else{
                warehouseOutputItem = warehouseOutputItemRepository.findByProductIdAndWarehouseOutputIdAndStatusFalse(product.getId(),warehouseOutput.getId());
            }
            if(warehouseOutputItem==null){
                throw new BadRequestExceptions(Constants.ErrorWarehouseOutputItem);
            }
            if(!warehouseOutput.getStatus()){
                throw new BadRequestExceptions(Constants.ErrorWarehouseOutputInactive);
            }
            try{
                warehouseOutputItem.setStatus(true);
                warehouseOutputItem.setUpdateDate(OffsetDateTime.now());
                warehouseOutputItem.setUser(user);
                warehouseOutputItem.setUserId(user.getId());
                warehouseOutputItemRepository.save(warehouseOutputItem);
                iWarehouseStock.out(warehouseOutput.getWarehouse(),product,warehouseOutputItem.getQuantity(),user);
                iAudit.save("ADD_WAREHOUSE_OUTPUT_ITEM",
                        "ITEM " +
                                iUtil.buildProductSku(product) +
                                " DE SALIDA DE ALMACEN "+
                                warehouseOutput.getOrderNumber()+
                                " ACTIVADO.",
                        warehouseOutput.getOrderNumber().toString(),user.getUsername());
                return ResponseSuccess.builder()
                        .message(Constants.update)
                        .code(200)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> updateQuantity(Integer quantity, UUID productId, UUID warehouseOutputId, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Product product;
            WarehouseOutputItem warehouseOutputItem;
            WarehouseOutput warehouseOutput;
            User user;
            WarehouseStock warehouseStock;
            try{
                product = productRepository.findByIdAndStatusTrue(productId);
                warehouseOutput = warehouseOutputRepository.findById(warehouseOutputId).orElse(null);
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(product==null){
                throw new BadRequestExceptions(Constants.ErrorProduct);
            }
            if(warehouseOutput==null){
                throw new BadRequestExceptions(Constants.ErrorWarehouseOutput);
            }else{
                warehouseOutputItem = warehouseOutputItemRepository.findByProductIdAndWarehouseOutputIdAndStatusTrue(product.getId(),warehouseOutput.getId());
            }
            if(warehouseOutputItem==null){
                throw new BadRequestExceptions(Constants.ErrorWarehouseOutputItem);
            }else{
                warehouseStock = warehouseStockRepository.findByWarehouseIdAndProductId(warehouseOutput.getWarehouse().getId(),product.getId());
            }
            if(!warehouseOutput.getStatus()){
                throw new BadRequestExceptions(Constants.ErrorWarehouseOutputInactive);
            }
            if(quantity>warehouseStock.getQuantity()){
                throw new BadRequestExceptions(Constants.ErrorWarehouseStockLess);
            }
            if(quantity.equals(warehouseOutputItem.getQuantity())){
                throw new BadRequestExceptions(Constants.ErrorWarehouseOutputItemEqualUpdate);
            }
            try{
                warehouseOutputItem.setQuantity(quantity);
                warehouseOutputItem.setUpdateDate(OffsetDateTime.now());
                warehouseOutputItem.setUser(user);
                warehouseOutputItem.setUserId(user.getId());
                warehouseOutputItemRepository.save(warehouseOutputItem);
                int updatedQuantity;
                if(quantity>warehouseOutputItem.getQuantity()){
                    updatedQuantity = quantity-warehouseOutputItem.getQuantity();
                    iWarehouseStock.out(warehouseOutput.getWarehouse(),product,updatedQuantity,user);
                }
                if(quantity<warehouseOutputItem.getQuantity()){
                    updatedQuantity = warehouseOutputItem.getQuantity()-quantity;
                    iWarehouseStock.in(warehouseOutput.getWarehouse(),product,updatedQuantity,user);
                }
                iAudit.save("ADD_WAREHOUSE_OUTPUT_ITEM",
                        "ITEM " +
                                iUtil.buildProductSku(product) +
                                " DE SALIDA DE ALMACEN "+
                                warehouseOutput.getOrderNumber()+
                                " ACTUALIZADO.",
                        warehouseOutput.getOrderNumber().toString(),user.getUsername());
                return ResponseSuccess.builder()
                        .message(Constants.update)
                        .code(200)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
        });
    }
}
