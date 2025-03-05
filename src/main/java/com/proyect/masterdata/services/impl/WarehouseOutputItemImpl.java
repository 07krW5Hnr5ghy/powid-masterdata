package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Product;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.WarehouseOutput;
import com.proyect.masterdata.domain.WarehouseOutputItem;
import com.proyect.masterdata.dto.request.RequestWarehouseOutputItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.ProductRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.repository.WarehouseOutputItemRepository;
import com.proyect.masterdata.repository.WarehouseOutputRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IUtil;
import com.proyect.masterdata.services.IWarehouseOutputItem;
import com.proyect.masterdata.services.IWarehouseStock;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
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
    IUtil iUtil;
    IAudit iAudit;
    IWarehouseStock iWarehouseStock;
    @Override
    public WarehouseOutputItem save(RequestWarehouseOutputItem requestWarehouseOutputItem, WarehouseOutput warehouseOutput, User user) throws BadRequestExceptions, InternalErrorExceptions {
        Product product;
        WarehouseOutputItem warehouseOutputItem;
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
}
