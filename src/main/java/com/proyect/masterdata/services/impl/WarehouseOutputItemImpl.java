package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Product;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.WarehouseOutput;
import com.proyect.masterdata.domain.WarehouseOutputItem;
import com.proyect.masterdata.dto.request.RequestWarehouseOutputItem;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.ProductRepository;
import com.proyect.masterdata.repository.WarehouseOutputItemRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IUtil;
import com.proyect.masterdata.services.IWarehouseOutputItem;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.UUID;

@Service
@RequiredArgsConstructor
@Log4j2
public class WarehouseOutputItemImpl implements IWarehouseOutputItem {
    ProductRepository productRepository;
    WarehouseOutputItemRepository warehouseOutputItemRepository;
    IUtil iUtil;
    IAudit iAudit;
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
}
