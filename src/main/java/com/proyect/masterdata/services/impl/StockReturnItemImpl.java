package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.StockReturnItemDTO;
import com.proyect.masterdata.dto.request.RequestStockReturnItem;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IStockReturnItem;
import com.proyect.masterdata.services.IUtil;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class StockReturnItemImpl implements IStockReturnItem {
    private final UserRepository userRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final StockReturnItemRepository stockReturnItemRepository;
    private final StockReturnItemRepositoryCustom stockReturnItemRepositoryCustom;
    private final StockReturnRepository stockReturnRepository;
    private final SupplierRepository supplierRepository;
    private final PurchaseRepository purchaseRepository;
    private final IAudit iAudit;
    private final IUtil iUtil;
    @Override
    public StockReturnItem save(StockReturn stockReturn, SupplierProduct supplierProduct, RequestStockReturnItem requestStockReturnItem, User user) throws InternalErrorExceptions, BadRequestExceptions {

        try{
            StockReturnItem newStockReturnItem = stockReturnItemRepository.save(StockReturnItem.builder()
                    .user(user).userId(user.getId())
                    .quantity(requestStockReturnItem.getQuantity())
                    .supplierProduct(supplierProduct)
                    .supplierProductId(supplierProduct.getId())
                    .client(user.getClient())
                    .clientId(user.getClientId())
                    .stockReturn(stockReturn)
                    .stockReturnId(stockReturn.getId())
                    .observations(requestStockReturnItem.getObservations().toUpperCase())
                    .registrationDate(OffsetDateTime.now())
                    .status(true)
                    .build());
            iAudit.save(
                    "ADD_STOCK_RETURN_ITEM",
                    "PRODUCTO DE INVENTARIO "+
                            iUtil.buildInventorySku(newStockReturnItem.getSupplierProduct())+
                            " AGREGADO A DEVOLUCION DE STOCK "+
                            stockReturn.getSerial(),stockReturn.getSerial(),user.getUsername());
            return newStockReturnItem;
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public CompletableFuture<StockReturnItem> saveAsync(StockReturn stockReturn,SupplierProduct supplierProduct, RequestStockReturnItem requestStockReturnItem, User user) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            try{
                StockReturnItem newStockReturnItem = stockReturnItemRepository.save(StockReturnItem.builder()
                        .user(user).userId(user.getId())
                        .quantity(requestStockReturnItem.getQuantity())
                        .supplierProduct(supplierProduct)
                        .supplierProductId(supplierProduct.getId())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .stockReturn(stockReturn)
                        .stockReturnId(stockReturn.getId())
                        .observations(requestStockReturnItem.getObservations().toUpperCase())
                        .registrationDate(OffsetDateTime.now())
                        .status(true)
                        .build());
                iAudit.save(
                        "ADD_STOCK_RETURN_ITEM",
                        "PRODUCTO DE INVENTARIO "+
                                iUtil.buildInventorySku(newStockReturnItem.getSupplierProduct())+
                                " AGREGADO A DEVOLUCION DE STOCK "+
                                stockReturn.getSerial(),stockReturn.getSerial(),user.getUsername());
                return newStockReturnItem;
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<StockReturnItemDTO>> list(
            String user,
            String serial,
            List<String> suppliers,
            String supplierProduct,
            String product,
            String model,
            String color,
            String size,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<StockReturnItem> pageStockReturn;
            UUID clientId;
            List<UUID> supplierIds;

            if(suppliers != null && !suppliers.isEmpty()){
                supplierIds = supplierRepository.findByRucIn(
                        suppliers.stream().map(String::toUpperCase).toList()
                ).stream().map(Supplier::getId).toList();
            }else{
                supplierIds = new ArrayList<>();
            }

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                pageStockReturn = stockReturnItemRepositoryCustom.searchForStockReturnItem(
                        clientId,
                        serial,
                        supplierIds,
                        supplierProduct,
                        product,
                        model,
                        color,
                        size,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize);
            } catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if(pageStockReturn.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }

            List<StockReturnItemDTO> stockReturnDTOS = pageStockReturn.getContent().stream().map(stockReturnItem -> StockReturnItemDTO.builder()
                    .serial(stockReturnItem.getStockReturn().getSerial())
                    .supplier(stockReturnItem.getStockReturn().getSupplier().getBusinessName())
                    .supplierProduct(iUtil.buildInventorySku(stockReturnItem.getSupplierProduct()))
                    .productSku(iUtil.buildProductSku(stockReturnItem.getSupplierProduct().getProduct()))
                    .model(stockReturnItem.getSupplierProduct().getProduct().getModel().getName())
                    .color(stockReturnItem.getSupplierProduct().getProduct().getColor().getName())
                    .size(stockReturnItem.getSupplierProduct().getProduct().getSize().getName())
                    .registrationDate(stockReturnItem.getRegistrationDate())
                    .quantity(stockReturnItem.getQuantity())
                    .observations(stockReturnItem.getObservations())
                    .build()
            ).toList();
            return new PageImpl<>(stockReturnDTOS,pageStockReturn.getPageable(),pageStockReturn.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<StockReturnItemDTO>> listStockReturnItem(String user,UUID id) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<StockReturnItem> stockReturnItems;
            UUID clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                if(id != null){
                    stockReturnItems = stockReturnItemRepository.findAllByClientIdAndStockReturnIdAndStatusTrue(clientId,id);
                }else{
                    stockReturnItems = stockReturnItemRepository.findAllByClientIdAndStatusTrue(clientId);
                }
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(stockReturnItems.isEmpty()){
                return Collections.emptyList();
            }
            return stockReturnItems.stream().map(stockReturnItem -> StockReturnItemDTO.builder()
                    .supplierProduct(iUtil.buildInventorySku(stockReturnItem.getSupplierProduct()))
                    .productSku(iUtil.buildProductSku(stockReturnItem.getSupplierProduct().getProduct()))
                    .model(stockReturnItem.getSupplierProduct().getProduct().getModel().getName())
                    .color(stockReturnItem.getSupplierProduct().getProduct().getColor().getName())
                    .size(stockReturnItem.getSupplierProduct().getProduct().getSize().getName())
                    .registrationDate(stockReturnItem.getRegistrationDate())
                    .quantity(stockReturnItem.getQuantity())
                    .observations(stockReturnItem.getObservations())
                    .supplier(stockReturnItem.getSupplierProduct().getSupplier().getBusinessName())
                    .serial(stockReturnItem.getStockReturn().getSerial())
                    .build()
            ).toList();
        });
    }

    @Override
    public CompletableFuture<List<StockReturnItemDTO>> listStockReturnItemFalse(String user,UUID id) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<StockReturnItem> stockReturnItems;
            UUID clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                if(id != null){
                    stockReturnItems = stockReturnItemRepository.findAllByClientIdAndStockReturnIdAndStatusFalse(clientId,id);
                }else{
                    stockReturnItems = stockReturnItemRepository.findAllByClientIdAndStatusFalse(clientId);
                }
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(stockReturnItems.isEmpty()){
                return Collections.emptyList();
            }
            return stockReturnItems.stream().map(stockReturnItem -> StockReturnItemDTO.builder()
                    .supplierProduct(iUtil.buildInventorySku(stockReturnItem.getSupplierProduct()))
                    .productSku(iUtil.buildProductSku(stockReturnItem.getSupplierProduct().getProduct()))
                    .model(stockReturnItem.getSupplierProduct().getProduct().getModel().getName())
                    .color(stockReturnItem.getSupplierProduct().getProduct().getColor().getName())
                    .size(stockReturnItem.getSupplierProduct().getProduct().getSize().getName())
                    .registrationDate(stockReturnItem.getRegistrationDate())
                    .quantity(stockReturnItem.getQuantity())
                    .observations(stockReturnItem.getObservations())
                    .supplier(stockReturnItem.getSupplierProduct().getSupplier().getBusinessName())
                    .serial(stockReturnItem.getStockReturn().getSerial())
                    .build()
            ).toList();
        });
    }

}