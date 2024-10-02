package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.StockReturnItemDTO;
import com.proyect.masterdata.dto.request.RequestStockReturnItem;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IStockReturnItem;
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
public class StockReturnItemImpl implements IStockReturnItem {
    private final UserRepository userRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final StockReturnItemRepository stockReturnItemRepository;
    private final StockReturnItemRepositoryCustom stockReturnItemRepositoryCustom;
    private final StockReturnRepository stockReturnRepository;
    private final SupplierRepository supplierRepository;
    private final PurchaseRepository purchaseRepository;
    private final IAudit iAudit;
    @Override
    public StockReturnItem save(StockReturn stockReturn, SupplierProduct supplierProduct, RequestStockReturnItem requestStockReturnItem, User user) throws InternalErrorExceptions, BadRequestExceptions {

        try{
            iAudit.save("ADD_STOCK_RETURN_ITEM","PRODUCTO DE INVENTARIO "+requestStockReturnItem.getSupplierProduct().toUpperCase()+" AGREGADO A DEVOLUCION DE STOCK "+stockReturn.getSerial(),stockReturn.getSerial(),user.getUsername());
            return stockReturnItemRepository.save(StockReturnItem.builder()
                            .tokenUser(user.getUsername())
                            .quantity(requestStockReturnItem.getQuantity())
                            .supplierProduct(supplierProduct)
                            .supplierProductId(supplierProduct.getId())
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .stockReturn(stockReturn)
                            .stockReturnId(stockReturn.getId())
                            .observations(requestStockReturnItem.getObservations().toUpperCase())
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .status(true)
                    .build());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public CompletableFuture<StockReturnItem> saveAsync(StockReturn stockReturn,SupplierProduct supplierProduct, RequestStockReturnItem requestStockReturnItem, User user) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            try{
                iAudit.save("ADD_STOCK_RETURN_ITEM","PRODUCTO DE INVENTARIO "+requestStockReturnItem.getSupplierProduct().toUpperCase()+" AGREGADO A DEVOLUCION DE STOCK "+stockReturn.getSerial(),stockReturn.getSerial(),user.getUsername());
                return stockReturnItemRepository.save(StockReturnItem.builder()
                        .tokenUser(user.getUsername())
                        .quantity(requestStockReturnItem.getQuantity())
                        .supplierProduct(supplierProduct)
                        .supplierProductId(supplierProduct.getId())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .stockReturn(stockReturn)
                        .stockReturnId(stockReturn.getId())
                        .observations(requestStockReturnItem.getObservations().toUpperCase())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .status(true)
                        .build());
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
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<StockReturnItem> pageStockReturn;
            Long clientId;
            List<Long> supplierIds;

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
                    .supplierProduct(stockReturnItem.getSupplierProduct().getSerial())
                    .productSku(stockReturnItem.getSupplierProduct().getProduct().getSku())
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
    public CompletableFuture<List<StockReturnItemDTO>> listStockReturnItem(String user,Long id) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<StockReturnItem> stockReturnItems;
            Long clientId;
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
                    .supplierProduct(stockReturnItem.getSupplierProduct().getSerial())
                    .productSku(stockReturnItem.getSupplierProduct().getProduct().getSku())
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
    public CompletableFuture<List<StockReturnItemDTO>> listStockReturnItemFalse(String user,Long id) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<StockReturnItem> stockReturnItems;
            Long clientId;
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
                    .supplierProduct(stockReturnItem.getSupplierProduct().getSerial())
                    .productSku(stockReturnItem.getSupplierProduct().getProduct().getSku())
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