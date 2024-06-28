package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.StockReturnDTO;
import com.proyect.masterdata.dto.StockReturnItemDTO;
import com.proyect.masterdata.dto.request.RequestStockReturnItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
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
    private final PurchaseRepository purchaseRepository;
    private final PurchaseItemRepository purchaseItemRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final StockReturnItemRepository stockReturnItemRepository;
    private final StockReturnItemRepositoryCustom stockReturnItemRepositoryCustom;
    private final StockReturnRepository stockReturnRepository;
    private final SupplierRepository supplierRepository;
    @Override
    public StockReturnItem save(StockReturn stockReturn,PurchaseItem purchaseItem,RequestStockReturnItem requestStockReturnItem, User user) throws InternalErrorExceptions, BadRequestExceptions {

        try{
            return stockReturnItemRepository.save(StockReturnItem.builder()
                            .purchaseItem(purchaseItem)
                            .purchaseItemId(purchaseItem.getId())
                            .tokenUser(user.getUsername())
                            .quantity(requestStockReturnItem.getQuantity())
                            .supplierProduct(purchaseItem.getSupplierProduct())
                            .supplierProductId(purchaseItem.getSupplierProductId())
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .purchase(purchaseItem.getPurchase())
                            .purchaseId(purchaseItem.getPurchaseId())
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
    public CompletableFuture<StockReturnItem> saveAsync(StockReturn stockReturn, PurchaseItem purchaseItem, RequestStockReturnItem requestStockReturnItem, User user) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            try{
                return stockReturnItemRepository.save(StockReturnItem.builder()
                        .purchaseItem(purchaseItem)
                        .purchaseItemId(purchaseItem.getId())
                        .tokenUser(user.getUsername())
                        .quantity(requestStockReturnItem.getQuantity())
                        .supplierProduct(purchaseItem.getSupplierProduct())
                        .supplierProductId(purchaseItem.getSupplierProductId())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .purchase(purchaseItem.getPurchase())
                        .purchaseId(purchaseItem.getPurchaseId())
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
            List<String> stockReturns,
            List<String> purchases,
            List<String> suppliers,
            List<String> supplierProducts,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<StockReturnItem> pageStockReturn;
            Long clientId;
            List<Long> stockReturnIds;
            List<Long> purchaseIds;
            List<Long> supplierIds;
            List<Long> supplierProductIds;

            if(stockReturns != null && !stockReturns.isEmpty()){
                stockReturnIds = stockReturnRepository.findBySerialIn(
                        stockReturns.stream().map(String::toUpperCase).toList()
                ).stream().map(StockReturn::getId).toList();
            }else {
                stockReturnIds = new ArrayList<>();
            }

            if(purchases != null && !purchases.isEmpty()){
                purchaseIds = purchaseRepository.findBySerialIn(
                        purchases.stream().map(String::toUpperCase).toList()
                ).stream().map(Purchase::getId).toList();
            }else {
                purchaseIds = new ArrayList<>();
            }

            if(suppliers != null && !suppliers.isEmpty()){
                supplierIds = supplierRepository.findByRucIn(
                        suppliers.stream().map(String::toUpperCase).toList()
                ).stream().map(Supplier::getId).toList();
            }else{
                supplierIds = new ArrayList<>();
            }

            if(supplierProducts != null && !supplierProducts.isEmpty()){
                supplierProductIds = supplierProductRepository.findBySerialIn(
                        supplierProducts.stream().map(String::toUpperCase).toList()
                ).stream().map(SupplierProduct::getId).toList();
            }else {
                supplierProductIds = new ArrayList<>();
            }

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                pageStockReturn = stockReturnItemRepositoryCustom.searchForStockReturnItem(
                        clientId,
                        stockReturnIds,
                        purchaseIds,
                        supplierIds,
                        supplierProductIds,
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
                    .supplier(stockReturnItem.getStockReturn().getPurchase().getSupplier().getBusinessName())
                    .purchase(stockReturnItem.getPurchase().getSerial())
                    .supplierProduct(stockReturnItem.getSupplierProduct().getSerial())
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
                    .purchase(stockReturnItem.getPurchase().getSerial())
                    .supplierProduct(stockReturnItem.getSupplierProduct().getSerial())
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
                    .purchase(stockReturnItem.getPurchase().getSerial())
                    .supplierProduct(stockReturnItem.getSupplierProduct().getSerial())
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