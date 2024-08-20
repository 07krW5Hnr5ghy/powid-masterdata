package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.StockReturnDTO;
import com.proyect.masterdata.dto.request.RequestStockReturn;
import com.proyect.masterdata.dto.request.RequestStockReturnItem;
import com.proyect.masterdata.dto.request.RequestStockTransactionItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.*;
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
public class StockReturnImpl implements IStockReturn {
    private final UserRepository userRepository;
    private final StockReturnRepository stockReturnRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final StockReturnRepositoryCustom stockReturnRepositoryCustom;
    private final IStockReturnItem iStockReturnItem;
    private final IStockTransaction iStockTransaction;
    private final PurchaseRepository purchaseRepository;
    private final IWarehouseStock iWarehouseStock;
    private final WarehouseRepository warehouseRepository;
    private final WarehouseStockRepository warehouseStockRepository;
    private final IGeneralStock iGeneralStock;
    private final IAudit iAudit;
    private final SupplierRepository supplierRepository;
    private final PurchaseItemRepository purchaseItemRepository;
    @Override
    public ResponseSuccess save(RequestStockReturn requestStockReturn) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        StockReturn stockReturn;
        Warehouse warehouse;
        Purchase purchase;

        try{
            user = userRepository.findByUsernameAndStatusTrue(requestStockReturn.getTokenUser().toUpperCase());
            stockReturn = stockReturnRepository.findBySerial(requestStockReturn.getSerial());
            purchase = purchaseRepository.findByPurchaseTypeNameAndSerial("COMPRA", requestStockReturn.getPurchaseSerial());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }else {
            warehouse = warehouseRepository.findByClientIdAndNameAndStatusTrue(user.getClientId(), requestStockReturn.getWarehouse().toUpperCase());
        }

        if(stockReturn != null){
            throw new BadRequestExceptions(Constants.ErrorStockReturnExists);
        }

        if(purchase == null){
            throw new BadRequestExceptions(Constants.ErrorPurchase);
        }

        try {
            for(RequestStockReturnItem requestStockReturnItem : requestStockReturn.getRequestStockReturnItemList()){
                SupplierProduct supplierProduct = supplierProductRepository.findBySerial(requestStockReturnItem.getSupplierProductSerial());
                if(supplierProduct == null){
                    throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
                }
                PurchaseItem purchaseItem = purchaseItemRepository.findByPurchaseIdAndSupplierProductId(purchase.getId(),supplierProduct.getId());
                if(purchaseItem == null){
                    throw new BadRequestExceptions(Constants.ErrorPurchaseItem);
                }
                WarehouseStock warehouseStock = warehouseStockRepository.findByWarehouseIdAndSupplierProductId(warehouse.getId(), supplierProduct.getId());
                if(requestStockReturnItem.getQuantity()<1){
                    throw new BadRequestExceptions(Constants.ErrorStockReturnItemZero);
                }
                if(requestStockReturnItem.getQuantity() > purchaseItem.getQuantity()){
                    throw new BadRequestExceptions(Constants.ErrorStockReturnQuantity);
                }
                if(requestStockReturnItem.getQuantity() > warehouseStock.getQuantity()){
                    throw new BadRequestExceptions(Constants.ErrorStockReturnWarehouseQuantity);
                }
            }
            StockReturn newStockReturn = stockReturnRepository.save(StockReturn.builder()
                            .serial(requestStockReturn.getSerial().toUpperCase())
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .updateDate(new Date(System.currentTimeMillis()))
                            .purchase(purchase)
                            .purchaseId(purchase.getId())
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .tokenUser(user.getUsername())
                            .status(true)
                    .build());
            List<RequestStockTransactionItem> requestStockTransactionItemList = new ArrayList<>();

            for(RequestStockReturnItem requestStockReturnItem : requestStockReturn.getRequestStockReturnItemList()){
                SupplierProduct supplierProduct = supplierProductRepository.findBySerial(requestStockReturnItem.getSupplierProductSerial());
                PurchaseItem purchaseItem = purchaseItemRepository.findByPurchaseIdAndSupplierProductId(purchase.getId(),supplierProduct.getId());
                iStockReturnItem.save(newStockReturn, purchaseItem,requestStockReturnItem,user);
                iWarehouseStock.out(warehouse,supplierProduct,requestStockReturnItem.getQuantity(),user);
                iGeneralStock.out(supplierProduct.getSerial(),requestStockReturnItem.getQuantity(),user.getUsername());
                requestStockTransactionItemList.add(RequestStockTransactionItem.builder()
                                .supplierProductSerial(supplierProduct.getSerial())
                                .quantity(requestStockReturnItem.getQuantity())
                        .build());
            }

            iStockTransaction.save("SR"+newStockReturn.getId(), purchase.getWarehouse(),requestStockTransactionItemList,"DEVOLUCION-PROVEEDOR",user);
            iAudit.save("ADD_STOCK_RETURN","DEVOLUCION DE STOCK "+newStockReturn.getSerial()+" CREADA.",newStockReturn.getSerial(),user.getUsername());
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
    public CompletableFuture<ResponseSuccess> saveAsync(RequestStockReturn requestStockReturn) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            StockReturn stockReturn;
            Warehouse warehouse;
            Purchase purchase;

            try{
                user = userRepository.findByUsernameAndStatusTrue(requestStockReturn.getTokenUser().toUpperCase());
                stockReturn = stockReturnRepository.findBySerial(requestStockReturn.getSerial());
                purchase = purchaseRepository.findByPurchaseTypeNameAndSerial("EMBARQUE", requestStockReturn.getSerial());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else {
                warehouse = warehouseRepository.findByClientIdAndNameAndStatusTrue(user.getClientId(), requestStockReturn.getWarehouse().toUpperCase());
            }

            if(stockReturn != null){
                throw new BadRequestExceptions(Constants.ErrorStockReturnExists);
            }

            if(purchase == null){
                throw new BadRequestExceptions(Constants.ErrorPurchase);
            }

            try {
                for(RequestStockReturnItem requestStockReturnItem : requestStockReturn.getRequestStockReturnItemList()){
                    SupplierProduct supplierProduct = supplierProductRepository.findBySerial(requestStockReturnItem.getSupplierProductSerial());
                    if(supplierProduct == null){
                        throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
                    }
                    PurchaseItem purchaseItem = purchaseItemRepository.findByPurchaseIdAndSupplierProductId(purchase.getId(),supplierProduct.getId());
                    if(purchaseItem == null){
                        throw new BadRequestExceptions(Constants.ErrorPurchaseItem);
                    }
                    WarehouseStock warehouseStock = warehouseStockRepository.findByWarehouseIdAndSupplierProductId(warehouse.getId(), supplierProduct.getId());
                    if(requestStockReturnItem.getQuantity()<1){
                        throw new BadRequestExceptions(Constants.ErrorStockReturnItemZero);
                    }
                    if(requestStockReturnItem.getQuantity() > purchaseItem.getQuantity()){
                        throw new BadRequestExceptions(Constants.ErrorStockReturnQuantity);
                    }
                    if(requestStockReturnItem.getQuantity() > warehouseStock.getQuantity()){
                        throw new BadRequestExceptions(Constants.ErrorStockReturnWarehouseQuantity);
                    }
                }
                StockReturn newStockReturn = stockReturnRepository.save(StockReturn.builder()
                        .serial(requestStockReturn.getSerial().toUpperCase())
                                .purchase(purchase)
                                .purchaseId(purchase.getId())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .updateDate(new Date(System.currentTimeMillis()))
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .tokenUser(user.getUsername())
                        .status(true)
                        .build());
                List<RequestStockTransactionItem> requestStockTransactionItemList = new ArrayList<>();

                for(RequestStockReturnItem requestStockReturnItem : requestStockReturn.getRequestStockReturnItemList()){
                    SupplierProduct supplierProduct = supplierProductRepository.findBySerial(requestStockReturnItem.getSupplierProductSerial());
                    PurchaseItem purchaseItem = purchaseItemRepository.findByPurchaseIdAndSupplierProductId(purchase.getId(),supplierProduct.getId());
                    iStockReturnItem.save(newStockReturn, purchaseItem,requestStockReturnItem,user);
                    iWarehouseStock.out(warehouse,supplierProduct,requestStockReturnItem.getQuantity(),user);
                    iGeneralStock.out(supplierProduct.getSerial(),requestStockReturnItem.getQuantity(),user.getUsername());
                    requestStockTransactionItemList.add(RequestStockTransactionItem.builder()
                            .supplierProductSerial(supplierProduct.getSerial())
                            .quantity(requestStockReturnItem.getQuantity())
                            .build());
                }

                iStockTransaction.save("SR"+newStockReturn.getId(), purchase.getWarehouse(),requestStockTransactionItemList,"DEVOLUCION-PROVEEDOR",user);
                iAudit.save("ADD_STOCK_RETURN","DEVOLUCION DE STOCK "+newStockReturn.getSerial()+" CREADA.",newStockReturn.getSerial(),user.getUsername());
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
    public CompletableFuture<Page<StockReturnDTO>> list(
            String user,
            List<String> serials,
            List<String> purchases,
            List<String> suppliers,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<StockReturn> pageStockReturn;
            Long clientId;
            List<String> serialsUppercase;
            List<Long> purchaseIds;
            List<Long> supplierIds;

            if(serials != null && !serials.isEmpty()){
                serialsUppercase = serials.stream().map(String::toUpperCase).toList();
            }else {
                serialsUppercase = new ArrayList<>();
            }

            if(purchases != null && !purchases.isEmpty()){
                purchaseIds = purchaseRepository.findBySerialIn(
                        purchases.stream().map(String::toUpperCase).toList()
                ).stream().map(com.proyect.masterdata.domain.Purchase::getId).toList();
            }else{
                purchaseIds = new ArrayList<>();
            }

            if(suppliers != null && !suppliers.isEmpty()){
                supplierIds = supplierRepository.findByRucIn(
                        suppliers.stream().map(String::toUpperCase).toList()
                ).stream().map(Supplier::getId).toList();
            }else{
                supplierIds = new ArrayList<>();
            }

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                pageStockReturn = stockReturnRepositoryCustom.searchForStockReturnItem(
                        clientId,
                        serialsUppercase,
                        purchaseIds,
                        supplierIds,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        true);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if(pageStockReturn.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }

            List<StockReturnDTO> stockReturnDTOS = pageStockReturn.getContent().stream().map(stockReturn -> StockReturnDTO.builder()
                    .registrationDate(stockReturn.getRegistrationDate())
                    .serial(stockReturn.getSerial())
                    .supplier(stockReturn.getPurchase().getSupplier().getBusinessName())
                    .purchase(stockReturn.getPurchase().getSerial())
                    .updateDate(stockReturn.getUpdateDate())
                    .build()).toList();
            return new PageImpl<>(stockReturnDTOS,pageStockReturn.getPageable(),pageStockReturn.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<StockReturnDTO>> listStockReturn(String user) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<StockReturn> stockReturns;
            Long clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                stockReturns = stockReturnRepository.findAllByClientIdAndStatusTrue(clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(stockReturns.isEmpty()){
                return Collections.emptyList();
            }

            return stockReturns.stream().map(stockReturn -> StockReturnDTO.builder()
                    .registrationDate(stockReturn.getRegistrationDate())
                    .serial(stockReturn.getSerial())
                    .purchase(stockReturn.getPurchase().getSerial())
                    .updateDate(stockReturn.getUpdateDate())
                    .supplier(stockReturn.getPurchase().getSupplier().getBusinessName())
                    .build()).toList();
        });
    }

    @Override
    public CompletableFuture<List<StockReturnDTO>> listStockReturnFalse(String user) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<StockReturn> stockReturns;
            Long clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                stockReturns = stockReturnRepository.findAllByClientIdAndStatusFalse(clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(stockReturns.isEmpty()){
                return Collections.emptyList();
            }

            return stockReturns.stream().map(stockReturn -> StockReturnDTO.builder()
                    .registrationDate(stockReturn.getRegistrationDate())
                    .serial(stockReturn.getSerial())
                    .purchase(stockReturn.getPurchase().getSerial())
                    .updateDate(stockReturn.getUpdateDate())
                    .supplier(stockReturn.getPurchase().getSupplier().getBusinessName())
                    .build()).toList();
        });
    }

    @Override
    public CompletableFuture<List<StockReturnDTO>> listFilter(String user) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<StockReturn> stockReturns;
            Long clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                stockReturns = stockReturnRepository.findAllByClientId(clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(stockReturns.isEmpty()){
                return Collections.emptyList();
            }

            return stockReturns.stream().map(stockReturn -> StockReturnDTO.builder()
                    .registrationDate(stockReturn.getRegistrationDate())
                    .serial(stockReturn.getSerial())
                    .purchase(stockReturn.getPurchase().getSerial())
                    .updateDate(stockReturn.getUpdateDate())
                    .supplier(stockReturn.getPurchase().getSupplier().getBusinessName())
                    .build()).toList();
        });
    }
}
