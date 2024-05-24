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
    private final PurchaseRepository purchaseRepository;
    private final UserRepository userRepository;
    private final StockReturnRepository stockReturnRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final PurchaseItemRepository purchaseItemRepository;
    private final StockReturnRepositoryCustom stockReturnRepositoryCustom;
    private final IStockReturnItem iStockReturnItem;
    private final IStockTransaction iStockTransaction;
    private final ShipmentRepository shipmentRepository;
    private final IWarehouseStock iWarehouseStock;
    private final WarehouseRepository warehouseRepository;
    private final WarehouseStockRepository warehouseStockRepository;
    private final IGeneralStock iGeneralStock;
    @Override
    public ResponseSuccess save(RequestStockReturn requestStockReturn) throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        Purchase purchase;
        StockReturn stockReturn;
        Warehouse warehouse;
        Shipment shipment;

        try{
            user = userRepository.findByUsernameAndStatusTrue(requestStockReturn.getTokenUser().toUpperCase());
            purchase = purchaseRepository.findBySerial(requestStockReturn.getPurchaseSerial().toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }else {
            warehouse = warehouseRepository.findByClientIdAndNameAndStatusTrue(user.getClientId(), requestStockReturn.getWarehouse().toUpperCase());
        }

        if(purchase == null){
            throw new BadRequestExceptions(Constants.ErrorPurchase);
        }else{
            stockReturn = stockReturnRepository.findBySerial(requestStockReturn.getSerial());
            shipment = shipmentRepository.findByPurchaseIdAndShipmentTypeName(purchase.getId(), "EMBARQUE");
        }

        if(stockReturn != null){
            throw new BadRequestExceptions(Constants.ErrorStockReturnExists);
        }

        if(shipment == null){
            throw new BadRequestExceptions(Constants.ErrorShipment);
        }

        try {
            for(RequestStockReturnItem requestStockReturnItem : requestStockReturn.getRequestStockReturnItemList()){
                SupplierProduct supplierProduct = supplierProductRepository.findBySerial(requestStockReturnItem.getSupplierProductSerial());
                PurchaseItem purchaseItem = purchaseItemRepository.findByPurchaseIdAndSupplierProductId(purchase.getId(),supplierProduct.getId());
                WarehouseStock warehouseStock = warehouseStockRepository.findByWarehouseIdAndSupplierProductId(warehouse.getId(), supplierProduct.getId());
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
                iStockReturnItem.save(newStockReturn,purchaseItem,requestStockReturnItem,user);
                iWarehouseStock.out(warehouse,supplierProduct,requestStockReturnItem.getQuantity(),user);
                iGeneralStock.out(supplierProduct.getSerial(),requestStockReturnItem.getQuantity(),user.getUsername());
                requestStockTransactionItemList.add(RequestStockTransactionItem.builder()
                                .supplierProductSerial(supplierProduct.getSerial())
                                .quantity(requestStockReturnItem.getQuantity())
                        .build());
            }

            iStockTransaction.save("SR"+newStockReturn.getId(),shipment.getWarehouse(),requestStockTransactionItemList,"DEVOLUCION-PROVEEDOR",user);

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
            Purchase purchase;
            StockReturn stockReturn;
            Warehouse warehouse;
            Shipment shipment;

            try{
                user = userRepository.findByUsernameAndStatusTrue(requestStockReturn.getTokenUser().toUpperCase());
                purchase = purchaseRepository.findBySerial(requestStockReturn.getPurchaseSerial().toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else {
                warehouse = warehouseRepository.findByClientIdAndNameAndStatusTrue(user.getClientId(), requestStockReturn.getWarehouse().toUpperCase());
            }

            if(purchase == null){
                throw new BadRequestExceptions(Constants.ErrorPurchase);
            }else{
                stockReturn = stockReturnRepository.findBySerial(requestStockReturn.getSerial());
                shipment = shipmentRepository.findByPurchaseIdAndShipmentTypeName(purchase.getId(), "EMBARQUE");
            }

            if(stockReturn != null){
                throw new BadRequestExceptions(Constants.ErrorStockReturnExists);
            }

            if(shipment == null){
                throw new BadRequestExceptions(Constants.ErrorShipment);
            }

            try {
                for(RequestStockReturnItem requestStockReturnItem : requestStockReturn.getRequestStockReturnItemList()){
                    SupplierProduct supplierProduct = supplierProductRepository.findBySerial(requestStockReturnItem.getSupplierProductSerial());
                    PurchaseItem purchaseItem = purchaseItemRepository.findByPurchaseIdAndSupplierProductId(purchase.getId(),supplierProduct.getId());
                    WarehouseStock warehouseStock = warehouseStockRepository.findByWarehouseIdAndSupplierProductId(warehouse.getId(), supplierProduct.getId());
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
                    iStockReturnItem.save(newStockReturn,purchaseItem,requestStockReturnItem,user);
                    iWarehouseStock.out(warehouse,supplierProduct,requestStockReturnItem.getQuantity(),user);
                    iGeneralStock.out(supplierProduct.getSerial(),requestStockReturnItem.getQuantity(),user.getUsername());
                    requestStockTransactionItemList.add(RequestStockTransactionItem.builder()
                            .supplierProductSerial(supplierProduct.getSerial())
                            .quantity(requestStockReturnItem.getQuantity())
                            .build());
                }

                iStockTransaction.save("SR"+newStockReturn.getId(),shipment.getWarehouse(),requestStockTransactionItemList,"DEVOLUCION-PROVEEDOR",user);

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
    public CompletableFuture<Page<StockReturnDTO>> list(String purchaseSerial, String user, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<StockReturn> pageStockReturn;
            Long clientId;
            Long purchaseId;

            if(purchaseSerial != null){
                purchaseId = purchaseRepository.findBySerial(purchaseSerial.toUpperCase()).getId();
            }else {
                purchaseId = null;
            }

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                pageStockReturn = stockReturnRepositoryCustom.searchForStockReturnItem(purchaseId,clientId,sort,sortColumn,pageNumber,pageSize,true);
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
                    .purchaseSerial(stockReturn.getPurchase().getSerial())
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
                    .purchaseSerial(stockReturn.getPurchase().getSerial())
                    .updateDate(stockReturn.getUpdateDate())
                    .id(stockReturn.getId())
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
                    .purchaseSerial(stockReturn.getPurchase().getSerial())
                    .updateDate(stockReturn.getUpdateDate())
                    .id(stockReturn.getId())
                    .supplier(stockReturn.getPurchase().getSupplier().getBusinessName())
                    .build()).toList();
        });
    }
}
