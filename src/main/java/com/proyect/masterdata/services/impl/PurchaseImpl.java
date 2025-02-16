package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.CheckStockDTO;
import com.proyect.masterdata.dto.PurchaseDTO;
import com.proyect.masterdata.dto.request.RequestPurchase;
import com.proyect.masterdata.dto.request.RequestPurchaseItem;
import com.proyect.masterdata.dto.request.RequestStockTransactionItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.*;
import com.proyect.masterdata.utils.Constants;
import jakarta.transaction.Transactional;
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
public class PurchaseImpl implements IPurchase {

    private final UserRepository userRepository;
    private final PurchaseRepository purchaseRepository;
    private final WarehouseRepository warehouseRepository;
    private final IStockTransaction iStockTransaction;
    private final IPurchaseItem iPurchaseItem;
    private final IWarehouseStock iWarehouseStock;
    private final IGeneralStock iGeneralStock;
    private final PurchaseTypeRepository purchaseTypeRepository;
    private final PurchaseRepositoryCustom purchaseRepositoryCustom;
    private final SupplierProductRepository supplierProductRepository;
    private final StockReturnRepository stockReturnRepository;
    private final PurchaseDocumentRepository purchaseDocumentRepository;
    private final IAudit iAudit;
    private final SupplierRepository supplierRepository;
    private final ProductRepository productRepository;
    private final WarehouseStockRepository warehouseStockRepository;
    @Override
    @Transactional
    public ResponseSuccess save(RequestPurchase requestPurchase, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {

        User user;
        Warehouse warehouse;
        Purchase purchase;
        PurchaseType purchaseType;
        StockReturn stockReturn;
        PurchaseDocument purchaseDocument;
        Supplier supplier;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            warehouse = warehouseRepository.findByNameAndStatusTrue(requestPurchase.getWarehouse().toUpperCase());
            purchaseType = purchaseTypeRepository.findByNameAndStatusTrue(requestPurchase.getPurchaseType().toUpperCase());
            purchaseDocument = purchaseDocumentRepository.findByNameAndStatusTrue(requestPurchase.getPurchaseDocument());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }else{
            supplier = supplierRepository.findByRucAndClientIdAndStatusTrue(requestPurchase.getSupplier(), user.getClientId());
        }

        if (warehouse == null) {
            throw new BadRequestExceptions(Constants.ErrorWarehouse);
        }

        if (!Objects.equals(warehouse.getClientId(), user.getClientId())) {
            throw new BadRequestExceptions(Constants.ErrorWarehouse);
        }

        if(purchaseType == null){
            throw new BadRequestExceptions(Constants.ErrorPurchaseType);
        }else{
            purchase = purchaseRepository.findBySerialAndPurchaseTypeId(requestPurchase.getSerial(), purchaseType.getId());
        }

        if (purchase != null) {
            throw new BadRequestExceptions(Constants.ErrorPurchaseExists);
        }

        if(purchaseDocument == null){
            throw new BadRequestExceptions(Constants.ErrorPurchaseDocument);
        }

        if(supplier==null){
            throw new BadRequestExceptions(Constants.ErrorSupplier);
        }

        try{
            if(Objects.equals(purchaseType.getName(), "DEVOLUCION")){
                stockReturn = stockReturnRepository.findBySerial(requestPurchase.getSerial());
                if(stockReturn == null){
                    throw new BadRequestExceptions(Constants.ErrorPurchaseReturn);
                }
            }

            for(RequestPurchaseItem requestPurchaseItem : requestPurchase.getRequestPurchaseItemList()){
                SupplierProduct supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(requestPurchaseItem.getSupplierProduct());
                if(supplierProduct == null){
                    throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
                }
                if(requestPurchaseItem.getQuantity() < 1){
                    throw new BadRequestExceptions(Constants.ErrorPurchaseItemZero);
                }
            }
            List<RequestStockTransactionItem> requestStockTransactionItemList = requestPurchase.getRequestPurchaseItemList().stream().map(purchaseItem -> RequestStockTransactionItem.builder()
                    .quantity(purchaseItem.getQuantity())
                    .supplierProductSerial(purchaseItem.getSupplierProduct().toUpperCase())
                    .build()).toList();
            StockTransaction newStockTransaction = iStockTransaction.save("S"+ requestPurchase.getSerial().toUpperCase(), warehouse,requestStockTransactionItemList,"COMPRA",user);
            Purchase newPurchase = purchaseRepository.save(com.proyect.masterdata.domain.Purchase.builder()
                            .serial(requestPurchase.getSerial().toUpperCase())
                            .supplier(supplier)
                            .supplierId(supplier.getId())
                            .status(true)
                            .registrationDate(OffsetDateTime.now())
                            .updateDate(OffsetDateTime.now())
                            .warehouse(warehouse)
                            .warehouseId(warehouse.getId())
                            .purchaseType(purchaseType)
                            .purchaseTypeId(purchaseType.getId())
                            .client(user.getClient())
                            .purchaseDocument(purchaseDocument)
                            .purchaseDocumentId(purchaseDocument.getId())
                            .clientId(user.getClientId())
                            .user(user).userId(user.getId())
                      .build());
            for(RequestPurchaseItem requestPurchaseItem : requestPurchase.getRequestPurchaseItemList()){
                    SupplierProduct supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(requestPurchaseItem.getSupplierProduct());
                  iPurchaseItem.save(newPurchase,warehouse.getName(), requestPurchaseItem,user.getUsername());
                  iWarehouseStock.in(warehouse,supplierProduct, requestPurchaseItem.getQuantity(),user);
                  iGeneralStock.in(requestPurchaseItem.getSupplierProduct(), requestPurchaseItem.getQuantity(),user.getUsername());
            }
            iAudit.save("ADD_PURCHASE","COMPRA " + newPurchase.getSerial() +" CREADA.",newPurchase.getSerial(),user.getUsername());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        }catch (RuntimeException e){
            e.printStackTrace();
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    @Transactional
    public CompletableFuture<ResponseSuccess> saveAsync(RequestPurchase requestPurchase, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Warehouse warehouse;
            Purchase purchase;
            PurchaseType purchaseType;
            StockReturn stockReturn;
            PurchaseDocument purchaseDocument;
            Supplier supplier;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                warehouse = warehouseRepository.findByNameAndStatusTrue(requestPurchase.getWarehouse().toUpperCase());
                purchaseType = purchaseTypeRepository.findByNameAndStatusTrue(requestPurchase.getPurchaseType().toUpperCase());
                purchaseDocument = purchaseDocumentRepository.findByNameAndStatusTrue(requestPurchase.getPurchaseDocument());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else{
                supplier = supplierRepository.findByRucAndClientIdAndStatusTrue(requestPurchase.getSupplier(), user.getClientId());
            }

            if (warehouse == null) {
                throw new BadRequestExceptions(Constants.ErrorWarehouse);
            }

            if (!Objects.equals(warehouse.getClientId(), user.getClientId())) {
                throw new BadRequestExceptions(Constants.ErrorWarehouse);
            }

            if(purchaseType == null){
                throw new BadRequestExceptions(Constants.ErrorPurchaseType);
            }else{
                purchase = purchaseRepository.findBySerialAndPurchaseTypeId(requestPurchase.getSerial(), purchaseType.getId());
            }

            if (purchase != null) {
                throw new BadRequestExceptions(Constants.ErrorPurchaseExists);
            }

            if(purchaseDocument == null){
                throw new BadRequestExceptions(Constants.ErrorPurchaseDocument);
            }

            if(supplier == null){
                throw new BadRequestExceptions(Constants.ErrorSupplier);
            }

            try{
                if(Objects.equals(purchaseType.getName(), "DEVOLUCION")){
                    stockReturn = stockReturnRepository.findBySerial(requestPurchase.getSerial());
                    if(stockReturn == null){
                        throw new BadRequestExceptions(Constants.ErrorPurchaseReturn);
                    }
                }

                for(RequestPurchaseItem requestPurchaseItem : requestPurchase.getRequestPurchaseItemList()){
                    SupplierProduct supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(requestPurchaseItem.getSupplierProduct());
                    if(supplierProduct == null){
                        throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
                    }
                    if(requestPurchaseItem.getQuantity() < 1){
                        throw new BadRequestExceptions(Constants.ErrorPurchaseItemZero);
                    }
                }
                List<RequestStockTransactionItem> requestStockTransactionItemList = requestPurchase.getRequestPurchaseItemList().stream().map(purchaseItem -> RequestStockTransactionItem.builder()
                        .quantity(purchaseItem.getQuantity())
                        .supplierProductSerial(purchaseItem.getSupplierProduct().toUpperCase())
                        .build()).toList();
                StockTransaction newStockTransaction = iStockTransaction.save("S"+ requestPurchase.getSerial().toUpperCase(), warehouse,requestStockTransactionItemList,"COMPRA",user);
                Purchase newPurchase = purchaseRepository.save(com.proyect.masterdata.domain.Purchase.builder()
                        .serial(requestPurchase.getSerial().toUpperCase())
                        .status(true)
                        .supplier(supplier)
                        .supplierId(supplier.getId())
                        .registrationDate(OffsetDateTime.now())
                        .updateDate(OffsetDateTime.now())
                        .warehouse(warehouse)
                        .warehouseId(warehouse.getId())
                        .purchaseType(purchaseType)
                        .purchaseTypeId(purchaseType.getId())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                                .purchaseDocument(purchaseDocument)
                                .purchaseDocumentId(purchaseDocument.getId())
                        .user(user).userId(user.getId())
                        .build());
                for(RequestPurchaseItem requestPurchaseItem : requestPurchase.getRequestPurchaseItemList()){
                    SupplierProduct supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(requestPurchaseItem.getSupplierProduct());
                    iPurchaseItem.save(newPurchase,warehouse.getName(), requestPurchaseItem,user.getUsername());
                    iWarehouseStock.in(warehouse,supplierProduct, requestPurchaseItem.getQuantity(),user);
                    iGeneralStock.in(requestPurchaseItem.getSupplierProduct(), requestPurchaseItem.getQuantity(),user.getUsername());
                }
                iAudit.save("ADD_PURCHASE","COMPRA " + newPurchase.getSerial() +" CREADA.",newPurchase.getSerial(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            }catch (RuntimeException e){
                e.printStackTrace();
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<PurchaseDTO>> list(
            List<String> serials,
            String user,
            List<String> warehouses,
            List<String> purchaseTypes,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<Purchase> pagePurchase;
            List<String> serialsUppercase;
            UUID clientId;
            List<UUID> warehouseIds;
            List<UUID> purchaseTypeIds;

            if(serials != null && !serials.isEmpty()){
                serialsUppercase = serials.stream().map(String::toUpperCase).toList();
            }else{
                serialsUppercase = new ArrayList<>();
            }

            if(warehouses != null && !warehouses.isEmpty()){
                warehouseIds = warehouseRepository.findByNameIn(
                        warehouses.stream().map(String::toUpperCase).toList()
                ).stream().map(Warehouse::getId).toList();
            }else{
                warehouseIds = new ArrayList<>();
            }

            if (purchaseTypes != null && !purchaseTypes.isEmpty()){
                purchaseTypeIds = purchaseTypeRepository.findByNameIn(
                        purchaseTypes.stream().map(String::toUpperCase).toList()
                ).stream().map(PurchaseType::getId).toList();
            }else {
                purchaseTypeIds = new ArrayList<>();
            }

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                pagePurchase = purchaseRepositoryCustom.searchForPurchase(
                        clientId,
                        serialsUppercase,
                        warehouseIds,
                        purchaseTypeIds,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        true);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.ResultsFound);
            }

            if(pagePurchase.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }

            List<PurchaseDTO> purchaseDTOS = pagePurchase.getContent().stream().map(purchase -> PurchaseDTO.builder()
                    .serial(purchase.getSerial())
                    .purchaseDocument(purchase.getPurchaseDocument().getName())
                    .warehouse(purchase.getWarehouse().getName())
                    .purchaseType(purchase.getPurchaseType().getName())
                    .registrationDate(purchase.getRegistrationDate())
                    .build()).toList();

            return new PageImpl<>(purchaseDTOS,pagePurchase.getPageable(),pagePurchase.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<Page<PurchaseDTO>> listFalse(
            List<String> serials,
            String user,
            List<String> warehouses,
            List<String> purchaseTypes,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<Purchase> pagePurchase;
            UUID clientId;
            List<String> serialsUppercase;
            List<UUID> warehouseIds;
            List<UUID> purchaseTypeIds;

            if(serials != null && !serials.isEmpty()){
                serialsUppercase = serials.stream().map(String::toUpperCase).toList();
            }else{
                serialsUppercase = new ArrayList<>();
            }

            if(warehouses != null && !warehouses.isEmpty()){
                warehouseIds = warehouseRepository.findByNameIn(
                        warehouses.stream().map(String::toUpperCase).toList()
                ).stream().map(Warehouse::getId).toList();
            }else{
                warehouseIds = new ArrayList<>();
            }

            if (purchaseTypes != null && !purchaseTypes.isEmpty()){
                purchaseTypeIds = purchaseTypeRepository.findByNameIn(
                        purchaseTypes.stream().map(String::toUpperCase).toList()
                ).stream().map(PurchaseType::getId).toList();
            }else {
                purchaseTypeIds = new ArrayList<>();
            }

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                pagePurchase = purchaseRepositoryCustom.searchForPurchase(
                        clientId,
                        serialsUppercase,
                        warehouseIds,
                        purchaseTypeIds,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        false);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.ResultsFound);
            }

            if(pagePurchase.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }

            List<PurchaseDTO> purchaseDTOS = pagePurchase.getContent().stream().map(purchase -> PurchaseDTO.builder()
                    .serial(purchase.getSerial())
                    .purchaseDocument(purchase.getPurchaseDocument().getName())
                    .warehouse(purchase.getWarehouse().getName())
                    .purchaseType(purchase.getPurchaseType().getName())
                    .registrationDate(purchase.getRegistrationDate())
                    .build()).toList();

            return new PageImpl<>(purchaseDTOS,pagePurchase.getPageable(),pagePurchase.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<PurchaseDTO>> listPurchase(String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Purchase> purchases;
            UUID clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                purchases = purchaseRepository.findAllByClientId(clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(purchases.isEmpty()){
                return Collections.emptyList();
            }

            return purchases.stream().map(purchase -> PurchaseDTO.builder()
                    .serial(purchase.getSerial())
                    .purchaseDocument(purchase.getPurchaseDocument().getName())
                    .warehouse(purchase.getWarehouse().getName())
                    .purchaseType(purchase.getPurchaseType().getName())
                    .registrationDate(purchase.getRegistrationDate())
                    .build()).toList();
        });
    }

    @Override
    public CompletableFuture<List<PurchaseDTO>> listFilter(String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Purchase> purchases;
            UUID clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                purchases = purchaseRepository.findAllByClientId(clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(purchases.isEmpty()){
                return Collections.emptyList();
            }

            return purchases.stream().map(purchase -> PurchaseDTO.builder()
                    .serial(purchase.getSerial())
                    .purchaseDocument(purchase.getPurchaseDocument().getName())
                    .warehouse(purchase.getWarehouse().getName())
                    .purchaseType(purchase.getPurchaseType().getName())
                    .registrationDate(purchase.getRegistrationDate())
                    .build()).toList();
        });
    }

    @Override
    public CompletableFuture<List<CheckStockDTO>> checkStock(String serial, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<WarehouseStock> warehouseStocks;
            SupplierProduct supplierProduct;
            User user;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(serial.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(supplierProduct == null){
                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
            }else{
                warehouseStocks = warehouseStockRepository.findAllBySupplierProductId(supplierProduct.getId());
            }

            try {
                return warehouseStocks.stream().map(warehouseStock -> CheckStockDTO.builder()
                        .warehouse(warehouseStock.getWarehouse().getName())
                        .quantity(warehouseStock.getQuantity())
                        .build()
                ).toList();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

}
