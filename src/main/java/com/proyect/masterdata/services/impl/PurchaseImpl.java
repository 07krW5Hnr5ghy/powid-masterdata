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
    private final PurchaseDocumentRepository purchaseDocumentRepository;
    private final IAudit iAudit;
    private final ProductRepository productRepository;
    private final WarehouseStockRepository warehouseStockRepository;
    private final IUtil iUtil;
    private final PurchasePaymentTypeRepository purchasePaymentTypeRepository;
    @Override
    @Transactional
    public ResponseSuccess save(RequestPurchase requestPurchase, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {

        User user;
        Warehouse warehouse;
        Purchase purchase;
        PurchaseType purchaseType;
        PurchaseDocument purchaseDocument;
        PurchasePaymentType purchasePaymentType;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            warehouse = warehouseRepository.findByNameAndStatusTrue(requestPurchase.getWarehouse().toUpperCase());
            purchaseType = purchaseTypeRepository.findByNameAndStatusTrue(requestPurchase.getPurchaseType().toUpperCase());
            purchaseDocument = purchaseDocumentRepository.findByNameAndStatusTrue(requestPurchase.getPurchaseDocument());
            purchasePaymentType = purchasePaymentTypeRepository.findByNameAndStatusTrue(requestPurchase.getPurchasePaymentType().toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
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
            purchase = purchaseRepository.findByRefAndPurchaseTypeId(requestPurchase.getRef(), purchaseType.getId());
        }

        if (purchase != null) {
            throw new BadRequestExceptions(Constants.ErrorPurchaseExists);
        }

        if(purchaseDocument == null){
            throw new BadRequestExceptions(Constants.ErrorPurchaseDocument);
        }

        if(purchasePaymentType==null){
            throw new BadRequestExceptions(Constants.ErrorPurchasePaymentType);
        }

        try{

            List<RequestStockTransactionItem> requestStockTransactionItemList = requestPurchase.getRequestPurchaseItemList().stream().map(purchaseItem -> RequestStockTransactionItem.builder()
                    .quantity(purchaseItem.getQuantity())
                    .productId(purchaseItem.getProductId())
                    .build()).toList();
            StockTransaction newStockTransaction = iStockTransaction.save("S"+ requestPurchase.getRef().toUpperCase(), warehouse,requestStockTransactionItemList,"COMPRA",user);
            Long purchaseNumber = purchaseRepository.countByClientId(user.getClientId())+1L;
            Purchase newPurchase = purchaseRepository.save(Purchase.builder()
                            .ref(requestPurchase.getRef().toUpperCase())
                            .purchaseNumber(purchaseNumber)
                            .status(true)
                            .registrationDate(OffsetDateTime.now())
                            .updateDate(OffsetDateTime.now())
                            .warehouse(warehouse)
                            .warehouseId(warehouse.getId())
                            .purchaseType(purchaseType)
                            .purchaseTypeId(purchaseType.getId())
                            .client(user.getClient())
                            .purchasePaymentType(purchasePaymentType)
                            .purchasePaymentTypeId(purchasePaymentType.getId())
                            .purchaseDocument(purchaseDocument)
                            .purchaseDocumentId(purchaseDocument.getId())
                            .clientId(user.getClientId())
                            .user(user).userId(user.getId())
                            .deliveryDate(requestPurchase.getDeliveryDate())
                      .build());
            iAudit.save("ADD_PURCHASE","COMPRA " + newPurchase.getRef() +" CREADA.",newPurchase.getRef(),user.getUsername());
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
            PurchaseDocument purchaseDocument;
            PurchasePaymentType purchasePaymentType;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                warehouse = warehouseRepository.findByNameAndStatusTrue(requestPurchase.getWarehouse().toUpperCase());
                purchaseType = purchaseTypeRepository.findByNameAndStatusTrue(requestPurchase.getPurchaseType().toUpperCase());
                purchaseDocument = purchaseDocumentRepository.findByNameAndStatusTrue(requestPurchase.getPurchaseDocument());
                purchasePaymentType = purchasePaymentTypeRepository.findByNameAndStatusTrue(requestPurchase.getPurchasePaymentType().toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
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
                purchase = purchaseRepository.findByRefAndPurchaseTypeId(requestPurchase.getRef(), purchaseType.getId());
            }

            if (purchase != null) {
                throw new BadRequestExceptions(Constants.ErrorPurchaseExists);
            }

            if(purchaseDocument == null){
                throw new BadRequestExceptions(Constants.ErrorPurchaseDocument);
            }

            if(purchasePaymentType==null){
                throw new BadRequestExceptions(Constants.ErrorPurchasePaymentType);
            }

            try{
                List<RequestStockTransactionItem> requestStockTransactionItemList = requestPurchase.getRequestPurchaseItemList().stream().map(purchaseItem -> RequestStockTransactionItem.builder()
                        .quantity(purchaseItem.getQuantity())
                        .productId(purchaseItem.getProductId())
                        .build()).toList();
                StockTransaction newStockTransaction = iStockTransaction.save("S"+ requestPurchase.getRef().toUpperCase(), warehouse,requestStockTransactionItemList,"COMPRA",user);
                Long purchaseNumber = purchaseRepository.countByClientId(user.getClientId())+1L;
                Purchase newPurchase = purchaseRepository.save(com.proyect.masterdata.domain.Purchase.builder()
                        .ref(requestPurchase.getRef().toUpperCase())
                        .status(true)
                        .purchaseNumber(purchaseNumber)
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
                        .purchasePaymentType(purchasePaymentType)
                        .purchasePaymentTypeId(purchasePaymentType.getId())
                        .user(user).userId(user.getId())
                        .deliveryDate(OffsetDateTime.now())
                        .build());
                for(RequestPurchaseItem requestPurchaseItem : requestPurchase.getRequestPurchaseItemList()){
                    Product product = productRepository.findByIdAndStatusTrue(requestPurchaseItem.getProductId());
                    iPurchaseItem.save(newPurchase,warehouse.getName(), requestPurchaseItem,user.getUsername());
                    iWarehouseStock.in(warehouse,product, requestPurchaseItem.getQuantity(),user);
                    iGeneralStock.in(product, requestPurchaseItem.getQuantity(),user.getUsername());
                }
                iAudit.save("ADD_PURCHASE","COMPRA " + newPurchase.getRef() +" CREADA.",newPurchase.getRef(),user.getUsername());
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
            String ref,
            String user,
            String warehouse,
            String purchaseType,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<Purchase> pagePurchase;
            UUID clientId;

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                pagePurchase = purchaseRepositoryCustom.searchForPurchase(
                        clientId,
                        ref,
                        warehouse,
                        purchaseType,
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
                    .ref(purchase.getRef())
                    .purchaseDocument(purchase.getPurchaseDocument().getName())
                    .warehouse(purchase.getWarehouse().getName())
                    .purchaseType(purchase.getPurchaseType().getName())
                    .registrationDate(purchase.getRegistrationDate())
                    .purchasePaymentType(purchase.getPurchasePaymentType().getName())
                    .purchaseNumber(purchase.getPurchaseNumber())
                    .deliveryDate(purchase.getDeliveryDate())
                    .build()).toList();

            return new PageImpl<>(purchaseDTOS,pagePurchase.getPageable(),pagePurchase.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<Page<PurchaseDTO>> listFalse(
            String ref,
            String user,
            String warehouse,
            String purchaseType,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<Purchase> pagePurchase;
            UUID clientId;

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                pagePurchase = purchaseRepositoryCustom.searchForPurchase(
                        clientId,
                        ref,
                        warehouse,
                        purchaseType,
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
                    .ref(purchase.getRef())
                    .purchaseDocument(purchase.getPurchaseDocument().getName())
                    .warehouse(purchase.getWarehouse().getName())
                    .purchaseType(purchase.getPurchaseType().getName())
                    .registrationDate(purchase.getRegistrationDate())
                    .purchaseNumber(purchase.getPurchaseNumber())
                    .purchasePaymentType(purchase.getPurchasePaymentType().getName())
                    .deliveryDate(purchase.getDeliveryDate())
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
                    .ref(purchase.getRef())
                    .purchaseDocument(purchase.getPurchaseDocument().getName())
                    .warehouse(purchase.getWarehouse().getName())
                    .purchaseNumber(purchase.getPurchaseNumber())
                    .purchaseType(purchase.getPurchaseType().getName())
                    .registrationDate(purchase.getRegistrationDate())
                    .deliveryDate(purchase.getDeliveryDate())
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
                    .ref(purchase.getRef())
                    .purchaseNumber(purchase.getPurchaseNumber())
                    .purchaseDocument(purchase.getPurchaseDocument().getName())
                    .warehouse(purchase.getWarehouse().getName())
                    .purchaseType(purchase.getPurchaseType().getName())
                    .registrationDate(purchase.getRegistrationDate())
                    .purchasePaymentType(purchase.getPurchasePaymentType().getName())
                    .deliveryDate(purchase.getDeliveryDate())
                    .build()).toList();
        });
    }

    @Override
    public CompletableFuture<List<CheckStockDTO>> checkStock(UUID productId, String username) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<WarehouseStock> warehouseStocks;
            Product product;
            User user;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                product = productRepository.findByIdAndStatusTrue(productId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(product == null){
                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
            }else{
                warehouseStocks = warehouseStockRepository.findAllByProductId(product.getId());
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
