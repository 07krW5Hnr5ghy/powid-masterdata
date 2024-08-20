package com.proyect.masterdata.services.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IAudit;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.dto.PurchaseItemDTO;
import com.proyect.masterdata.dto.request.RequestPurchaseItem;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.IGeneralStock;
import com.proyect.masterdata.services.IPurchaseItem;
import com.proyect.masterdata.services.IWarehouseStock;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class PurchaseItemImpl implements IPurchaseItem {

    private final UserRepository userRepository;
    private final PurchaseItemRepository purchaseItemRepository;
    private final WarehouseRepository warehouseRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final PurchaseItemRepositoryCustom purchaseItemRepositoryCustom;
    private final PurchaseRepository purchaseRepository;
    private final IWarehouseStock iWarehouseStock;
    private final IGeneralStock iGeneralStock;
    private final IAudit iAudit;
    @Override
    public PurchaseItem save(Purchase purchase, String warehouse, RequestPurchaseItem requestPurchaseItem,
                             String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        SupplierProduct supplierProduct;
        PurchaseItem purchaseItem;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(requestPurchaseItem.getSupplierProduct().toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(supplierProduct == null){
            throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
        }else{
            purchaseItem = purchaseItemRepository.findByPurchaseIdAndSupplierProductId(purchase.getId(),supplierProduct.getId());
        }

        if (purchaseItem != null) {
            throw new BadRequestExceptions(Constants.ErrorPurchaseExists);
        }

        try {

            PurchaseItem newPurchaseItem = purchaseItemRepository.save(PurchaseItem.builder()
                            .purchase(purchase)
                            .purchaseId(purchase.getId())
                            .supplierProduct(supplierProduct)
                            .supplierProductId(supplierProduct.getId())
                            .status(true)
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .updateDate(new Date(System.currentTimeMillis()))
                            .observations(requestPurchaseItem.getObservations().toUpperCase())
                            .client(user.getClient())
                            .clientId(user.getClientId())
                            .quantity(requestPurchaseItem.getQuantity())
                            .tokenUser(user.getUsername())
                    .build());

            iWarehouseStock.in(purchase.getWarehouse(),supplierProduct, requestPurchaseItem.getQuantity(), user);
            iGeneralStock.in(supplierProduct.getSerial(), requestPurchaseItem.getQuantity(), user.getUsername());
            iAudit.save("ADD_PURCHASE_ITEM","PRODUCTO DE INVENTARIO "+ newPurchaseItem.getSupplierProduct().getSerial()+" CREADO EN COMPRA.",newPurchaseItem.getPurchase().getSerial(),user.getUsername());
            return newPurchaseItem;
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public CompletableFuture<PurchaseItem> saveAsync(Purchase purchase, String warehouse, RequestPurchaseItem requestPurchaseItem, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            SupplierProduct supplierProduct;
            PurchaseItem purchaseItem;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(requestPurchaseItem.getSupplierProduct().toUpperCase());
                purchaseItem = purchaseItemRepository.findByPurchaseIdAndSupplierProductId(purchase.getId(),supplierProduct.getId());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(supplierProduct == null){
                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
            }

            if (purchaseItem != null) {
                throw new BadRequestExceptions(Constants.ErrorPurchaseItemExists);
            }

            try {

                PurchaseItem newPurchaseItem = purchaseItemRepository.save(PurchaseItem.builder()
                        .purchase(purchase)
                        .purchaseId(purchase.getId())
                        .supplierProduct(supplierProduct)
                        .supplierProductId(supplierProduct.getId())
                        .status(true)
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .updateDate(new Date(System.currentTimeMillis()))
                        .observations(requestPurchaseItem.getObservations().toUpperCase())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .quantity(requestPurchaseItem.getQuantity())
                        .tokenUser(user.getUsername())
                        .build());

                iWarehouseStock.in(purchase.getWarehouse(),supplierProduct, requestPurchaseItem.getQuantity(), user);
                iGeneralStock.in(supplierProduct.getSerial(), requestPurchaseItem.getQuantity(), user.getUsername());
                iAudit.save("ADD_PURCHASE_ITEM","PRODUCTO DE INVENTARIO "+ newPurchaseItem.getSupplierProduct().getSerial()+" CREADO EN COMPRA.",newPurchaseItem.getPurchase().getSerial(),user.getUsername());
                return newPurchaseItem;
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> delete(String serial,String supplierProductSerial, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            SupplierProduct supplierProduct;
            Purchase purchase;
            PurchaseItem purchaseItem;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                purchase = purchaseRepository.findBySerial(serial.toUpperCase());
                supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(supplierProductSerial.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(supplierProduct == null){
                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
            }else {
                purchaseItem = purchaseItemRepository.findByPurchaseIdAndSupplierProductId(purchase.getId(),supplierProduct.getId());
            }

            if (purchaseItem == null) {
                throw new BadRequestExceptions(Constants.ErrorPurchaseItem);
            }

            try {
                purchaseItem.setStatus(false);
                purchaseItem.setUpdateDate(new Date(System.currentTimeMillis()));
                purchaseItem.setTokenUser(user.getUsername());
                iWarehouseStock.out(purchase.getWarehouse(),supplierProduct, purchaseItem.getQuantity(), user);
                iGeneralStock.out(supplierProduct.getSerial(), purchaseItem.getQuantity(), user.getUsername());
                iAudit.save("DELETE_PURCHASE_ITEM","PRODUCTO DE INVENTARIO "+ purchaseItem.getSupplierProduct().getSerial()+" DESACTIVADO EN COMPRA.",purchaseItem.getPurchase().getSerial(),user.getUsername());
                return ResponseDelete.builder()
                        .message(Constants.delete)
                        .code(200)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(String purchaseSerial, String supplierProductSerial, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            SupplierProduct supplierProduct;
            Purchase purchase;
            PurchaseItem purchaseItem;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                purchase = purchaseRepository.findBySerial(purchaseSerial.toUpperCase());
                supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(supplierProductSerial.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(supplierProduct == null){
                throw new BadRequestExceptions(Constants.ErrorSupplierProduct);
            }else {
                purchaseItem = purchaseItemRepository.findByPurchaseIdAndSupplierProductId(purchase.getId(),supplierProduct.getId());
            }

            if (purchaseItem == null) {
                throw new BadRequestExceptions(Constants.ErrorPurchaseItem);
            }

            try {
                purchaseItem.setStatus(true);
                purchaseItem.setUpdateDate(new Date(System.currentTimeMillis()));
                purchaseItem.setTokenUser(user.getUsername());
                iWarehouseStock.in(purchase.getWarehouse(),supplierProduct, purchaseItem.getQuantity(), user);
                iGeneralStock.in(supplierProduct.getSerial(), purchaseItem.getQuantity(), user.getUsername());
                iAudit.save("ACTIVATE_PURCHASE_ITEM","PRODUCTO DE INVENTARIO "+ purchaseItem.getSupplierProduct().getSerial()+" ACTIVADO EN COMPRA.",purchaseItem.getPurchase().getSerial(),user.getUsername());
                return ResponseSuccess.builder()
                        .message(Constants.update)
                        .code(200)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<PurchaseItemDTO>> list(
            String user,
            List<String> purchases,
            List<String> warehouses,
            List<String> supplierProducts,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<PurchaseItem> pagePurchaseItem;
            Long clientId;
            List<Long> purchaseIds;
            List<Long> warehouseIds;
            List<Long> supplierProductIds;

            if(purchases != null && !purchases.isEmpty()){
                purchaseIds = purchaseRepository.findBySerialIn(
                        purchases.stream().map(String::toUpperCase).toList()
                ).stream().map(com.proyect.masterdata.domain.Purchase::getId).toList();
            }else{
                purchaseIds = new ArrayList<>();
            }

            if(warehouses != null && !warehouses.isEmpty()){
                warehouseIds = warehouseRepository.findByNameIn(
                        warehouses.stream().map(String::toUpperCase).toList()
                ).stream().map(Warehouse::getId).toList();
            }else{
                warehouseIds = new ArrayList<>();
            }

            if(supplierProducts != null && !supplierProducts.isEmpty()){
                supplierProductIds = supplierProductRepository.findBySerialIn(
                        supplierProducts.stream().map(String::toUpperCase).toList()
                ).stream().map(SupplierProduct::getId).toList();
            }else{
                supplierProductIds = new ArrayList<>();
            }

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                pagePurchaseItem = purchaseItemRepositoryCustom.searchForPurchaseItem(
                        clientId,
                        purchaseIds,
                        warehouseIds,
                        supplierProductIds,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.ResultsFound);
            }

            if (pagePurchaseItem.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<PurchaseItemDTO> purchaseItemDTOS = pagePurchaseItem.getContent().stream().map(purchaseItem -> PurchaseItemDTO.builder()
                    .purchase(purchaseItem.getPurchase().getSerial())
                    .quantity(purchaseItem.getQuantity())
                    .supplierProduct(purchaseItem.getSupplierProduct().getSerial())
                    .warehouse(purchaseItem.getPurchase().getWarehouse().getName())
                    .registrationDate(purchaseItem.getRegistrationDate())
                    .build()).toList();

            return new PageImpl<>(purchaseItemDTOS, pagePurchaseItem.getPageable(), pagePurchaseItem.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<PurchaseItemDTO>> listPurchaseItem(String user,Long id) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<PurchaseItem> purchaseItems;
            Long clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                if(id != null){
                    purchaseItems = purchaseItemRepository.findAllByClientIdAndPurchaseId(clientId,id);
                }else{
                    purchaseItems = purchaseItemRepository.findAllByClientId(clientId);
                }
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (purchaseItems.isEmpty()){
                return Collections.emptyList();
            }

            return purchaseItems.stream().map(purchaseItem -> PurchaseItemDTO.builder()
                    .purchase(purchaseItem.getPurchase().getSerial())
                    .quantity(purchaseItem.getQuantity())
                    .supplierProduct(purchaseItem.getSupplierProduct().getSerial())
                    .warehouse(purchaseItem.getPurchase().getWarehouse().getName())
                    .registrationDate(purchaseItem.getRegistrationDate())
                    .build()).toList();
        });
    }

}
