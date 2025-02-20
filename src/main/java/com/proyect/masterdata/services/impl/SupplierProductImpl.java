package com.proyect.masterdata.services.impl;

import java.time.OffsetDateTime;
import java.util.*;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IUtil;
import jakarta.transaction.Transactional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.Product;
import com.proyect.masterdata.domain.Supplier;
import com.proyect.masterdata.domain.SupplierProduct;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.SupplierProductDTO;
import com.proyect.masterdata.dto.request.RequestSupplierProduct;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.ProductRepository;
import com.proyect.masterdata.repository.SupplierProductRepository;
import com.proyect.masterdata.repository.SupplierProductRepositoryCustom;
import com.proyect.masterdata.repository.SupplierRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.ISupplierProduct;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class SupplierProductImpl implements ISupplierProduct {

    private final UserRepository userRepository;
    private final SupplierRepository supplierRepository;
    private final ProductRepository productRepository;
    private final SupplierProductRepository supplierProductRepository;
    private final SupplierProductRepositoryCustom supplierProductRepositoryCustom;
    private final IAudit iAudit;
    private final IUtil iUtil;
    @Override
    @Transactional
    public ResponseSuccess save(RequestSupplierProduct requestSupplierProduct, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        Supplier supplier;
        Product product;
        SupplierProduct supplierProduct;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            product = productRepository.findByIdAndStatusTrue(requestSupplierProduct.getProductId());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }else {
            supplier = supplierRepository.findByRucAndClientIdAndStatusTrue(requestSupplierProduct.getSupplier(), user.getClientId());
        }

        if (supplier == null) {
            throw new BadRequestExceptions(Constants.ErrorSupplier);
        }else{
            supplierProduct = supplierProductRepository.findBySupplierIdAndProductId(supplier.getId(),product.getId());
        }

        if (product == null) {
            throw new BadRequestExceptions(Constants.ErrorProduct);
        }

        if (supplierProduct != null) {
            throw new BadRequestExceptions(Constants.ErrorSupplierProductExists);
        }

        try {
            SupplierProduct newSupplierProduct = supplierProductRepository.save(SupplierProduct.builder()
                    .client(user.getClient())
                    .clientId(user.getClientId())
                    .product(product)
                    .productId(product.getId())
                    .purchasePrice(requestSupplierProduct.getPurchasePrice())
                    .registrationDate(OffsetDateTime.now())
                    .status(true)
                    .supplier(supplier)
                    .supplierId(supplier.getId())
                    .user(user).userId(user.getId())
                    .build());
            String finalSku = iUtil.buildInventorySku(newSupplierProduct);
            iAudit.save(
                    "ADD_SUPPLIER_PRODUCT",
                    "PRODUCTO DE INVENTARIO "+
                            finalSku+" CREADO.",
                    finalSku,user.getUsername());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

    }

    @Override
    @Transactional
    public CompletableFuture<ResponseSuccess> saveAsync(RequestSupplierProduct requestSupplierProduct, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Supplier supplier;
            Product product;
            SupplierProduct supplierProduct;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                product = productRepository.findByIdAndStatusTrue(requestSupplierProduct.getProductId());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else {
                supplier = supplierRepository.findByRucAndClientIdAndStatusTrue(requestSupplierProduct.getSupplier(), user.getClientId());
            }

            if (product == null) {
                throw new BadRequestExceptions(Constants.ErrorProduct);
            }

            if (supplier == null) {
                throw new BadRequestExceptions(Constants.ErrorSupplier);
            }else{
                supplierProduct = supplierProductRepository.findBySupplierIdAndProductId(supplier.getId(),product.getId());
            }

            if (supplierProduct != null) {
                throw new BadRequestExceptions(Constants.ErrorSupplierProductExists);
            }

            try {
                SupplierProduct newSupplierProduct = supplierProductRepository.save(SupplierProduct.builder()
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .product(product)
                        .productId(product.getId())
                        .purchasePrice(requestSupplierProduct.getPurchasePrice())
                        .registrationDate(OffsetDateTime.now())
                        .status(true)
                        .supplier(supplier)
                        .supplierId(supplier.getId())
                        .user(user).userId(user.getId())
                        .build());
                String finalSku = iUtil.buildInventorySku(newSupplierProduct);
                iAudit.save(
                        "ADD_SUPPLIER_PRODUCT",
                        "PRODUCTO DE INVENTARIO "+
                                finalSku+" CREADO.",
                        finalSku,user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> delete(UUID supplierProductId, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            SupplierProduct supplierProduct;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                supplierProduct = supplierProductRepository.findByIdAndStatusTrue(supplierProductId);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (supplierProduct == null) {
                throw new BadRequestExceptions(Constants.ErrorSupplier);
            }

            try {

                supplierProduct.setStatus(false);
                supplierProduct.setUpdateDate(OffsetDateTime.now());
                supplierProduct.setUser(user);
                supplierProduct.setUserId(user.getId());
                supplierProductRepository.save(supplierProduct);
                String finalSku = iUtil.buildInventorySku(supplierProduct);
                iAudit.save(
                        "DELETE_SUPPLIER_PRODUCT",
                        "PRODUCTO DE INVENTARIO "+
                                finalSku+
                                " DESACTIVADO.",
                        finalSku,user.getUsername());
                return ResponseDelete.builder()
                        .code(200)
                        .message(Constants.delete)
                        .build();
            } catch (RuntimeException e) {
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(UUID supplierProductId, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            SupplierProduct supplierProduct;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                supplierProduct = supplierProductRepository.findByIdAndStatusFalse(supplierProductId);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (supplierProduct == null) {
                throw new BadRequestExceptions(Constants.ErrorSupplier);
            }

            try {
                supplierProduct.setStatus(true);
                supplierProduct.setUpdateDate(OffsetDateTime.now());
                supplierProduct.setUser(user);
                supplierProduct.setUserId(user.getId());
                supplierProductRepository.save(supplierProduct);
                String finalSku = iUtil.buildInventorySku(supplierProduct);
                iAudit.save(
                        "ACTIVATE_SUPPLIER_PRODUCT",
                        "PRODUCTO DE INVENTARIO "+
                                finalSku+" ACTIVADO.",
                        finalSku,user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.update)
                        .build();
            } catch (RuntimeException e) {
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<SupplierProductDTO>> list(
            String user,
            String serial,
            String model,
            List<String> suppliers,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<SupplierProduct> supplierProductPage;
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
                supplierProductPage = supplierProductRepositoryCustom.searchForSupplierProduct(
                        clientId,
                        serial,
                        model,
                        supplierIds,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        true);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (supplierProductPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<SupplierProductDTO> supplierProductDTOs = supplierProductPage.getContent().stream()
                    .map(supplierProduct -> SupplierProductDTO.builder()
                            .productSku(iUtil.buildProductSku(supplierProduct.getProduct()))
                            .model(supplierProduct.getProduct().getModel().getName())
                            .color(supplierProduct.getProduct().getColor().getName())
                            .size(supplierProduct.getProduct().getSize().getName())
                            .price(supplierProduct.getPurchasePrice())
                            .serial(iUtil.buildInventorySku(supplierProduct))
                            .supplier(supplierProduct.getSupplier().getBusinessName())
                            .registrationDate(supplierProduct.getRegistrationDate())
                            .updateDate(supplierProduct.getUpdateDate())
                            .build())
                    .toList();

            return new PageImpl<>(supplierProductDTOs, supplierProductPage.getPageable(),
                    supplierProductPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<Page<SupplierProductDTO>> listFalse(
            String user,
            String serial,
            String model,
            List<String> suppliers,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize
    ) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<SupplierProduct> supplierProductPage;
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
                supplierProductPage = supplierProductRepositoryCustom.searchForSupplierProduct(
                        clientId,
                        serial,
                        model,
                        supplierIds,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        false);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (supplierProductPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<SupplierProductDTO> supplierProductDTOs = supplierProductPage.getContent().stream()
                    .map(supplierProduct -> SupplierProductDTO.builder()
                            .productSku(iUtil.buildProductSku(supplierProduct.getProduct()))
                            .model(supplierProduct.getProduct().getModel().getName())
                            .color(supplierProduct.getProduct().getColor().getName())
                            .size(supplierProduct.getProduct().getSize().getName())
                            .price(supplierProduct.getPurchasePrice())
                            .serial(iUtil.buildInventorySku(supplierProduct))
                            .supplier(supplierProduct.getSupplier().getBusinessName())
                            .registrationDate(supplierProduct.getRegistrationDate())
                            .updateDate(supplierProduct.getUpdateDate())
                            .build())
                    .toList();

            return new PageImpl<>(supplierProductDTOs, supplierProductPage.getPageable(),
                    supplierProductPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<SupplierProductDTO>> listSupplierProduct(String user,String supplier) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<SupplierProduct> supplierProducts;
            UUID clientId;
            UUID supplierId;

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                if(supplier != null){
                    supplierId = supplierRepository.findByClientIdAndRucAndStatusTrue(clientId,supplier.toUpperCase()).getId();
                }else{
                    supplierId = null;
                }
                if(supplierId != null){
                    supplierProducts = supplierProductRepository.findAllByClientIdAndSupplierIdAndStatusTrue(clientId,supplierId);
                }else {
                    supplierProducts = supplierProductRepository.findAllByClientIdAndStatusTrue(clientId);
                }
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(supplierProducts.isEmpty()){
                return Collections.emptyList();
            }

            return supplierProducts.stream()
                    .map(supplierProduct -> SupplierProductDTO.builder()
                            .productSku(iUtil.buildProductSku(supplierProduct.getProduct()))
                            .model(supplierProduct.getProduct().getModel().getName())
                            .color(supplierProduct.getProduct().getColor().getName())
                            .size(supplierProduct.getProduct().getSize().getName())
                            .price(supplierProduct.getPurchasePrice())
                            .serial(iUtil.buildInventorySku(supplierProduct))
                            .supplier(supplierProduct.getSupplier().getBusinessName())
                            .registrationDate(supplierProduct.getRegistrationDate())
                            .updateDate(supplierProduct.getUpdateDate())
                            .build())
                    .toList();
        });
    }

    @Override
    public CompletableFuture<List<SupplierProductDTO>> listFilter(String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<SupplierProduct> supplierProducts;
            UUID clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                supplierProducts = supplierProductRepository.findAllByClientId(clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(supplierProducts.isEmpty()){
                return Collections.emptyList();
            }

            return supplierProducts.stream()
                    .map(supplierProduct -> SupplierProductDTO.builder()
                            .productSku(iUtil.buildProductSku(supplierProduct.getProduct()))
                            .model(supplierProduct.getProduct().getModel().getName())
                            .color(supplierProduct.getProduct().getColor().getName())
                            .size(supplierProduct.getProduct().getSize().getName())
                            .price(supplierProduct.getPurchasePrice())
                            .serial(iUtil.buildInventorySku(supplierProduct))
                            .supplier(supplierProduct.getSupplier().getBusinessName())
                            .registrationDate(supplierProduct.getRegistrationDate())
                            .updateDate(supplierProduct.getUpdateDate())
                            .build())
                    .toList();
        });
    }

    @Override
    public CompletableFuture<List<SupplierProductDTO>> listSupplierProductFalse(String user,UUID id) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<SupplierProduct> supplierProducts;
            UUID clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusFalse(user.toUpperCase()).getClientId();
                if(id != null){
                    supplierProducts = supplierProductRepository.findAllByClientIdAndSupplierIdAndStatusTrue(clientId,id);
                }else{
                    supplierProducts = supplierProductRepository.findAllByClientIdAndStatusTrue(clientId);
                }
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(supplierProducts.isEmpty()){
                return Collections.emptyList();
            }

            return supplierProducts.stream()
                    .map(supplierProduct -> SupplierProductDTO.builder()
                            .productSku(iUtil.buildProductSku(supplierProduct.getProduct()))
                            .model(supplierProduct.getProduct().getModel().getName())
                            .color(supplierProduct.getProduct().getColor().getName())
                            .size(supplierProduct.getProduct().getSize().getName())
                            .price(supplierProduct.getPurchasePrice())
                            .serial(iUtil.buildInventorySku(supplierProduct))
                            .supplier(supplierProduct.getSupplier().getBusinessName())
                            .registrationDate(supplierProduct.getRegistrationDate())
                            .updateDate(supplierProduct.getUpdateDate())
                            .build())
                    .toList();
        });
    }

    @Override
    public CompletableFuture<List<SupplierProductDTO>> listSupplierProductByProduct(String user, UUID productId) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<SupplierProduct> supplierProducts;
            UUID clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                supplierProducts = supplierProductRepository.findAllByClientIdAndProductIdAndStatusTrue(clientId,productId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(supplierProducts.isEmpty()){
                return Collections.emptyList();
            }

            return supplierProducts.stream()
                    .map(supplierProduct -> SupplierProductDTO.builder()
                            .productSku(iUtil.buildProductSku(supplierProduct.getProduct()))
                            .model(supplierProduct.getProduct().getModel().getName())
                            .color(supplierProduct.getProduct().getColor().getName())
                            .size(supplierProduct.getProduct().getSize().getName())
                            .price(supplierProduct.getPurchasePrice())
                            .serial(iUtil.buildInventorySku(supplierProduct))
                            .supplier(supplierProduct.getSupplier().getBusinessName())
                            .registrationDate(supplierProduct.getRegistrationDate())
                            .updateDate(supplierProduct.getUpdateDate())
                            .build())
                    .toList();
        });
    }

}
