package com.proyect.masterdata.services.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.services.IAudit;
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
    @Override
    public ResponseSuccess save(RequestSupplierProduct requestSupplierProduct, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        Supplier supplier;
        Product product;
        SupplierProduct supplierProduct;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            product = productRepository.findBySkuAndStatusTrue(requestSupplierProduct.getProductSku());
            supplierProduct = supplierProductRepository.findBySerial(requestSupplierProduct.getSerial());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }else {
            supplier = supplierRepository.findByRucAndClientIdAndStatusTrue(requestSupplierProduct.getSupplierRuc(), user.getClientId());
        }

        if (supplier == null) {
            throw new BadRequestExceptions(Constants.ErrorSupplier);
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
                    .registrationDate(new Date(System.currentTimeMillis()))
                    .serial(requestSupplierProduct.getSerial())
                    .status(true)
                    .supplier(supplier)
                    .supplierId(supplier.getId())
                    .tokenUser(user.getUsername())
                    .build());
            iAudit.save("ADD_SUPPLIER_PRODUCT","ADD SUPPLIER PRODUCT "+newSupplierProduct.getSerial()+".",user.getUsername());
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
    public CompletableFuture<ResponseSuccess> saveAsync(RequestSupplierProduct requestSupplierProduct, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Supplier supplier;
            Product product;
            SupplierProduct supplierProduct;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                product = productRepository.findBySkuAndStatusTrue(requestSupplierProduct.getProductSku());
                supplierProduct = supplierProductRepository.findBySerial(requestSupplierProduct.getSerial());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }else {
                supplier = supplierRepository.findByRucAndClientIdAndStatusTrue(requestSupplierProduct.getSupplierRuc(), user.getClientId());
            }

            if (supplier == null) {
                throw new BadRequestExceptions(Constants.ErrorSupplier);
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
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .serial(requestSupplierProduct.getSerial())
                        .status(true)
                        .supplier(supplier)
                        .supplierId(supplier.getId())
                        .tokenUser(user.getUsername())
                        .build());
                iAudit.save("ADD_SUPPLIER_PRODUCT","ADD SUPPLIER PRODUCT "+newSupplierProduct.getSerial()+".",user.getUsername());
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
    public CompletableFuture<ResponseDelete> delete(String serial, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            SupplierProduct supplierProduct;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                supplierProduct = supplierProductRepository.findBySerialAndStatusTrue(serial);
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
                supplierProduct.setUpdateDate(new Date(System.currentTimeMillis()));
                supplierProduct.setTokenUser(user.getUsername());
                supplierProductRepository.save(supplierProduct);
                iAudit.save("DELETE_SUPPLIER_PRODUCT","DELETE SUPPLIER PRODUCT "+supplierProduct.getSerial()+".",user.getUsername());
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
    public CompletableFuture<ResponseSuccess> activate(String serial, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            SupplierProduct supplierProduct;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                supplierProduct = supplierProductRepository.findBySerialAndStatusFalse(serial);
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
                supplierProduct.setUpdateDate(new Date(System.currentTimeMillis()));
                supplierProduct.setTokenUser(user.getUsername());
                supplierProductRepository.save(supplierProduct);
                iAudit.save("ACTIVATE_SUPPLIER_PRODUCT","ACTIVATE SUPPLIER PRODUCT "+supplierProduct.getSerial()+".",user.getUsername());
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
            List<String> serials,
            List<String> products,
            List<String> suppliers,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<SupplierProduct> supplierProductPage;
            Long clientId;
            List<String> serialsUppercase;
            List<Long> productIds;
            List<Long> supplierIds;

            if(serials != null && !serials.isEmpty()){
                serialsUppercase = serials.stream().map(String::toUpperCase).toList();
            }else {
                serialsUppercase = new ArrayList<>();
            }

            if(products != null && !products.isEmpty()){
                productIds = productRepository.findBySkuIn(
                        products.stream().map(String::toUpperCase).toList()
                ).stream().map(Product::getId).toList();
            }else{
                productIds = new ArrayList<>();
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
                supplierProductPage = supplierProductRepositoryCustom.searchForSupplierProduct(
                        clientId,
                        serialsUppercase,
                        productIds,
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
                            .productSku(supplierProduct.getProduct().getSku())
                            .price(supplierProduct.getPurchasePrice())
                            .serial(supplierProduct.getSerial())
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
            List<String> serials,
            List<String> products,
            List<String> suppliers,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize
    ) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<SupplierProduct> supplierProductPage;
            Long clientId;
            List<String> serialsUppercase;
            List<Long> productIds;
            List<Long> supplierIds;

            if(serials != null && !serials.isEmpty()){
                serialsUppercase = serials.stream().map(String::toUpperCase).toList();
            }else {
                serialsUppercase = new ArrayList<>();
            }

            if(products != null && !products.isEmpty()){
                productIds = productRepository.findBySkuIn(
                        products.stream().map(String::toUpperCase).toList()
                ).stream().map(Product::getId).toList();
            }else{
                productIds = new ArrayList<>();
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
                supplierProductPage = supplierProductRepositoryCustom.searchForSupplierProduct(
                        clientId,
                        serialsUppercase,
                        productIds,
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
                            .productSku(supplierProduct.getProduct().getSku())
                            .price(supplierProduct.getPurchasePrice())
                            .serial(supplierProduct.getSerial())
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
    public CompletableFuture<List<SupplierProductDTO>> listSupplierProduct(String user,Long id) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<SupplierProduct> supplierProducts;
            Long clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                if(id != null){
                    supplierProducts = supplierProductRepository.findAllByClientIdAndSupplierIdAndStatusTrue(clientId,id);
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
                            .productSku(supplierProduct.getProduct().getSku())
                            .price(supplierProduct.getPurchasePrice())
                            .serial(supplierProduct.getSerial())
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
            Long clientId;
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
                            .productSku(supplierProduct.getProduct().getSku())
                            .price(supplierProduct.getPurchasePrice())
                            .serial(supplierProduct.getSerial())
                            .supplier(supplierProduct.getSupplier().getBusinessName())
                            .registrationDate(supplierProduct.getRegistrationDate())
                            .updateDate(supplierProduct.getUpdateDate())
                            .build())
                    .toList();
        });
    }

    @Override
    public CompletableFuture<List<SupplierProductDTO>> listSupplierProductFalse(String user,Long id) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<SupplierProduct> supplierProducts;
            Long clientId;
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
                            .productSku(supplierProduct.getProduct().getSku())
                            .price(supplierProduct.getPurchasePrice())
                            .serial(supplierProduct.getSerial())
                            .supplier(supplierProduct.getSupplier().getBusinessName())
                            .registrationDate(supplierProduct.getRegistrationDate())
                            .updateDate(supplierProduct.getUpdateDate())
                            .build())
                    .toList();
        });
    }

    @Override
    public CompletableFuture<List<SupplierProductDTO>> listSupplierProductByProduct(String user, String productSku) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<SupplierProduct> supplierProducts;
            Long clientId;
            Long productId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                productId = productRepository.findBySkuAndStatusTrue(productSku.toUpperCase()).getId();
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
                            .productSku(supplierProduct.getProduct().getSku())
                            .price(supplierProduct.getPurchasePrice())
                            .serial(supplierProduct.getSerial())
                            .supplier(supplierProduct.getSupplier().getBusinessName())
                            .registrationDate(supplierProduct.getRegistrationDate())
                            .updateDate(supplierProduct.getUpdateDate())
                            .build())
                    .toList();
        });
    }

}
