package com.proyect.masterdata.services.impl;

import java.util.Collections;
import java.util.Date;
import java.util.List;

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

    @Override
    public ResponseSuccess save(RequestSupplierProduct requestSupplierProduct, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        Supplier supplier;
        Product product;
        SupplierProduct supplierProduct;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            supplier = supplierRepository.findByRucAndStatusTrue(requestSupplierProduct.getSupplierRuc());
            product = productRepository.findBySkuAndStatusTrue(requestSupplierProduct.getProductSku());
            supplierProduct = supplierProductRepository.findBySerial(requestSupplierProduct.getSerial());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
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
            supplierProductRepository.save(SupplierProduct.builder()
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
    public ResponseSuccess saveAll(List<RequestSupplierProduct> requestSupplierProducts, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        List<SupplierProduct> supplierProducts;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            supplierProducts = supplierProductRepository.findBySerialIn(
                    requestSupplierProducts.stream().map(supplierProduct -> supplierProduct.getSerial()).toList());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (supplierProducts.size() > 0) {
            throw new BadRequestExceptions(Constants.ErrorSupplierExists);
        }

        try {
            List<SupplierProduct> supplierProductsList = requestSupplierProducts.stream().map(supplierProduct -> {

                Supplier supplier = supplierRepository.findByRucAndStatusTrue(supplierProduct.getSupplierRuc());

                if (supplier == null) {
                    throw new BadRequestExceptions(Constants.ErrorSupplier);
                }

                Product product = productRepository.findBySkuAndStatusTrue(supplierProduct.getProductSku());

                if (product == null) {
                    throw new BadRequestExceptions(Constants.ErrorProduct);
                }

                return SupplierProduct.builder()
                        .product(product)
                        .productId(product.getId())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .purchasePrice(supplierProduct.getPurchasePrice())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .serial(supplierProduct.getSerial())
                        .status(true)
                        .supplier(supplier)
                        .supplierId(supplier.getId())
                        .tokenUser(tokenUser.toUpperCase())
                        .build();
            }).toList();

            supplierProductRepository.saveAll(supplierProductsList);

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
    public ResponseDelete delete(String serial, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {

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
            supplierProductRepository.save(supplierProduct);

            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e) {
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public Page<SupplierProductDTO> list(String serial, String user, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {

        Page<SupplierProduct> supplierProductPage;
        Long clientId;

        try {
            clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
            supplierProductPage = supplierProductRepositoryCustom.searchForSupplierProduct(serial, clientId, sort,
                    sortColumn, pageNumber, pageSize, true);
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
                        .purchasePrice(supplierProduct.getPurchasePrice())
                        .serial(supplierProduct.getSerial())
                        .supplierRuc(supplierProduct.getSupplier().getBusinessName())
                        .build())
                .toList();

        return new PageImpl<>(supplierProductDTOs, supplierProductPage.getPageable(),
                supplierProductPage.getTotalElements());
    }

}
