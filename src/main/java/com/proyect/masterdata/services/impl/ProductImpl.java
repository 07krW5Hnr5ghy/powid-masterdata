package com.proyect.masterdata.services.impl;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;

import com.proyect.masterdata.domain.*;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IProductPicture;
import com.proyect.masterdata.services.IProductPrice;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.dto.ProductDTO;
import com.proyect.masterdata.dto.request.RequestProductSave;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.services.IProduct;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class ProductImpl implements IProduct {
    private final ProductRepository productRepository;
    private final UserRepository userRepository;
    private final ModelRepository modelRepository;
    private final SizeRepository sizeRepository;
    private final CategoryRepository categoryRepository;
    private final CategoryProductRepository categoryProductRepository;
    private final ColorRepository colorRepository;
    private final ProductRepositoryCustom productRepositoryCustom;
    private final IProductPrice iProductPrice;
    private final UnitRepository unitRepository;
    private final ProductPriceRepository productPriceRepository;
    private final IProductPicture iProductPicture;
    private final ProductPictureRepository productPictureRepository;
    private final IAudit iAudit;
    @Override
    public ResponseSuccess save(RequestProductSave product, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions {
        User user;
        boolean existsProduct;
        Model modelData;
        Size sizeData;
        CategoryProduct categoryProductData;
        Color colorData;
        Unit unit;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            existsProduct = productRepository.existsBySkuAndStatusTrue(product.getSku().toUpperCase());
            modelData = modelRepository.findByName(product.getModel().toUpperCase());
            sizeData = sizeRepository.findByNameAndStatusTrue(product.getSize().toUpperCase());
            categoryProductData = categoryProductRepository
                    .findByNameAndStatusTrue(product.getCategory().toUpperCase());
            colorData = colorRepository.findByNameAndStatusTrue(product.getColor().toUpperCase());
            unit = unitRepository.findByNameAndStatusTrue(product.getUnit().toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (existsProduct) {
            throw new BadRequestExceptions(Constants.ErrorProductExists);
        }

        if (modelData == null) {
            throw new BadRequestExceptions(Constants.ErrorModel);
        }

        if (sizeData == null) {
            throw new BadRequestExceptions(Constants.ErrorSize);
        }

        if (categoryProductData == null) {
            throw new BadRequestExceptions(Constants.ErrorCategory);
        }

        if(!Objects.equals(sizeData.getSizeTypeId(), categoryProductData.getSizeTypeId())){
            throw new BadRequestExceptions(Constants.ErrorSizeTypeCategoryProduct);
        }

        if (colorData == null) {
            throw new BadRequestExceptions(Constants.ErrorColor);
        }

        if(unit == null){
            throw new BadRequestExceptions(Constants.ErrorUnit);
        }

        try {
            Product productData = productRepository.save(Product.builder()
                    .sku(product.getSku().toUpperCase())
                    .model(modelData)
                    .modelId(modelData.getId())
                    .size(sizeData)
                    .sizeId(sizeData.getId())
                    .categoryProduct(categoryProductData)
                    .categoryProductId(categoryProductData.getId())
                    .color(colorData)
                    .colorId(colorData.getId())
                    .unit(unit)
                    .unitId(unit.getId())
                    .client(user.getClient())
                    .clientId(user.getClientId())
                    .tokenUser(tokenUser.toUpperCase())
                    .status(true)
                    .registrationDate(new Date(System.currentTimeMillis()))
                    .build());
            iProductPrice.save(productData.getSku(), product.getPrice(),tokenUser.toUpperCase());
            iProductPicture.uploadPicture(product.getPictures(),productData.getId(),user.getUsername());
            iAudit.save("ADD_PRODUCT","ADD PRODUCT "+productData.getSku()+".",user.getUsername());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e) {
            e.printStackTrace();
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public CompletableFuture<ResponseSuccess> saveAsync(RequestProductSave product, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            boolean existsProduct;
            Model modelData;
            Size sizeData;
            CategoryProduct categoryProductData;
            Color colorData;
            Unit unit;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                existsProduct = productRepository.existsBySkuAndStatusTrue(product.getSku().toUpperCase());
                modelData = modelRepository.findByName(product.getModel().toUpperCase());
                sizeData = sizeRepository.findByNameAndStatusTrue(product.getSize().toUpperCase());
                categoryProductData = categoryProductRepository
                        .findByNameAndStatusTrue(product.getCategory().toUpperCase());
                colorData = colorRepository.findByNameAndStatusTrue(product.getColor().toUpperCase());
                unit = unitRepository.findByNameAndStatusTrue(product.getUnit().toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (existsProduct) {
                throw new BadRequestExceptions(Constants.ErrorProductExists);
            }

            if (modelData == null) {
                throw new BadRequestExceptions(Constants.ErrorModel);
            }

            if (sizeData == null) {
                throw new BadRequestExceptions(Constants.ErrorSize);
            }

            if (categoryProductData == null) {
                throw new BadRequestExceptions(Constants.ErrorCategory);
            }

            if(!Objects.equals(sizeData.getSizeTypeId(), categoryProductData.getSizeTypeId())){
                throw new BadRequestExceptions(Constants.ErrorSizeTypeCategoryProduct);
            }

            if (colorData == null) {
                throw new BadRequestExceptions(Constants.ErrorColor);
            }

            if(unit == null){
                throw new BadRequestExceptions(Constants.ErrorUnit);
            }

            try {
                Product productData = productRepository.save(Product.builder()
                        .sku(product.getSku().toUpperCase())
                        .model(modelData)
                        .modelId(modelData.getId())
                        .size(sizeData)
                        .sizeId(sizeData.getId())
                        .categoryProduct(categoryProductData)
                        .categoryProductId(categoryProductData.getId())
                        .color(colorData)
                        .colorId(colorData.getId())
                        .unit(unit)
                        .unitId(unit.getId())
                        .client(user.getClient())
                        .clientId(user.getClientId())
                        .tokenUser(tokenUser.toUpperCase())
                        .status(true)
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .build());
                iProductPrice.save(productData.getSku(), product.getPrice(),tokenUser.toUpperCase());
                iProductPicture.uploadPicture(product.getPictures(),productData.getId(),user.getUsername());
                iAudit.save("ADD_PRODUCT","ADD PRODUCT "+productData.getSku()+".",user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            } catch (RuntimeException e) {
                e.printStackTrace();
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> delete(String sku, String username) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Product product;

            try {
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                product = productRepository.findBySkuAndStatusTrue(sku.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user==null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (product == null) {
                throw new BadRequestExceptions(Constants.ErrorProduct);
            }

            try {
                product.setStatus(false);
                product.setUpdateDate(new Date(System.currentTimeMillis()));
                product.setTokenUser(user.getUsername());
                productRepository.save(product);
                iAudit.save("DELETE_PRODUCT","DELETE PRODUCT "+product.getSku()+".",user.getUsername());
                return ResponseDelete.builder()
                        .code(200)
                        .message(Constants.delete)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> activate(String sku, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Product product;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                product = productRepository.findBySkuAndStatusFalse(sku.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user==null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (product == null) {
                throw new BadRequestExceptions(Constants.ErrorProduct);
            }

            try {
                product.setStatus(true);
                product.setUpdateDate(new Date(System.currentTimeMillis()));
                product.setTokenUser(user.getUsername());
                productRepository.save(product);
                iAudit.save("ACTIVATE_PRODUCT","ACTIVATE PRODUCT "+product.getSku()+".",user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.update)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<Page<ProductDTO>> list(String sku, String model,String tokenUser, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) {
        return CompletableFuture.supplyAsync(()->{
            Page<Product> productPage;
            Model modelData;
            Long clientId;

            if(model != null){
                modelData = modelRepository.findByNameAndStatusTrue(model.toUpperCase());
            }else {
                modelData = null;
            }

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase()).getClient().getId();
                productPage = productRepositoryCustom.searchForProduct(sku, modelData,clientId, sort, sortColumn, pageNumber,
                        pageSize, true);

            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (productPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<ProductDTO> productDTOs = productPage.getContent().stream().map(product -> {
                ProductPrice productPrice = productPriceRepository.findByProductId(product.getId());
                List<String> productImages = productPictureRepository.findAllByProductId(product.getId()).stream().map(ProductPicture::getProductPictureUrl).toList();
                return ProductDTO.builder()
                        .sku(product.getSku())
                        .brand(product.getModel().getBrand().getName())
                        .model(product.getModel().getName())
                        .category(product.getCategoryProduct().getName())
                        .color(product.getColor().getName())
                        .size(product.getSize().getName())
                        .unit(product.getUnit().getName())
                        .price(productPrice.getUnitSalePrice())
                        .pictures(productImages)
                        .registrationDate(product.getRegistrationDate())
                        .updateDate(product.getUpdateDate())
                        .build();
            }).toList();

            return new PageImpl<>(productDTOs, productPage.getPageable(), productPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<Page<ProductDTO>> listFalse(String sku, String model, String tokenUser, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<Product> productPage;
            Model modelData;
            Long clientId;

            if(model != null){
                modelData = modelRepository.findByNameAndStatusTrue(model.toUpperCase());
            }else {
                modelData = null;
            }

            try {
                clientId = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase()).getClient().getId();
                productPage = productRepositoryCustom.searchForProduct(sku, modelData,clientId, sort, sortColumn, pageNumber,
                        pageSize, false);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (productPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<ProductDTO> productDTOs = productPage.getContent().stream().map(product -> {
                ProductPrice productPrice = productPriceRepository.findByProductId(product.getId());
                return ProductDTO.builder()
                        .sku(product.getSku())
                        .brand(product.getModel().getBrand().getName())
                        .model(product.getModel().getName())
                        .category(product.getCategoryProduct().getName())
                        .color(product.getColor().getName())
                        .size(product.getSize().getName())
                        .unit(product.getUnit().getName())
                        .price(productPrice.getUnitSalePrice())
                        .registrationDate(product.getRegistrationDate())
                        .updateDate(product.getUpdateDate())
                        .build();
            }).toList();

            return new PageImpl<>(productDTOs, productPage.getPageable(), productPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<ProductDTO>> listProducts(String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Product> products;
            Long clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                products = productRepository.findAllByClientIdAndStatusTrue(clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(products.isEmpty()){
                return Collections.emptyList();
            }

            return products.stream().map(product -> {
                ProductPrice productPrice = productPriceRepository.findByProductId(product.getId());
                return ProductDTO.builder()
                        .sku(product.getSku())
                        .brand(product.getModel().getBrand().getName())
                        .model(product.getModel().getName())
                        .category(product.getCategoryProduct().getName())
                        .color(product.getColor().getName())
                        .size(product.getSize().getName())
                        .unit(product.getUnit().getName())
                        .price(productPrice.getUnitSalePrice())
                        .registrationDate(product.getRegistrationDate())
                        .updateDate(product.getUpdateDate())
                        .build();
            }).toList();
        });
    }

    @Override
    public CompletableFuture<List<ProductDTO>> listProductsFalse(String user) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Product> products;
            Long clientId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                products = productRepository.findAllByClientIdAndStatusFalse(clientId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(products.isEmpty()){
                return Collections.emptyList();
            }

            return products.stream().map(product -> {
                ProductPrice productPrice = productPriceRepository.findByProductId(product.getId());
                return ProductDTO.builder()
                        .sku(product.getSku())
                        .brand(product.getModel().getBrand().getName())
                        .model(product.getModel().getName())
                        .category(product.getCategoryProduct().getName())
                        .color(product.getColor().getName())
                        .size(product.getSize().getName())
                        .unit(product.getUnit().getName())
                        .price(productPrice.getUnitSalePrice())
                        .registrationDate(product.getRegistrationDate())
                        .updateDate(product.getUpdateDate())
                        .build();
            }).toList();
        });
    }

    @Override
    public CompletableFuture<List<ProductDTO>> listProductsModel(String user, String model) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Product> products;
            Long clientId;
            Long modelId;
            try {
                clientId = userRepository.findByUsernameAndStatusTrue(user.toUpperCase()).getClientId();
                modelId = modelRepository.findByNameAndStatusTrue(model.toUpperCase()).getId();
                products = productRepository.findAllByClientIdAndModelIdAndStatusFalse(clientId,modelId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(products.isEmpty()){
                return Collections.emptyList();
            }

            return products.stream().map(product -> {
                ProductPrice productPrice = productPriceRepository.findByProductId(product.getId());
                return ProductDTO.builder()
                        .sku(product.getSku())
                        .brand(product.getModel().getBrand().getName())
                        .model(product.getModel().getName())
                        .category(product.getCategoryProduct().getName())
                        .color(product.getColor().getName())
                        .size(product.getSize().getName())
                        .unit(product.getUnit().getName())
                        .price(productPrice.getUnitSalePrice())
                        .registrationDate(product.getRegistrationDate())
                        .updateDate(product.getUpdateDate())
                        .build();
            }).toList();
        });
    }
}
