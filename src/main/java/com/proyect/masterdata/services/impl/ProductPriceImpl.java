package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Product;
import com.proyect.masterdata.domain.ProductPrice;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.ProductPriceRepository;
import com.proyect.masterdata.repository.ProductRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.IProductPrice;
import com.proyect.masterdata.services.IUtil;
import com.proyect.masterdata.utils.Constants;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.Date;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class ProductPriceImpl implements IProductPrice {
    private final UserRepository userRepository;
    private final ProductRepository productRepository;
    private final ProductPriceRepository productPriceRepository;
    private final IAudit iAudit;
    private final IUtil iUtil;
    @Override
    @Transactional
    public ResponseSuccess save(String productSku,Double unitPrice, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {

        User user;
        Product product;
        ProductPrice productPrice;

        try{
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            product = productRepository.findBySkuAndStatusTrue(productSku.toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if(user == null){
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(product == null){
            throw new BadRequestExceptions(Constants.ErrorProduct);
        }else {
            productPrice = productPriceRepository.findByProductIdAndStatusTrue(product.getId());
        }

        if(productPrice!=null){
            throw new BadRequestExceptions(Constants.ErrorProductPriceExist);
        }

        try {
            ProductPrice newProductPrice = productPriceRepository.save(ProductPrice.builder()
                            .product(product)
                            .productId(product.getId())
                            .unitSalePrice(unitPrice)
                            .status(true)
                            .registrationDate(OffsetDateTime.now())
                            .updateDate(OffsetDateTime.now())
                            .user(user)
                            .userId(user.getId())
                    .build());
            iAudit.save("ADD_PRODUCT_PRICE","PRECIO "+newProductPrice.getUnitSalePrice()+" DE PRODUCTO DE MARKETING "+iUtil.buildProductSku(newProductPrice.getProduct())+" AGREGADO.",iUtil.buildProductSku(newProductPrice.getProduct()),user.getUsername());
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
    @Transactional
    public CompletableFuture<ResponseSuccess> saveAsync(String productSku, Double unitPrice, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Product product;
            ProductPrice productPrice;

            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                product = productRepository.findBySkuAndStatusTrue(productSku.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(product == null){
                throw new BadRequestExceptions(Constants.ErrorProduct);
            }else {
                productPrice = productPriceRepository.findByProductIdAndStatusTrue(product.getId());
            }

            if(productPrice!=null){
                throw new BadRequestExceptions(Constants.ErrorProductPriceExist);
            }

            try {
                ProductPrice newProductPrice = productPriceRepository.save(ProductPrice.builder()
                        .product(product)
                        .productId(product.getId())
                        .unitSalePrice(unitPrice)
                        .status(true)
                        .registrationDate(OffsetDateTime.now())
                        .updateDate(OffsetDateTime.now())
                        .user(user)
                        .userId(user.getId())
                        .build());
                iAudit.save("ADD_PRODUCT_PRICE","PRECIO "+newProductPrice.getUnitSalePrice()+" DE PRODUCTO DE MARKETING "+iUtil.buildProductSku(newProductPrice.getProduct())+" AGREGADO.",iUtil.buildProductSku(newProductPrice.getProduct()),user.getUsername());
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
    public CompletableFuture<ResponseDelete> delete(String productSku, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Product product;
            ProductPrice productPrice;
            try {
                user=userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                product=productRepository.findBySkuAndStatusTrue(productSku.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(product==null){
                throw new BadRequestExceptions(Constants.ErrorProduct);
            }else {
                productPrice=productPriceRepository.findByProductIdAndStatusTrue(product.getId());
            }
            if(productPrice==null){
                throw new BadRequestExceptions(Constants.ErrorProductPrice);
            }
            try {
                productPrice.setStatus(true);
                productPrice.setUpdateDate(OffsetDateTime.now());
                productPrice.setUser(user);
                productPriceRepository.save(productPrice);
                iAudit.save("DELETE_PRODUCT_PRICE","PRECIO "+productPrice.getUnitSalePrice()+" DE PRODUCTO DE MARKETING "+iUtil.buildProductSku(productPrice.getProduct())+" DESACTIVADO.",iUtil.buildProductSku(productPrice.getProduct()),user.getUsername());
                return ResponseDelete.builder()
                        .code(200)
                        .message(Constants.delete)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }
}
