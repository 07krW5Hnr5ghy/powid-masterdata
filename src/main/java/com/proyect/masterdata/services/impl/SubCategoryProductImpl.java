package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.CategoryProduct;
import com.proyect.masterdata.domain.SubCategoryProduct;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.request.RequestSubCategoryProduct;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.CategoryProductRepository;
import com.proyect.masterdata.repository.SubCategoryProductRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.ISubCategoryProduct;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class SubCategoryProductImpl implements ISubCategoryProduct {
    private final UserRepository userRepository;
    private final CategoryProductRepository categoryProductRepository;
    private final SubCategoryProductRepository subCategoryProductRepository;
    @Override
    public ResponseSuccess save(RequestSubCategoryProduct requestSubCategoryProduct) throws BadRequestExceptions, InternalErrorExceptions {
        User user;
        CategoryProduct categoryProduct;
        SubCategoryProduct subCategoryProduct;
        try{
            user = userRepository.findByUsernameAndStatusTrue(requestSubCategoryProduct.getTokenUser().toUpperCase());
            categoryProduct = categoryProductRepository.findByNameAndStatusTrue(requestSubCategoryProduct.getCategoryName().toUpperCase());
        }catch (RuntimeException e){
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if(categoryProduct == null){
            throw new BadRequestExceptions(Constants.ErrorCategoryProduct);
        }else{
            subCategoryProduct = subCategoryProductRepository.findByNameOrSku(requestSubCategoryProduct.getName().toUpperCase(), requestSubCategoryProduct.getSku().toUpperCase());
        }

        if(subCategoryProduct!=null){
            throw new BadRequestExceptions(Constants.ErrorSubCategoryExists);
        }

        try{
            subCategoryProductRepository.save(SubCategoryProduct.builder()
                            .categoryProduct(categoryProduct)
                            .categoryProductId(categoryProduct.getId())
                            .sku(requestSubCategoryProduct.getSku().toUpperCase())
                            .name(requestSubCategoryProduct.getName().toUpperCase())
                            .registrationDate(OffsetDateTime.now())
                            .updateDate(OffsetDateTime.now())
                            .status(true)
                            .user(user)
                            .userId(user.getId())
                    .build());
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
    public CompletableFuture<ResponseSuccess> saveAsync(RequestSubCategoryProduct requestSubCategoryProduct) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            CategoryProduct categoryProduct;
            SubCategoryProduct subCategoryProduct;
            try{
                user = userRepository.findByUsernameAndStatusTrue(requestSubCategoryProduct.getTokenUser().toUpperCase());
                categoryProduct = categoryProductRepository.findByNameAndStatusTrue(requestSubCategoryProduct.getCategoryName().toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(categoryProduct == null){
                throw new BadRequestExceptions(Constants.ErrorCategoryProduct);
            }else{
                subCategoryProduct = subCategoryProductRepository.findByNameOrSku(requestSubCategoryProduct.getName().toUpperCase(), requestSubCategoryProduct.getSku().toUpperCase());
            }

            if(subCategoryProduct!=null){
                throw new BadRequestExceptions(Constants.ErrorSubCategoryExists);
            }

            try{
                subCategoryProductRepository.save(SubCategoryProduct.builder()
                        .categoryProduct(categoryProduct)
                        .categoryProductId(categoryProduct.getId())
                        .sku(requestSubCategoryProduct.getSku().toUpperCase())
                        .name(requestSubCategoryProduct.getName().toUpperCase())
                        .registrationDate(OffsetDateTime.now())
                        .updateDate(OffsetDateTime.now())
                        .status(true)
                        .user(user)
                        .userId(user.getId())
                        .build());
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
    public CompletableFuture<ResponseDelete> delete(String name,String sku, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            SubCategoryProduct subCategoryProduct;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                subCategoryProduct = subCategoryProductRepository.findByNameAndSkuAndStatusTrue(name.toUpperCase(),sku.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(subCategoryProduct==null){
                throw new BadRequestExceptions(Constants.ErrorSubCategoryExists);
            }
            try {
                subCategoryProduct.setStatus(false);
                subCategoryProduct.setUpdateDate(OffsetDateTime.now());
                subCategoryProduct.setUser(user);
                subCategoryProduct.setUserId(user.getId());
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

    @Override
    public CompletableFuture<ResponseSuccess> activate(String name,String sku, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            SubCategoryProduct subCategoryProduct;
            try{
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                subCategoryProduct = subCategoryProductRepository.findByNameAndSkuAndStatusTrue(name.toUpperCase(),sku.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(subCategoryProduct==null){
                throw new BadRequestExceptions(Constants.ErrorSubCategoryExists);
            }
            try {
                subCategoryProduct.setStatus(true);
                subCategoryProduct.setUpdateDate(OffsetDateTime.now());
                subCategoryProduct.setUser(user);
                subCategoryProduct.setUserId(user.getId());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.update)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }
}
