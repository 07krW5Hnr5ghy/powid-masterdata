package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.CategoryProduct;
import com.proyect.masterdata.domain.Size;
import com.proyect.masterdata.domain.SubCategoryProduct;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.SubCategoryProductDTO;
import com.proyect.masterdata.dto.request.RequestSubCategoryProduct;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.CategoryProductRepository;
import com.proyect.masterdata.repository.SubCategoryProductRepository;
import com.proyect.masterdata.repository.SubCategoryProductRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.ISubCategoryProduct;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class SubCategoryProductImpl implements ISubCategoryProduct {
    private final UserRepository userRepository;
    private final CategoryProductRepository categoryProductRepository;
    private final SubCategoryProductRepository subCategoryProductRepository;
    private final SubCategoryProductRepositoryCustom subCategoryProductRepositoryCustom;
    private final IAudit iAudit;
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
            iAudit.save("ADD_SUB_CATEGORY_PRODUCT","SUB CATEGORIA DE PRODUCTO "+requestSubCategoryProduct.getName().toUpperCase()+" CREADA.",requestSubCategoryProduct.getName().toUpperCase(),user.getUsername());
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
                iAudit.save("ADD_SUB_CATEGORY_PRODUCT","SUB CATEGORIA DE PRODUCTO "+requestSubCategoryProduct.getName().toUpperCase()+" CREADA.",requestSubCategoryProduct.getName().toUpperCase(),user.getUsername());
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
                subCategoryProductRepository.save(subCategoryProduct);
                iAudit.save("DELETE_SUB_CATEGORY_PRODUCT","SUB CATEGORIA DE PRODUCTO "+subCategoryProduct.getName()+" ELIMINADA.",subCategoryProduct.getName(),user.getUsername());
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
                subCategoryProductRepository.save(subCategoryProduct);
                iAudit.save("ACTIVATE_SUB_CATEGORY_PRODUCT","SUB CATEGORIA DE PRODUCTO "+subCategoryProduct.getName()+" ACTIVADA.",subCategoryProduct.getName(),user.getUsername());
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

    @Override
    public CompletableFuture<Page<SubCategoryProductDTO>> list(
            String name,
            String user,
            String sku,
            String categoryProduct,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<SubCategoryProduct> subCategoryProductPage;

            try{
                subCategoryProductPage = subCategoryProductRepositoryCustom.searchForSubCategoryProduct(
                        name,
                        sku,
                        user,
                        categoryProduct,
                        registrationStartDate,
                        registrationEndDate,
                        updateStartDate,
                        updateEndDate,
                        sort,
                        sortColumn,
                        pageNumber,
                        pageSize,
                        status
                );
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if(subCategoryProductPage.isEmpty()){
                return new PageImpl<>(Collections.emptyList());
            }

            List<SubCategoryProductDTO> subCategoryProductDTOS = subCategoryProductPage.getContent()
                    .stream().map(subCategoryProduct -> SubCategoryProductDTO.builder()
                            .categoryProduct(subCategoryProduct.getCategoryProduct().getName())
                            .name(subCategoryProduct.getName())
                            .sizeType(subCategoryProduct.getCategoryProduct().getSizeType().getName())
                            .sku(subCategoryProduct.getSku())
                            .registrationDate(subCategoryProduct.getRegistrationDate())
                            .updateDate(subCategoryProduct.getUpdateDate())
                            .status(subCategoryProduct.getStatus())
                            .build()).toList();

            return new PageImpl<>(subCategoryProductDTOS, subCategoryProductPage.getPageable(),
                    subCategoryProductPage.getTotalElements());
        });
    }
    @Override
    public CompletableFuture<List<SubCategoryProductDTO>> listByCategoryProduct(String username,String name) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<SubCategoryProduct> subCategoryProducts;
            User user;
            UUID categoryProductId;
            try{
                user = userRepository.findByUsernameAndStatusTrue(username.toUpperCase());
                categoryProductId = categoryProductRepository.findByNameAndStatusTrue(name.toUpperCase()).getId();
                subCategoryProducts = subCategoryProductRepository.findAllByCategoryProductIdAndStatusTrue(categoryProductId);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(subCategoryProducts.isEmpty()){
                return Collections.emptyList();
            }
            return subCategoryProducts.stream().map(subCategoryProduct -> SubCategoryProductDTO.builder()
                    .name(subCategoryProduct.getName())
                    .sku(subCategoryProduct.getSku())
                    .categoryProduct(subCategoryProduct.getCategoryProduct().getName())
                    .registrationDate(OffsetDateTime.now())
                    .updateDate(OffsetDateTime.now())
                    .sizeType(subCategoryProduct.getCategoryProduct().getSizeType().getName())
                    .build()).toList();
        });
    }
}
