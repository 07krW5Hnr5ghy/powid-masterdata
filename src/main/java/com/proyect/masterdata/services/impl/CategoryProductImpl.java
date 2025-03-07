package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.CategoryProduct;
import com.proyect.masterdata.domain.SizeType;
import com.proyect.masterdata.domain.UnitType;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.CategoryProductDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.*;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.ICategoryProduct;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import java.time.OffsetDateTime;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Service
@RequiredArgsConstructor
@Log4j2
public class CategoryProductImpl implements ICategoryProduct {

    private final UserRepository userRepository;
    private final CategoryProductRepository categoryProductRepository;
    private final CategoryProductRepositoryCustom categoryProductRepositoryCustom;
    private final SizeTypeRepository sizeTypeRepository;
    private final UnitTypeRepository unitTypeRepository;
    private final IAudit iAudit;
    @Override
    public ResponseSuccess save(String name,String sku,String sizeTypeName, String unitTypeName , String tokenUser)
            throws BadRequestExceptions, InternalErrorExceptions {

        User user;
        CategoryProduct categoryProduct;
        SizeType sizeType;
        UnitType unitType;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            categoryProduct = categoryProductRepository.findByName(name.toUpperCase());
            sizeType = sizeTypeRepository.findByNameAndStatusTrue(sizeTypeName.toUpperCase());
            unitType = unitTypeRepository.findByNameAndStatusTrue(unitTypeName.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (categoryProduct != null) {
            throw new BadRequestExceptions(Constants.ErrorCategoryProductExists);
        }

        if(sizeType == null){
            throw new BadRequestExceptions(Constants.ErrorSizeType);
        }

        if(unitType == null){
            throw new BadRequestExceptions(Constants.ErrorUnitType);
        }

        try {
            CategoryProduct newCategoryProduct = categoryProductRepository.save(CategoryProduct.builder()
                    .name(name.toUpperCase())
                            .sku(sku.toUpperCase())
                    .registrationDate(OffsetDateTime.now())
                            .updateDate(OffsetDateTime.now())
                    .status(true)
                            .user(user)
                            .userId(user.getId())
                            .sizeType(sizeType)
                            .sizeTypeId(sizeType.getId())
                            .unitType(unitType)
                            .unitTypeId(unitType.getId())
                            .client(user.getClient())
                            .clientId(user.getClientId())
                    .build());
            iAudit.save("ADD_CATEGORY_PRODUCT","CATEGORIA DE PRODUCTO "+newCategoryProduct.getName()+" CREADA.",newCategoryProduct.getName(),user.getUsername());
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
    public CompletableFuture<ResponseSuccess> saveAsync(String name,String sku,String sizeTypeName,String unitTypeName , String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            CategoryProduct categoryProduct;
            SizeType sizeType;
            UnitType unitType;


            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                categoryProduct = categoryProductRepository.findByName(name.toUpperCase());
                sizeType = sizeTypeRepository.findByNameAndStatusTrue(sizeTypeName.toUpperCase());
                unitType = unitTypeRepository.findByNameAndStatusTrue(unitTypeName.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if (categoryProduct != null) {
                throw new BadRequestExceptions(Constants.ErrorCategoryProductExists);
            }

            if(sizeType == null){
                throw new BadRequestExceptions(Constants.ErrorSizeType);
            }

            if(unitType == null){
                throw new BadRequestExceptions(Constants.ErrorUnitType);
            }

            try {
                CategoryProduct newCategoryProduct = categoryProductRepository.save(CategoryProduct.builder()
                        .name(name.toUpperCase())
                        .sku(sku.toUpperCase())
                        .registrationDate(OffsetDateTime.now())
                        .updateDate(OffsetDateTime.now())
                        .status(true)
                        .user(user)
                        .userId(user.getId())
                                .sizeType(sizeType)
                                .sizeTypeId(sizeType.getId())
                                .unitTypeId(unitType.getId())
                                .unitType(unitType)
                                .client(user.getClient())
                                .clientId(user.getClientId())
                        .build());
                iAudit.save("ADD_CATEGORY_PRODUCT","CATEGORIA DE PRODUCTO "+newCategoryProduct.getName()+" CREADA.",newCategoryProduct.getName(),user.getUsername());
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
    public CompletableFuture<Page<CategoryProductDTO>> list(String name, String user,OffsetDateTime registrationStartDate, OffsetDateTime registrationEndDate, OffsetDateTime updateStartDate, OffsetDateTime updateEndDate, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{

            Page<CategoryProduct> categoryProductPage;

            try {
                categoryProductPage = categoryProductRepositoryCustom.searchForCategoryProduct(name, user,registrationStartDate,registrationEndDate,updateStartDate,updateStartDate, sort, sortColumn,
                        pageNumber, pageSize, true);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (categoryProductPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<CategoryProductDTO> categoryProductDTOs = categoryProductPage.getContent().stream()
                    .map(categoryProduct -> CategoryProductDTO.builder()
                            .status(categoryProduct.getStatus())
                            .id(categoryProduct.getId())
                            .user(categoryProduct.getUser().getUsername())
                            .name(categoryProduct.getName())
                            .registrationDate(categoryProduct.getRegistrationDate())
                            .updateDate(categoryProduct.getUpdateDate())
                            .sizeType(categoryProduct.getSizeType().getName())
                            .unitType(categoryProduct.getUnitType().getName())
                            .build())
                    .toList();

            return new PageImpl<>(categoryProductDTOs, categoryProductPage.getPageable(),
                    categoryProductPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<Page<CategoryProductDTO>> listFalse(String name, String user, OffsetDateTime registrationStartDate, OffsetDateTime registrationEndDate, OffsetDateTime updateStartDate, OffsetDateTime updateEndDate, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{

            Page<CategoryProduct> categoryProductPage;

            try {
                categoryProductPage = categoryProductRepositoryCustom.searchForCategoryProduct(name, user,registrationStartDate,registrationEndDate,updateStartDate,updateStartDate, sort, sortColumn,
                        pageNumber, pageSize, false);
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (categoryProductPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            List<CategoryProductDTO> categoryProductDTOs = categoryProductPage.getContent().stream()
                    .map(categoryProduct -> CategoryProductDTO.builder()
                            .status(categoryProduct.getStatus())
                            .id(categoryProduct.getId())
                            .user(categoryProduct.getUser().getUsername())
                            .name(categoryProduct.getName())
                            .registrationDate(categoryProduct.getRegistrationDate())
                            .updateDate(categoryProduct.getUpdateDate())
                            .sizeType(categoryProduct.getSizeType().getName())
                            .unitType(categoryProduct.getUnitType().getName())
                            .build())
                    .toList();

            return new PageImpl<>(categoryProductDTOs, categoryProductPage.getPageable(),
                    categoryProductPage.getTotalElements());
        });
    }

    @Override
    public CompletableFuture<List<CategoryProductDTO>> listCategoryProducts() throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<CategoryProduct> categoryProducts;
            try {
                categoryProducts = categoryProductRepository.findAllByStatusTrue();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(categoryProducts.isEmpty()){
                return Collections.emptyList();
            }
            return categoryProducts.stream()
                    .map(categoryProduct -> CategoryProductDTO.builder()
                            .status(categoryProduct.getStatus())
                            .id(categoryProduct.getId())
                            .user(categoryProduct.getUser().getUsername())
                            .name(categoryProduct.getName())
                            .registrationDate(categoryProduct.getRegistrationDate())
                            .updateDate(categoryProduct.getUpdateDate())
                            .sizeType(categoryProduct.getSizeType().getName())
                            .unitType(categoryProduct.getUnitType().getName())
                            .build())
                    .toList();
        });
    }

    @Override
    public CompletableFuture<ResponseDelete> delete(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            CategoryProduct categoryProduct;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                categoryProduct = categoryProductRepository.findByNameAndStatusTrue(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(categoryProduct==null){
                throw new BadRequestExceptions(Constants.ErrorCategoryProduct);
            }
            try {
                categoryProduct.setStatus(false);
                categoryProduct.setUpdateDate(OffsetDateTime.now());
                categoryProduct.setUser(user);
                categoryProduct.setUserId(user.getId());
                categoryProductRepository.save(categoryProduct);
                iAudit.save("DELETE_CATEGORY_PRODUCT","CATEGORIA DE PRODUCTO "+categoryProduct.getName()+" ELIMINADA.", categoryProduct.getName(), user.getUsername());
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
    public CompletableFuture<ResponseSuccess> activate(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            CategoryProduct categoryProduct;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                categoryProduct = categoryProductRepository.findByNameAndStatusFalse(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(categoryProduct==null){
                throw new BadRequestExceptions(Constants.ErrorCategoryProduct);
            }
            try {
                categoryProduct.setStatus(true);
                categoryProduct.setUpdateDate(OffsetDateTime.now());
                categoryProduct.setUser(user);
                categoryProduct.setUserId(user.getId());
                iAudit.save("ACTIVATE_CATEGORY_PRODUCT","CATEGORIA DE PRODUCTO "+categoryProduct.getName()+" ACTIVADA.", categoryProduct.getName(), user.getUsername());
                categoryProductRepository.save(categoryProduct);
                return ResponseSuccess.builder()
                        .message(Constants.update)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<ResponseSuccess> update(String name, String description, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            CategoryProduct categoryProduct;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                categoryProduct = categoryProductRepository.findByNameAndStatusTrue(name);
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(user==null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }
            if(categoryProduct==null){
                throw new BadRequestExceptions(Constants.ErrorCategoryProduct);
            }
            try {
                categoryProduct.setUpdateDate(OffsetDateTime.now());
                categoryProduct.setUser(user);
                categoryProduct.setUserId(user.getId());
                categoryProductRepository.save(categoryProduct);
                iAudit.save("UPDATE_CATEGORY_PRODUCT","CATEGORIA DE PRODUCTO "+categoryProduct.getName()+" EDITADA.",categoryProduct.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .message(Constants.update)
                        .code(200)
                        .build();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<List<CategoryProductDTO>> listFilter() throws InternalErrorExceptions, BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<CategoryProduct> categoryProducts;
            try {
                categoryProducts = categoryProductRepository.findAll();
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
            if(categoryProducts.isEmpty()){
                return Collections.emptyList();
            }
            return categoryProducts.stream()
                    .map(categoryProduct -> CategoryProductDTO.builder()
                            .status(categoryProduct.getStatus())
                            .id(categoryProduct.getId())
                            .user(categoryProduct.getUser().getUsername())
                            .name(categoryProduct.getName())
                            .registrationDate(categoryProduct.getRegistrationDate())
                            .updateDate(categoryProduct.getUpdateDate())
                            .sizeType(categoryProduct.getSizeType().getName())
                            .unitType(categoryProduct.getUnitType().getName())
                            .build())
                    .toList();
        });
    }
}
