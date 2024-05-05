package com.proyect.masterdata.services.impl;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;

import com.proyect.masterdata.domain.CategoryProduct;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.CategoryProductDTO;
import com.proyect.masterdata.dto.request.RequestCategoryProduct;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.repository.CategoryProductRepository;
import com.proyect.masterdata.repository.CategoryProductRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.ICategoryProduct;
import com.proyect.masterdata.utils.Constants;

import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;

@Service
@RequiredArgsConstructor
@Log4j2
public class CategoryProductImpl implements ICategoryProduct {

    private final UserRepository userRepository;
    private final CategoryProductRepository categoryProductRepository;
    private final CategoryProductRepositoryCustom categoryProductRepositoryCustom;

    @Override
    public ResponseSuccess save(String name, String description, String tokenUser)
            throws BadRequestExceptions, InternalErrorExceptions {

        User user;
        CategoryProduct categoryProduct;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            categoryProduct = categoryProductRepository.findByName(name.toUpperCase());
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

        try {
            categoryProductRepository.save(CategoryProduct.builder()
                    .description(description.toUpperCase())
                    .name(name.toUpperCase())
                    .registrationDate(new Date(System.currentTimeMillis()))
                    .status(true)
                    .tokenUser(tokenUser.toUpperCase())
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
    public CompletableFuture<ResponseSuccess> saveAsync(String name, String description, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            CategoryProduct categoryProduct;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                categoryProduct = categoryProductRepository.findByName(name.toUpperCase());
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

            try {
                categoryProductRepository.save(CategoryProduct.builder()
                        .description(description.toUpperCase())
                        .name(name.toUpperCase())
                        .registrationDate(new Date(System.currentTimeMillis()))
                        .status(true)
                        .tokenUser(tokenUser.toUpperCase())
                        .build());

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
    public ResponseSuccess saveAll(List<RequestCategoryProduct> categoryProductList, String tokenUser)
            throws BadRequestExceptions, InternalErrorExceptions {

        User user;
        List<CategoryProduct> categoryProducts;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            categoryProducts = categoryProductRepository.findByNameIn(categoryProductList.stream()
                    .map(categoryProduct -> categoryProduct.getName().toUpperCase()).toList());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser);
        }

        if (!categoryProducts.isEmpty()) {
            throw new BadRequestExceptions(Constants.ErrorCategoryProductExists);
        }

        try {
            categoryProductRepository.saveAll(categoryProductList.stream()
                    .map(categoryProduct -> CategoryProduct.builder()
                            .description(categoryProduct.getDescription().toUpperCase())
                            .name(categoryProduct.getName().toUpperCase())
                            .registrationDate(new Date(System.currentTimeMillis()))
                            .status(true)
                            .tokenUser(tokenUser.toUpperCase())
                            .build())
                    .toList());

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
    public CompletableFuture<Page<CategoryProductDTO>> list(String name, String user, String sort, String sortColumn, Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{

            Page<CategoryProduct> categoryProductPage;

            try {
                categoryProductPage = categoryProductRepositoryCustom.searchForCategoryProduct(name, user, sort, sortColumn,
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
                            .description(categoryProduct.getDescription())
                            .name(categoryProduct.getName())
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
                            .description(categoryProduct.getDescription())
                            .name(categoryProduct.getName())
                            .build())
                    .toList();
        });
    }
}
