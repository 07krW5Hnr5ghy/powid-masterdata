package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Category;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.CategoryDTO;
import com.proyect.masterdata.dto.request.RequestCategory;
import com.proyect.masterdata.dto.request.RequestCategorySave;
import com.proyect.masterdata.dto.request.RequestCreateCategory;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import com.proyect.masterdata.mapper.CategoryMapper;
import com.proyect.masterdata.repository.CategoryRepository;
import com.proyect.masterdata.repository.CategoryRepositoryCustom;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.IAudit;
import com.proyect.masterdata.services.ICategory;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.OffsetDateTime;
import java.util.Date;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Log4j2
public class CategoryImpl implements ICategory {
    private final CategoryRepository categoryRepository;
    private final CategoryMapper categoryMapper;
    private final UserRepository userRepository;
    private final CategoryRepositoryCustom categoryRepositoryCustom;
    private final IAudit iAudit;
    @Override
    public ResponseSuccess save(String name, String description, String tokenUser)
            throws BadRequestExceptions, InternalErrorExceptions {

        User user;
        Category categoryName;
        Category categoryDescription;

        try {
            user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
            categoryName = categoryRepository.findByName(name.toUpperCase());
            categoryDescription = categoryRepository.findByDescription(description.toUpperCase());
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (user == null) {
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if (categoryName != null) {
            throw new BadRequestExceptions(Constants.ErrorCategoryExists.toUpperCase());
        }
        if (categoryDescription != null) {
            throw new BadRequestExceptions(Constants.ErrorCategoryDescriptionExists.toUpperCase());
        }

        try {
            Category category = categoryRepository.save(Category.builder()
                    .name(name.toUpperCase())
                    .description(description.toUpperCase())
                    .status(true)
                    .user(user)
                    .userId(user.getId())
                    .build()
            );
            iAudit.save("ADD_CATEGORY","CATEGORIA "+category.getName()+" CREADA.",category.getName(),user.getUsername());
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e) {
            log.error(e.getMessage());
            throw new BadRequestExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public CompletableFuture<ResponseSuccess> saveAsync(String name, String description, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Category categoryName;
            Category categoryDescription;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                categoryName = categoryRepository.findByName(name.toUpperCase());
                categoryDescription = categoryRepository.findByDescription(description.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }
            if (categoryName != null) {
                throw new BadRequestExceptions(Constants.ErrorCategoryExists.toUpperCase());
            }
            if (categoryDescription != null) {
                throw new BadRequestExceptions(Constants.ErrorCategoryDescriptionExists.toUpperCase());
            }

            try {
                Category category = categoryRepository.save(Category.builder()
                        .name(name.toUpperCase())
                        .description(description.toUpperCase())
                        .status(true)
                        .user(user)
                                .userId(user.getId())
                        .build());
                iAudit.save("ADD_CATEGORY","CATEGORIA "+category.getName()+" CREADA.",category.getName(),user.getUsername());
                return ResponseSuccess.builder()
                        .code(200)
                        .message(Constants.register)
                        .build();
            } catch (RuntimeException e) {
                log.error(e.getMessage());
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
        });
    }

    @Override
    public CompletableFuture<CategoryDTO> update(RequestCategory requestCategory, String tokenUser)
            throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{

            User user;
            Category category;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                category = categoryRepository.findByNameAndStatusTrue(requestCategory.getName().toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }
            if (category == null) {
                throw new BadRequestExceptions(Constants.ErrorCategory.toUpperCase());
            }

            try {
                category.setDescription(requestCategory.getDescription().toUpperCase());
                category.setUser(user);
                category.setUserId(user.getId());
                category.setUpdateDate(OffsetDateTime.now());
                iAudit.save("UPDATE_CATEGORY","CATEGORIA "+category.getName()+" EDITADA.", category.getName(), user.getUsername());
                return categoryMapper.categoryToCategoryDTO(categoryRepository.save(category));
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.InternalErrorExceptions);
            }
        });
    }
    @Override
    @Transactional
    public CompletableFuture<ResponseDelete> delete(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(() -> {

            User user;
            Category category;

            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                category = categoryRepository.findByNameAndStatusTrue(name.toUpperCase());
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if (user == null) {
                throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
            }

            if (category == null) {
                throw new BadRequestExceptions(Constants.ErrorCategory.toUpperCase());
            }

            try {
                category.setStatus(false);
                category.setRegistrationDate(OffsetDateTime.now());
                categoryRepository.save(category);
                iAudit.save("DELETE_CATEGORY","CATEGORIA "+category.getName()+" DESACTIVADA.", category.getName(), user.getUsername());
                return ResponseDelete.builder()
                        .code(200)
                        .message(Constants.delete)
                        .build();
            } catch (RuntimeException e) {
                log.error(e);
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }
        });
    }
    @Override
    public CompletableFuture<List<CategoryDTO>> listCategory() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Category> categories;

            try {
                categories = categoryRepository.findAllByStatusTrue();
            } catch (RuntimeException e) {
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (categories.isEmpty()) {
                return Collections.emptyList();
            }

            return categoryMapper.listCategoryToListCategoryDTO(categories);
        });
    }
    @Override
    public CompletableFuture<Page<CategoryDTO>> list(String name, String user, OffsetDateTime registrationStartDate, OffsetDateTime registrationEndDate, OffsetDateTime updateStartDate, OffsetDateTime updateEndDate, String sort, String sortColumn, Integer pageNumber,
                                                     Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<Category> categoryPage;
            try {
                categoryPage = categoryRepositoryCustom.searchForCategory(name, user,registrationStartDate,registrationEndDate,updateStartDate,updateStartDate, sort, sortColumn, pageNumber,
                        pageSize, true);
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }
            if (categoryPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }
            return new PageImpl<>(categoryMapper.listCategoryToListCategoryDTO(categoryPage.getContent()),
                    categoryPage.getPageable(), categoryPage.getTotalElements());
        });
    }
    @Override
    public CompletableFuture<Page<CategoryDTO>> listStatusFalse(String name, String user,OffsetDateTime registrationStartDate, OffsetDateTime registrationEndDate, OffsetDateTime updateStartDate, OffsetDateTime updateEndDate, String sort, String sortColumn,
            Integer pageNumber, Integer pageSize) throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            Page<Category> categoryPage;
            try {
                categoryPage = categoryRepositoryCustom.searchForCategory(name, user,registrationStartDate,registrationEndDate,updateStartDate,updateStartDate, sort, sortColumn, pageNumber,
                        pageSize, false);
            } catch (RuntimeException e) {
                log.error(e);
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (categoryPage.isEmpty()) {
                return new PageImpl<>(Collections.emptyList());
            }

            return new PageImpl<>(categoryMapper.listCategoryToListCategoryDTO(categoryPage.getContent()),
                    categoryPage.getPageable(), categoryPage.getTotalElements());
        });
    }
    @Override
    public CompletableFuture<ResponseSuccess> activate(String name, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions {
        return CompletableFuture.supplyAsync(()->{
            User user;
            Category category;
            try {
                user = userRepository.findByUsernameAndStatusTrue(tokenUser.toUpperCase());
                category = categoryRepository.findByNameAndStatusTrue(name.toUpperCase());
            }catch (RuntimeException e){
                log.error(e.getMessage());
                throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
            }

            if(user == null){
                throw new BadRequestExceptions(Constants.ErrorUser);
            }

            if(category == null){
                throw new BadRequestExceptions(Constants.ErrorCategory);
            }

            try {
                category.setStatus(false);
                category.setUpdateDate(OffsetDateTime.now());
                category.setUser(user);
                category.setUserId(user.getId());
                iAudit.save("ACTIVATE_CATEGORY","CATEGORIA "+category.getName()+" ACTIVADA.", category.getName(), user.getUsername());
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
    public CompletableFuture<List<CategoryDTO>> listFilter() throws BadRequestExceptions {
        return CompletableFuture.supplyAsync(()->{
            List<Category> categories;

            try {
                categories = categoryRepository.findAll();
            } catch (RuntimeException e) {
                throw new BadRequestExceptions(Constants.ResultsFound);
            }

            if (categories.isEmpty()) {
                return Collections.emptyList();
            }

            return categoryMapper.listCategoryToListCategoryDTO(categories);
        });
    }

}
