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
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.ICategory;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import lombok.extern.log4j.Log4j2;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Date;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Log4j2
public class CategoryImpl implements ICategory {
    private final CategoryRepository categoryRepository;
    private final CategoryMapper categoryMapper;
    private final UserRepository userRepository;

    @Override
    public ResponseSuccess save(String name,String description,String user) throws BadRequestExceptions,InternalErrorExceptions {
        User datauser;
        Category categoryName;
        Category categoryDescription;

        try{
            datauser = userRepository.findById(user.toUpperCase()).orElse(null);
            categoryName = categoryRepository.findByNameAndStatusTrue(name.toUpperCase());
            categoryDescription = categoryRepository.findByDescriptionAndStatusTrue(description.toUpperCase());
        }catch(RuntimeException e){
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if(categoryName!=null){
            throw new BadRequestExceptions(Constants.ErrorCategoryExists.toUpperCase());
        }
        if(categoryDescription!=null){
            throw new BadRequestExceptions(Constants.ErrorCategoryDescriptionExists.toUpperCase());
        }

        try {
            categoryRepository.save(categoryMapper.categoryToName(RequestCategorySave.builder()
                    .name(name.toUpperCase())
                    .description(description.toUpperCase())
                    .user(user.toUpperCase()).build())
            );
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
        }
    }

    @Override
    public ResponseSuccess saveAll(List<RequestCreateCategory> categories,String user) throws BadRequestExceptions{
        User datauser;
        List<Category> categoryListNames;
        List<Category> categoryListDescriptions;

        try{
            datauser = userRepository.findById(user.toUpperCase()).orElse(null);
            categoryListNames = categoryRepository.findByNameIn(categories
                    .stream()
                    .map(category -> category.getName().toUpperCase())
                    .collect(Collectors.toList()));
            categoryListDescriptions = categoryRepository.findByDescriptionIn(categories
                    .stream()
                    .map(category -> category.getDescription().toUpperCase())
                    .collect(Collectors.toList()));
        }catch (RuntimeException e){
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if(!categoryListNames.isEmpty()){
            throw new BadRequestExceptions(Constants.ErrorCategoryList.toUpperCase());
        }
        if(!categoryListDescriptions.isEmpty()){
            throw new BadRequestExceptions(Constants.ErrorCategoryListDescription.toUpperCase());
        }

        try {
            List<RequestCategorySave> categorySaves = categories.stream().map(data -> RequestCategorySave.builder()
                    .user(user.toUpperCase())
                    .name(data.getName().toUpperCase())
                    .description(data.getDescription().toUpperCase())
                    .build()).toList();
            categoryRepository.saveAll(categoryMapper.ListCategoryToListName(categorySaves));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
        }
    }

    @Override
    public CategoryDTO update(RequestCategory requestCategory) throws BadRequestExceptions,InternalErrorExceptions {
        User datauser;
        Category category;

        try{
            datauser = userRepository.findById(requestCategory.getUser().toUpperCase()).orElse(null);
            category = categoryRepository.findById(requestCategory.getCode()).orElse(null);
        }catch (RuntimeException e){
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if(category==null){
            throw new BadRequestExceptions(Constants.ErrorCategory.toUpperCase());
        }

        category.setName(requestCategory.getName().toUpperCase());
        category.setDescription(requestCategory.getDescription().toUpperCase());
        category.setStatus(requestCategory.isStatus());
        category.setDateRegistration(new Date(System.currentTimeMillis()));

        try {
            return categoryMapper.categoryToCategoryDTO(categoryRepository.save(category));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileUpdating);
        }
    }

    @Override
    @Transactional
    public ResponseDelete delete(Long code,String user) throws BadRequestExceptions, InternalErrorExceptions {
        User datauser;
        Category category;
        try{
            datauser = userRepository.findById(user.toUpperCase()).orElse(null);
            category = categoryRepository.findById(code).orElse(null);
        }catch(RuntimeException e){
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }
        if(category==null){
            throw new BadRequestExceptions(Constants.ErrorCategory.toUpperCase());
        }

        try {
            category.setStatus(false);
            category.setDateRegistration(new Date(System.currentTimeMillis()));
            categoryRepository.save(category);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            log.error(e);
            throw new InternalErrorExceptions(Constants.InternalErrorExceptions);
        }
    }

    @Override
    public List<CategoryDTO> list() throws BadRequestExceptions{
        List<Category> categories = new ArrayList<>();
        try {
            categories = categoryRepository.findAllByStatusTrue();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
        if(categories.isEmpty()){
            return Collections.emptyList();
        }
        return categoryMapper.listCategoryToListCategoryDTO(categories);
    }

    public List<CategoryDTO> listStatusFalse() throws BadRequestExceptions{
        try {
            return categoryMapper.listCategoryToListCategoryDTO(categoryRepository.findAllByStatusFalse());
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public CategoryDTO findByCode(Long code) throws BadRequestExceptions{
        try {
            return categoryMapper.categoryToCategoryDTO(categoryRepository.findByIdAndStatusTrue(code));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

}
