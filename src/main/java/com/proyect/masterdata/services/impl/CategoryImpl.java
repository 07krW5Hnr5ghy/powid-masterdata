package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Category;
import com.proyect.masterdata.dto.CategoryDTO;
import com.proyect.masterdata.dto.request.RequestCategory;
import com.proyect.masterdata.dto.request.RequestCreateCategory;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mapper.CategoryMapper;
import com.proyect.masterdata.repository.CategoryRepository;
import com.proyect.masterdata.services.ICategory;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class CategoryImpl implements ICategory {
    private final CategoryRepository categoryRepository;
    private final CategoryMapper categoryMapper;

    @Override
    public ResponseSuccess save(String name,String description) throws BadRequestExceptions {
        try {
            categoryRepository.save(categoryMapper.categoryToName(name.toUpperCase(),description.toUpperCase()));
            return ResponseSuccess.builder()
                    .code(200)
                    .message(Constants.register)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileRegistering);
        }
    }

    @Override
    public ResponseSuccess saveAll(List<RequestCreateCategory> requestCategoryList) throws BadRequestExceptions{
        try {
            categoryRepository.saveAll(categoryMapper.listRequestCreateCategoryToListCategory(requestCategoryList)
                    .stream().map(
                            c -> {
                                Category category = new Category();
                                category.setName(c.getName().toUpperCase());
                                category.setDescription(c.getDescription().toUpperCase());
                                category.setStatus(true);
                                return category;
                            }
                    ).collect(Collectors.toList())
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
    public CategoryDTO update(RequestCategory requestCategory) throws BadRequestExceptions {
        try {
            requestCategory.setName(requestCategory.getName().toUpperCase());
            requestCategory.setDescription(requestCategory.getDescription().toUpperCase());
            Category category = categoryRepository.save(categoryMapper.requestCategoryToCategory(requestCategory));
            return categoryMapper.categoryToCategoryDTO(category);
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileUpdating);
        }
    }

    @Override
    public ResponseDelete delete(Long code) throws BadRequestExceptions{
        try {
            categoryRepository.deleteById(code);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
        }
    }

    @Override
    public ResponseDelete deleteAll(List<Long> codes) throws BadRequestExceptions{
        try {
            categoryRepository.deleteAllById(codes);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
        }
    }

    @Override
    public List<CategoryDTO> list() throws BadRequestExceptions{
        try {
            return categoryMapper.categoryListToCategoryListDTO(categoryRepository.findAll());
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public CategoryDTO findByCode(Long code) throws BadRequestExceptions{
        try {
            return categoryMapper.categoryToCategoryDTO(categoryRepository.findById(code).orElse(null));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public CategoryDTO findByName(String name) throws BadRequestExceptions{
        try {
            return categoryMapper.categoryToCategoryDTO(categoryRepository.findByName(name.toUpperCase()));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }
}
