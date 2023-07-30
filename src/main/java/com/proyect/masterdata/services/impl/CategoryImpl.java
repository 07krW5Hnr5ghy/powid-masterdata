package com.proyect.masterdata.services.impl;

import com.proyect.masterdata.domain.Category;
import com.proyect.masterdata.domain.Color;
import com.proyect.masterdata.dto.CategoryDTO;
import com.proyect.masterdata.dto.response.ResponseMasterList;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.mapper.CategoryMapper;
import com.proyect.masterdata.repository.CategoryRepository;
import com.proyect.masterdata.services.ICategory;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.sql.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
public class CategoryImpl implements ICategory {
    private final CategoryRepository categoryRepository;
    private final CategoryMapper categoryMapper;

    @Override
    public List<CategoryDTO> listRecords() throws BadRequestExceptions {
        return categoryMapper.INSTANCE.categoryListToCategoryListDTO(categoryRepository.findAll());
    }

    @Override
    public ResponseMasterList addRecord(String name, String description) throws BadRequestExceptions {
        try{
            categoryRepository.save(Category.builder()
                    .name(name)
                    .description(description)
                    .status(true)
                    .build()
            );
            return ResponseMasterList.builder()
                    .code(200)
                    .message("Success")
                    .build();
        }catch (RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }

    @Override
    public ResponseMasterList deleteRecord(Long id) throws BadRequestExceptions {
        try{
            Category category = categoryRepository.findById(id).get();
            categoryRepository.save(Category.builder()
                    .name(category.getName())
                    .description(category.getDescription())
                    .dateRegistration(new Date(System.currentTimeMillis()))
                    .id(category.getId())
                    .status(false)
                    .build()
            );
            return ResponseMasterList.builder()
                    .code(200)
                    .message("Success")
                    .build();
        }catch (RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }

    @Override
    public CategoryDTO updateRecord(String name, Long id, String description) throws BadRequestExceptions {
        try{
            Category category = categoryRepository.save(Category.builder()
                    .id(id)
                    .dateRegistration(new Date(System.currentTimeMillis()))
                    .name(name)
                    .description(description)
                    .status(true)
                    .build()
            );
            return categoryMapper.INSTANCE.categoryToCategoryDTO(category);
        }catch (RuntimeException ex){
            throw new BadRequestExceptions(ex.getMessage());
        }
    }
}
