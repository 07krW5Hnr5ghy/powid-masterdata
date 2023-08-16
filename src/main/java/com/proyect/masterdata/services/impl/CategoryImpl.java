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
import com.proyect.masterdata.mapper.CategoryMapper;
import com.proyect.masterdata.repository.CategoryRepository;
import com.proyect.masterdata.repository.UserRepository;
import com.proyect.masterdata.services.ICategory;
import com.proyect.masterdata.utils.Constants;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Date;
import java.util.List;

@Service
@RequiredArgsConstructor
public class CategoryImpl implements ICategory {
    private final CategoryRepository categoryRepository;
    private final CategoryMapper categoryMapper;
    private final UserRepository userRepository;

    @Override
    public ResponseSuccess save(String name,String description,String user) throws BadRequestExceptions {
        User datauser = userRepository.findById(user).orElse(null);

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        try {
            categoryRepository.save(categoryMapper.categoryToName(name.toUpperCase(),description.toUpperCase(),user.toUpperCase()));
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
        User datauser = userRepository.findById(user).orElse(null);

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        try {
            List<RequestCategorySave> categorySaves = categories.stream().map(data -> RequestCategorySave.builder()
                    .user(user)
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
    public CategoryDTO update(RequestCategory requestCategory) throws BadRequestExceptions {
        try {
            requestCategory.setName(requestCategory.getName().toUpperCase());
            requestCategory.setDescription(requestCategory.getDescription().toUpperCase());
            Category updatedCategory = categoryMapper.requestCategoryToCategory(requestCategory);
            updatedCategory.setDateRegistration(new Date(System.currentTimeMillis()));
            Category category = categoryRepository.save(categoryMapper.requestCategoryToCategory(requestCategory));
            return categoryMapper.categoryToCategoryDTO(category);
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhileUpdating);
        }
    }

    @Override
    @Transactional
    public ResponseDelete delete(Long code,String user) throws BadRequestExceptions{
        User datauser = userRepository.findById(user).orElse(null);

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        try {
            categoryRepository.deleteByIdAndUser(code,user);
            return ResponseDelete.builder()
                    .code(200)
                    .message(Constants.delete)
                    .build();
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ErrorWhenDeleting);
        }
    }

    @Override
    public ResponseDelete deleteAll(List<Long> codes,String user) throws BadRequestExceptions{
        User datauser = userRepository.findById(user).orElse(null);

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

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
            return categoryMapper.listCategoryToListCategoryDTO(categoryRepository.findAllByStatusTrue());
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
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

    @Override
    public CategoryDTO findByName(String name) throws BadRequestExceptions{
        try {
            return categoryMapper.categoryToCategoryDTO(categoryRepository.findByNameAndStatusTrue(name.toUpperCase()));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }

    @Override
    public List<CategoryDTO> findByUser(String user) throws BadRequestExceptions{
        User datauser = userRepository.findById(user).orElse(null);

        if (datauser==null){
            throw new BadRequestExceptions(Constants.ErrorUser.toUpperCase());
        }

        try {
            return categoryMapper.listCategoryToListCategoryDTO(categoryRepository.findByUser(user.toUpperCase()));
        } catch (RuntimeException e){
            throw new BadRequestExceptions(Constants.ResultsFound);
        }
    }
}
