package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.Category;
import com.proyect.masterdata.dto.CategoryDTO;
import com.proyect.masterdata.dto.request.RequestCategorySave;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface CategoryMapper {
    CategoryMapper INSTANCE = Mappers.getMapper(CategoryMapper.class);

    CategoryDTO categoryToCategoryDTO(Category category);

    List<CategoryDTO> listCategoryToListCategoryDTO(List<Category> categoryList);

    @Mapping(target = "name", source = "requestCategorySave.name")
    @Mapping(target = "description", source = "requestCategorySave.description")
    @Mapping(target = "tokenUser", source = "requestCategorySave.user")
    Category categoryToName(RequestCategorySave requestCategorySave);

    List<Category> ListCategoryToListName(List<RequestCategorySave> requestCategorySaveList);

}
