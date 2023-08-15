package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.Category;
import com.proyect.masterdata.dto.CategoryDTO;
import com.proyect.masterdata.dto.request.RequestCategory;
import com.proyect.masterdata.dto.request.RequestCategorySave;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface CategoryMapper {
    CategoryMapper INSTANCE = Mappers.getMapper(CategoryMapper.class);
    @Mapping(target = "code", source = "id")
    CategoryDTO categoryToCategoryDTO(Category category);

    List<CategoryDTO> categoryListToCategoryListDTO(List<Category> categoryList);

    @Mapping(target = "id", ignore = true)
    @Mapping(target = "status", constant = "true")
    @Mapping(target = "dateRegistration", ignore = true)
    @Mapping(target = "name", source = "name")
    Category categoryToName(String name,String description);

    @Mapping(target = "id", source = "code")
    @Mapping(target = "dateRegistration", ignore = true)
    Category requestCategoryToCategory(RequestCategory requestCategory);

    @Mapping(target = "id", ignore = true)
    @Mapping(target = "status", constant = "true")
    @Mapping(target = "dateRegistration", ignore = true)
    @Mapping(target = "name", source = "name")
    List<Category> listRequestCreateCategoryToListCategory(List<RequestCategorySave> requestCategorySaveList);

}
