package com.proyect.masterdata.mapper;

import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;
import com.proyect.masterdata.dto.ModelDTO;
import com.proyect.masterdata.domain.Model;

import java.util.List;

@Mapper(componentModel = "spring")
public interface ModelMapper {

    ModelMapper INSTANCE = Mappers.getMapper(ModelMapper.class);

    List<ModelDTO> listModelToListModelDTO(List<Model> modelList);

}
