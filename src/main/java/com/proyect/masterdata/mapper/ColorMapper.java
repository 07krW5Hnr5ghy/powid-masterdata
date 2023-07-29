package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.Color;
import com.proyect.masterdata.dto.MasterListDTO;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface ColorMapper {
    ColorMapper INSTANCE = Mappers.getMapper(ColorMapper.class);
    @Mapping(source="id",target = "id")
    MasterListDTO colorToColorDTO(Color color);
    List<MasterListDTO> colorListToColorListDTO(List<Color> colorList);
}
