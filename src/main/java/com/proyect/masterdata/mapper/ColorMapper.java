package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.Color;
import com.proyect.masterdata.dto.ColorDTO;
import com.proyect.masterdata.dto.request.RequestColorSave;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface ColorMapper {
    ColorMapper INSTANCE = Mappers.getMapper(ColorMapper.class);

    ColorDTO colorToColorDTO(Color color);

    List<ColorDTO> listColorToListColorDTO(List<Color> colorList);

    @Mapping(target = "name", source = "requestColorSave.name")
    @Mapping(target = "tokenUser", source = "requestColorSave.user")
    Color colorToName(RequestColorSave requestColorSave);

    List<Color> listColorToListName(List<RequestColorSave> requestColorSaveList);

}
