package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.Color;
import com.proyect.masterdata.dto.ColorDTO;
import com.proyect.masterdata.dto.MasterListDTO;
import com.proyect.masterdata.dto.request.RequestColor;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface ColorMapper {
    ColorMapper INSTANCE = Mappers.getMapper(ColorMapper.class);
    @Mapping(source="code",target = "id")
    ColorDTO colorToColorDTO(Color color);
    List<ColorDTO> colorListToColorListDTO(List<Color> colorList);
    @Mapping(target = "id", ignore = true)
    @Mapping(target = "status", constant = "true")
    @Mapping(target = "dateRegistration", ignore = true)
    @Mapping(target = "name", source = "name")
    Color colorToName(String name);

    List<Color> listColorToListName(List<String> names);

    @Mapping(target = "id", source = "code")
    @Mapping(target = "dateRegistration", ignore = true)
    Color requestColorToColor(RequestColor requestColor);
}
