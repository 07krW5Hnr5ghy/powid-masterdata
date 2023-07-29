package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.Size;
import com.proyect.masterdata.dto.MasterListDTO;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface SizeMapper {
     SizeMapper INSTANCE = Mappers.getMapper(SizeMapper.class);
     MasterListDTO sizeToSizeDTO(Size size);
     List<MasterListDTO> sizeListToSizeListDTO(List<Size> sizeList);
}
