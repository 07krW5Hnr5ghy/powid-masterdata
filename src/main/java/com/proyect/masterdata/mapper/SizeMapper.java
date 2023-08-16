package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.Size;
import com.proyect.masterdata.dto.SizeDTO;
import com.proyect.masterdata.dto.request.RequestSize;
import com.proyect.masterdata.dto.request.RequestSizeSave;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface SizeMapper {
     SizeMapper INSTANCE = Mappers.getMapper(SizeMapper.class);
     @Mapping(target = "code", source = "id")
     SizeDTO sizeToSizeDTO(Size size);
     List<SizeDTO> listSizeToListSizeDTO(List<Size> sizeList);
     @Mapping(target = "id", ignore = true)
     @Mapping(target = "status", constant = "true")
     @Mapping(target = "dateRegistration", ignore = true)
     @Mapping(target = "name", source = "requestSizeSave.name")
     @Mapping(target = "user", source = "requestSizeSave.user")
     @Mapping(target = "idSizeType", source = "requestSizeSave.codeSizeType")
     Size sizeToName(RequestSizeSave requestSizeSave);

     @Mapping(target = "id", source = "code")
     @Mapping(target = "dateRegistration", ignore = true)
     Size requestSizeToSize(RequestSize requestSize);

     List<Size> listSizeToListName(List<RequestSizeSave> requestSizeSaveList);

}
