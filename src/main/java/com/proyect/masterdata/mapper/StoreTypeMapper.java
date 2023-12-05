package com.proyect.masterdata.mapper;

import java.util.List;

import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

import com.proyect.masterdata.domain.StoreType;
import com.proyect.masterdata.dto.StoreTypeDTO;

@Mapper(componentModel = "spring")
public interface StoreTypeMapper {
    StoreTypeMapper INSTANCE = Mappers.getMapper(StoreTypeMapper.class);

    List<StoreTypeDTO> listStoreTypeToListStoreTypeDTO(List<StoreType> storeTypeList);
}
