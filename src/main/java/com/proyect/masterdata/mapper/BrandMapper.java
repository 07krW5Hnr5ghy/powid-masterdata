package com.proyect.masterdata.mapper;

import java.util.List;

import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

import com.proyect.masterdata.domain.Brand;
import com.proyect.masterdata.dto.BrandDTO;

@Mapper(componentModel = "spring")
public interface BrandMapper {

    BrandMapper INSTANCE = Mappers.getMapper(BrandMapper.class);

    List<BrandDTO> listBrandToListBrandDTO(List<Brand> brandList);
}
