package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.SaleChannel;
import com.proyect.masterdata.dto.MasterListDTO;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface SaleChannelMapper {
    SaleChannelMapper INSTANCE = Mappers.getMapper(SaleChannelMapper.class);
    @Mapping(source="id",target = "id")
    @Mapping(source="name",target = "name")
    @Mapping(source = "status",target = "status")
    MasterListDTO saleChannelToSaleChannelDTO(SaleChannel saleChannel);
    List<MasterListDTO> saleChannelListToSaleChannelListDTO(List<SaleChannel> saleChannelList);
}
