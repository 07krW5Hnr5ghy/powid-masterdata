package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.SaleChannel;
import com.proyect.masterdata.dto.SaleChannelDTO;
import com.proyect.masterdata.dto.request.RequestSaleChannelSave;
import com.proyect.masterdata.dto.request.RequestSaleChannel;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface SaleChannelMapper {
    SaleChannelMapper INSTANCE = Mappers.getMapper(SaleChannelMapper.class);

    @Mapping(target = "code", source = "id")
    SaleChannelDTO saleChannelToSaleChannelDTO(SaleChannel saleChannel);

    List<SaleChannelDTO> listSaleChannelToListSaleChannelDTO(List<SaleChannel> saleChannelList);

    @Mapping(target = "id", ignore = true)
    @Mapping(target = "status", constant = "true")
    @Mapping(target = "dateRegistration", ignore = true)
    @Mapping(target = "name", source = "requestSaleChannelSave.name")
    @Mapping(target = "tokenUser", source = "requestSaleChannelSave.user")
    SaleChannel saleChannelToName(RequestSaleChannelSave requestSaleChannelSave);

    List<SaleChannel> listSaleChannelToListName(List<RequestSaleChannelSave> requestSaleChannelSaveList);
}
