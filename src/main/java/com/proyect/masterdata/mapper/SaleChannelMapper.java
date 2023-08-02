package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.PaymentState;
import com.proyect.masterdata.domain.SaleChannel;
import com.proyect.masterdata.dto.SaleChannelDTO;
import com.proyect.masterdata.dto.request.RequestSaleChannel;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface SaleChannelMapper {
    SaleChannelMapper INSTANCE = Mappers.getMapper(SaleChannelMapper.class);
    @Mapping(source="code",target = "id")
    SaleChannelDTO saleChannelToSaleChannelDTO(PaymentState paymentState);
    List<SaleChannelDTO> saleChannelListToSaleChannelListDTO(List<SaleChannel> saleChannelList);
    @Mapping(target = "id", ignore = true)
    @Mapping(target = "status", constant = "true")
    @Mapping(target = "dateRegistration", ignore = true)
    @Mapping(target = "name", source = "name")
    SaleChannel saleChannelToName(String name);

    List<SaleChannel> saleChannelToListName(List<String> names);

    @Mapping(target = "id", source = "code")
    @Mapping(target = "dateRegistration", ignore = true)
    SaleChannel requestSaleChannelToSaleChannel(RequestSaleChannel requestSaleChannel);
}
