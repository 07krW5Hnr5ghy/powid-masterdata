package com.proyect.masterdata.mapper;

import java.util.List;

import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

import com.proyect.masterdata.domain.ClosingChannel;
import com.proyect.masterdata.dto.ClosingChannelDTO;

@Mapper(componentModel = "spring")
public interface ClosingChannelMapper {
    ClosingChannelMapper INSTANCE = Mappers.getMapper(ClosingChannelMapper.class);

    List<ClosingChannelDTO> listClosingChannelToListClosindChannelDTO(List<ClosingChannel> closingChannelList);
}
