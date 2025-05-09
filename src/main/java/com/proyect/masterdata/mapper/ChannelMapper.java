package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.Channel;
import com.proyect.masterdata.dto.ChannelDTO;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface ChannelMapper {
    ChannelMapper INSTANCE = Mappers.getMapper(ChannelMapper.class);
}
