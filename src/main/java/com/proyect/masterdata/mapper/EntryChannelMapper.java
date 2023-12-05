package com.proyect.masterdata.mapper;

import java.util.List;

import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

import com.proyect.masterdata.domain.EntryChannel;
import com.proyect.masterdata.dto.EntryChannelDTO;

@Mapper(componentModel = "spring")
public interface EntryChannelMapper {
    EntryChannelMapper INSTANCE = Mappers.getMapper(EntryChannelMapper.class);

    List<EntryChannelDTO> listEntryChannelToListEntryChannelDTO(List<EntryChannel> entryChannelList);
}
