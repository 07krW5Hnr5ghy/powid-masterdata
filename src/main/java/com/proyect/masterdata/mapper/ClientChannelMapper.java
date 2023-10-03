package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.ClientChannel;
import com.proyect.masterdata.dto.ClientChannelDTO;
import com.proyect.masterdata.dto.request.RequestClientChannelSave;
import com.proyect.masterdata.dto.request.RequestModuleSave;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface ClientChannelMapper {
    ClientChannelMapper INSTANCE = Mappers.getMapper(ClientChannelMapper.class);
    @Mapping(target="code",source = "id")
    @Mapping(target="name",source = "name")
    @Mapping(target="url",source = "url")
    ClientChannelDTO clientChannelToClientChannelDTO(ClientChannel clientChannel);
    List<ClientChannelDTO> listClientChannelToListClientChannelDTO(List<ClientChannel> clientChannelList);
    @Mapping(target = "id",ignore = true)
    @Mapping(target = "name", source = "requestClientChannelSave.name")
    @Mapping(target = "url", source = "requestClientChannelSave.url")
    @Mapping(target = "status")
    @Mapping(target = "dateRegistration",ignore = true)
    ClientChannel clientChannelToName(RequestClientChannelSave requestClientChannelSave);

    List<ClientChannel> listClientChannelToListName(List<RequestClientChannelSave> requestClientChannelSaveList);
}
