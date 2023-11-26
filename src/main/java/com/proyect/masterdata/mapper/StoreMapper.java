package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.Store;
import com.proyect.masterdata.dto.StoreDTO;
import com.proyect.masterdata.dto.request.RequestStoreSave;
import com.proyect.masterdata.dto.request.RequestModuleSave;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface StoreMapper {
    StoreMapper INSTANCE = Mappers.getMapper(StoreMapper.class);

    @Mapping(target = "code", source = "id")
    @Mapping(target = "name", source = "name")
    @Mapping(target = "url", source = "url")
    StoreDTO clientChannelToClientChannelDTO(Store clientChannel);

    List<StoreDTO> listClientChannelToListClientChannelDTO(List<Store> clientChannelList);

    @Mapping(target = "id", ignore = true)
    @Mapping(target = "name", source = "requestClientChannelSave.name")
    @Mapping(target = "url", source = "requestClientChannelSave.url")
    Store clientChannelToName(RequestStoreSave requestClientChannelSave);

    List<Store> listClientChannelToListName(List<RequestStoreSave> requestClientChannelSaveList);
}
