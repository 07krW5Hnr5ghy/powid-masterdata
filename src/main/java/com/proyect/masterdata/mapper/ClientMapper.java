package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.Client;
import com.proyect.masterdata.dto.request.RequestClientSave;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface ClientMapper {
    ClientMapper INSTANCE = Mappers.getMapper(ClientMapper.class);
    List<Client> listClientToListName(List<RequestClientSave> requestClientSaveList);
}
