package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.Connection;
import com.proyect.masterdata.dto.ConnectionDTO;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface ConnectionMapper {
    ConnectionMapper INSTANCE = Mappers.getMapper(ConnectionMapper.class);
    List<ConnectionDTO> listConnectionToListConnectionDTO(List<Connection> connectionList);
}
