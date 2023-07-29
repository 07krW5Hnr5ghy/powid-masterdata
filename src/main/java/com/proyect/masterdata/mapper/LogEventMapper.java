package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.LogEvent;
import com.proyect.masterdata.dto.MasterListDTO;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface LogEventMapper {
    LogEventMapper INSTANCE = Mappers.getMapper(LogEventMapper.class);
    @Mapping(source = "id",target="id")
    MasterListDTO logEventToLogEventDTO(LogEvent logEvent);
    List<MasterListDTO> logEventListToLogEventListDTO(List<LogEvent> logEventList);
}
