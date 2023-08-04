package com.proyect.masterdata.mapper;


import com.proyect.masterdata.domain.LogEvent;
import com.proyect.masterdata.dto.LogEventDTO;
import com.proyect.masterdata.dto.request.RequestLogEvent;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface LogEventMapper {
    LogEventMapper INSTANCE = Mappers.getMapper(LogEventMapper.class);
    @Mapping(target = "code", source = "id")
    LogEventDTO logEventToLogEventDTO(LogEvent logEvent);
    List<LogEventDTO> listLogEventToListLogEventDTO(List<LogEvent> logEventList);
    @Mapping(target = "id", ignore = true)
    @Mapping(target = "status", constant = "true")
    @Mapping(target = "dateRegistration", ignore = true)
    @Mapping(target = "name", source = "name")
    LogEvent logEventToName(String name);

    List<LogEvent> listLogEventToListName(List<String> names);

    @Mapping(target = "id", source = "code")
    @Mapping(target = "dateRegistration", ignore = true)
    LogEvent requestLogEventToLogEvent(RequestLogEvent requestLogEvent);
}
