package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.State;
import com.proyect.masterdata.dto.StateDTO;
import com.proyect.masterdata.dto.request.RequestState;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface StateMapper {
    StateMapper INSTANCE = Mappers.getMapper(StateMapper.class);
    @Mapping(target = "code", source = "id")
    StateDTO stateToStateDTO(State state);
    List<StateDTO> listStateToListStateDTO(List<State> stateList);
    @Mapping(target = "id", ignore = true)
    @Mapping(target = "status", constant = "true")
    @Mapping(target = "dateRegistration", ignore = true)
    @Mapping(target = "name", source = "name")
    State stateToName(String name);

    List<State> listStateToListName(List<String> names);

    @Mapping(target = "id", source = "code")
    @Mapping(target = "dateRegistration", ignore = true)
    State requestStateToState(RequestState requestState);
}
