package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.State;
import com.proyect.masterdata.dto.StateDTO;
import org.mapstruct.factory.Mappers;

import java.util.List;

public interface StateMapper {
    StateMapper INSTANCE = Mappers.getMapper(StateMapper.class);
    StateDTO stateToStateDTO(State state);
    List<StateDTO> stateListToStateListDTO(List<State> stateList);
}
