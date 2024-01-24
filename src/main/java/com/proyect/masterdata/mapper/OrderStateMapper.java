package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.OrderState;
import com.proyect.masterdata.dto.OrderStateDTO;
import com.proyect.masterdata.dto.request.RequestStateSave;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface OrderStateMapper {
    OrderStateMapper INSTANCE = Mappers.getMapper(OrderStateMapper.class);

    @Mapping(target = "code", source = "id")
    OrderStateDTO stateToStateDTO(OrderState state);

    List<OrderStateDTO> listStateToListStateDTO(List<OrderState> stateList);

    @Mapping(target = "id", ignore = true)
    @Mapping(target = "status", constant = "true")
    @Mapping(target = "dateRegistration", ignore = true)
    @Mapping(target = "name", source = "requestStateSave.name")
    @Mapping(target = "tokenUser", source = "requestStateSave.user")
    OrderState stateToName(RequestStateSave requestStateSave);

    List<OrderState> listStateToListName(List<RequestStateSave> requestStateSaveList);

}
