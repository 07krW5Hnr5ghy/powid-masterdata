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
}
