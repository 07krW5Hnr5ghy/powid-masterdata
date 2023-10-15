package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.Payment;
import com.proyect.masterdata.dto.PaymentDTO;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface PaymentMapper {
    PaymentMapper INSTANCE = Mappers.getMapper(PaymentMapper.class);
}
