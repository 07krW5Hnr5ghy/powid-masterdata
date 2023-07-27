package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.PaymentState;
import com.proyect.masterdata.dto.PaymentStateDTO;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface PaymentStateMapper {
    PaymentStateMapper INSTANCE = Mappers.getMapper(PaymentStateMapper.class);

    PaymentStateDTO paymentStateToPaymentStateDTO(PaymentState paymentState);

    List<PaymentStateDTO> paymentStateListToPaymentStateListDTO(List<PaymentState> paymentStateList);

}
