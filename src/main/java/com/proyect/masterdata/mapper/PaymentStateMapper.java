package com.proyect.masterdata.mapper;


import com.proyect.masterdata.domain.PaymentState;
import com.proyect.masterdata.dto.PaymentStateDTO;
import com.proyect.masterdata.dto.request.RequestPaymentStateSave;
import com.proyect.masterdata.dto.request.RequestPaymentState;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface PaymentStateMapper {
    PaymentStateMapper INSTANCE = Mappers.getMapper(PaymentStateMapper.class);
    @Mapping(target = "code", source = "id")
    PaymentStateDTO paymentStateToPaymentStateDTO(PaymentState paymentState);
    List<PaymentStateDTO> listPaymentStateToListPaymentStateDTO(List<PaymentState> paymentStateList);
    @Mapping(target = "id", ignore = true)
    @Mapping(target = "status", constant = "true")
    @Mapping(target = "dateRegistration", ignore = true)
    @Mapping(target = "name", source = "requestPaymentStateSave.name")
    @Mapping(target = "user", source = "requestPaymentStateSave.user")
    PaymentState paymentStateToName(RequestPaymentStateSave requestPaymentStateSave);

    List<PaymentState> listPaymentStateToListName(List<RequestPaymentStateSave> requestPaymentStateSaveList);

}
