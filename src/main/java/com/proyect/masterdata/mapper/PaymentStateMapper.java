package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.OrderPaymentState;
import com.proyect.masterdata.dto.OrderPaymentStateDTO;
import com.proyect.masterdata.dto.request.RequestOrderPaymentStateSave;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface PaymentStateMapper {
    PaymentStateMapper INSTANCE = Mappers.getMapper(PaymentStateMapper.class);

    @Mapping(target = "code", source = "id")
    OrderPaymentStateDTO paymentStateToPaymentStateDTO(OrderPaymentState orderPaymentState);

    List<OrderPaymentStateDTO> listPaymentStateToListPaymentStateDTO(List<OrderPaymentState> orderPaymentStateList);

    OrderPaymentState paymentStateToName(RequestOrderPaymentStateSave requestOrderPaymentStateSave);

    List<OrderPaymentState> listPaymentStateToListName(List<RequestOrderPaymentStateSave> requestOrderPaymentStateSaveList);

}
