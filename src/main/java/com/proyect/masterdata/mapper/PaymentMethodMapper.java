package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.OrderPaymentMethod;
import com.proyect.masterdata.dto.OrderPaymentMethodDTO;
import com.proyect.masterdata.dto.request.RequestOrderPaymentMethodSave;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface PaymentMethodMapper {
    PaymentMethodMapper INSTANCE = Mappers.getMapper(PaymentMethodMapper.class);

    @Mapping(target = "code", source = "id")
    OrderPaymentMethodDTO paymentMethodToPaymentMethodDTO(OrderPaymentMethod orderPaymentMethod);

    List<OrderPaymentMethodDTO> listPaymentMethodToListPaymentMethodDTO(List<OrderPaymentMethod> orderPaymentMethodList);

    OrderPaymentMethod paymentMethodToName(RequestOrderPaymentMethodSave requestOrderPaymentMethodSave);

    List<OrderPaymentMethod> listPaymentMethodToListName(List<RequestOrderPaymentMethodSave> requestOrderPaymentMethodSaveList);
}
