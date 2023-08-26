package com.proyect.masterdata.mapper;


import com.proyect.masterdata.domain.PaymentMethod;
import com.proyect.masterdata.dto.PaymentMethodDTO;
import com.proyect.masterdata.dto.request.RequestPaymentMethodSave;
import com.proyect.masterdata.dto.request.RequestPaymentMethod;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface PaymentMethodMapper {
    PaymentMethodMapper INSTANCE = Mappers.getMapper(PaymentMethodMapper.class);
    @Mapping(target = "code", source = "id")
    PaymentMethodDTO paymentMethodToPaymentMethodDTO(PaymentMethod paymentMethod);
    List<PaymentMethodDTO> listPaymentMethodToListPaymentMethodDTO(List<PaymentMethod> paymentMethodList);
    @Mapping(target = "id", ignore = true)
    @Mapping(target = "status", constant = "true")
    @Mapping(target = "dateRegistration", ignore = true)
    @Mapping(target = "name", source = "requestPaymentMethodSave.name")
    @Mapping(target = "user", source = "requestPaymentMethodSave.user")
    PaymentMethod paymentMethodToName(RequestPaymentMethodSave requestPaymentMethodSave);
    List<PaymentMethod> listPaymentMethodToListName(List<RequestPaymentMethodSave> requestPaymentMethodSaveList);
}
