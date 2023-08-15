package com.proyect.masterdata.mapper;


import com.proyect.masterdata.domain.PaymentMethod;
import com.proyect.masterdata.dto.PaymentMethodDTO;
import com.proyect.masterdata.dto.request.RequestCreatePaymentMethod;
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
    @Mapping(target = "name", source = "name")
    PaymentMethod paymentMethodToName(String name,String user);

    @Mapping(target = "id", source = "code")
    @Mapping(target = "dateRegistration", ignore = true)
    PaymentMethod requestPaymentMethodToPaymentMethod(RequestPaymentMethod requestPaymentMethod);
    List<PaymentMethod> listRequestPaymentMethodToListPaymentMethod(List<RequestCreatePaymentMethod> requestCreatePaymentMethodList);

}
