package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.PaymentMethod;
import com.proyect.masterdata.dto.PaymentMethodDTO;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface PaymentMethodMapper {
    PaymentMethodMapper INSTANCE = Mappers.getMapper(PaymentMethodMapper.class);

    PaymentMethodDTO paymentMethodToPaymentMethodDTO(PaymentMethod paymentMethod);

    List<PaymentMethodDTO> paymentMethodListToPaymentMethodListDTO(List<PaymentMethod> paymentMethodList);
}
