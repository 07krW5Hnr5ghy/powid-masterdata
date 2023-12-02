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

    PaymentMethod paymentMethodToName(RequestPaymentMethodSave requestPaymentMethodSave);

    List<PaymentMethod> listPaymentMethodToListName(List<RequestPaymentMethodSave> requestPaymentMethodSaveList);
}
