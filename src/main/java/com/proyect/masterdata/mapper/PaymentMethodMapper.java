package com.proyect.masterdata.mapper;

import com.proyect.masterdata.domain.PaymentMethod;
import com.proyect.masterdata.dto.MasterListDTO;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface PaymentMethodMapper {
    PaymentMethodMapper INSTANCE = Mappers.getMapper(PaymentMethodMapper.class);
    @Mapping(source="id",target = "id")
    MasterListDTO paymentMethodToPaymentMethodDTO(PaymentMethod paymentMethod);

    List<MasterListDTO> paymentMethodListToPaymentMethodListDTO(List<PaymentMethod> paymentMethodList);
}
