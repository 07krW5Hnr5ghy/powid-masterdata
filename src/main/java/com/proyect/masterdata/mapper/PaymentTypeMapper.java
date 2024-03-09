package com.proyect.masterdata.mapper;

import com.proyect.masterdata.dto.PaymentTypeDTO;
import com.proyect.masterdata.dto.request.RequestPaymentTypeSave;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

import java.util.List;

@Mapper(componentModel = "spring")
public interface PaymentTypeMapper {
    PaymentTypeMapper INSTANCE = Mappers.getMapper(PaymentTypeMapper.class);
    List<PaymentTypeDTO> listPaymentTypeToListPaymentTypeDTO(List<PaymentType> paymentTypeList);
    List<PaymentType> listPaymentTypeToName(List<RequestPaymentTypeSave> requestPaymentTypeSaveList);
}
