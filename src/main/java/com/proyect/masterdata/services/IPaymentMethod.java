package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.PaymentMethodDTO;
import com.proyect.masterdata.dto.request.RequestPaymentMethod;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.List;

public interface IPaymentMethod {
    ResponseSuccess save(String name, String user) throws BadRequestExceptions,InternalErrorExceptions;
    ResponseSuccess saveAll(List<String> names,String user) throws BadRequestExceptions,InternalErrorExceptions;
    PaymentMethodDTO update(RequestPaymentMethod requestPaymentMethod) throws BadRequestExceptions,InternalErrorExceptions;
    ResponseDelete delete(Long code,String user) throws BadRequestExceptions, InternalErrorExceptions;
    List<PaymentMethodDTO> listPaymentMethod() throws BadRequestExceptions;
    List<PaymentMethodDTO> listStatusFalse() throws BadRequestExceptions;
    PaymentMethodDTO findByCode(Long code) throws BadRequestExceptions;
}
