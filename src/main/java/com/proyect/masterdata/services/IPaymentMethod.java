package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.PaymentMethodDTO;
import com.proyect.masterdata.dto.request.RequestCreatePaymentMethod;
import com.proyect.masterdata.dto.request.RequestPaymentMethod;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;

import java.util.List;

public interface IPaymentMethod {
    ResponseSuccess save(String name, String user) throws BadRequestExceptions;
    ResponseSuccess saveAll(List<RequestCreatePaymentMethod> requestCreatePaymentMethodList) throws BadRequestExceptions;
    PaymentMethodDTO update(RequestPaymentMethod requestPaymentMethod) throws BadRequestExceptions;
    ResponseDelete delete(Long code) throws BadRequestExceptions;
    ResponseDelete deleteAll(List<Long> codes) throws BadRequestExceptions;
    List<PaymentMethodDTO> list() throws BadRequestExceptions;
    PaymentMethodDTO findByCode(Long code) throws BadRequestExceptions;
    PaymentMethodDTO findByName(String name) throws BadRequestExceptions;
}
