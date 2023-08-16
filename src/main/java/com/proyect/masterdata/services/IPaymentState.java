package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.PaymentStateDTO;
import com.proyect.masterdata.dto.request.RequestPaymentState;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;

import java.util.List;

public interface IPaymentState {
    ResponseSuccess save(String name,String user) throws BadRequestExceptions;
    ResponseSuccess saveAll(List<String> names,String user) throws BadRequestExceptions;
    PaymentStateDTO update(RequestPaymentState requestPaymentState) throws BadRequestExceptions;
    ResponseDelete delete(Long code) throws BadRequestExceptions;
    ResponseDelete deleteAll(List<Long> codes) throws BadRequestExceptions;
    List<PaymentStateDTO> list() throws BadRequestExceptions;
    PaymentStateDTO findByCode(Long code) throws BadRequestExceptions;
    PaymentStateDTO findByName(String name) throws BadRequestExceptions;
}
