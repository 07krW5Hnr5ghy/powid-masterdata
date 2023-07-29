package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.PaymentStateDTO;
import com.proyect.masterdata.dto.response.ResponsePaymentState;
import com.proyect.masterdata.exceptions.BadRequestExceptions;

import java.util.List;

public interface IPaymentState {
    List<PaymentStateDTO> listPaymentState() throws BadRequestExceptions;
    ResponsePaymentState addPaymentState(String paymentState) throws BadRequestExceptions;
    ResponsePaymentState deletePaymentState(Long id) throws BadRequestExceptions;
    PaymentStateDTO updatePaymentState(String name,Long id) throws BadRequestExceptions;
}
