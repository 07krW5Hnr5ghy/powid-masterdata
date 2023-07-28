package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.PaymentStateDTO;
import com.proyect.masterdata.exceptions.BadRequestExceptions;

import java.util.List;

public interface IPaymentState {
    List<PaymentStateDTO> listPaymentState() throws BadRequestExceptions;
    void addPaymentState(String paymentState) throws BadRequestExceptions;
    void deletePaymentState(Long id) throws BadRequestExceptions;
    void updatePaymentState(String name,Long id) throws BadRequestExceptions;
}
