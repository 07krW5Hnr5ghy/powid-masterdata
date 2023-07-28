package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.PaymentMethodDTO;
import com.proyect.masterdata.dto.response.ResponsePaymentMethod;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import java.util.List;

public interface IPaymentMethod {
    List<PaymentMethodDTO> listPaymentMethod() throws BadRequestExceptions;
    ResponsePaymentMethod addPaymentMethod(String paymentMethod) throws BadRequestExceptions;
    void deletePaymentMethod(Long id) throws BadRequestExceptions;
    void updatePaymentMethod(String name,Long id) throws BadRequestExceptions;
}
