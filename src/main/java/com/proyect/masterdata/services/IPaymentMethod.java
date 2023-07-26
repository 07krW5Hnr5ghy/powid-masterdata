package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.PaymentMethodDTO;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import java.util.List;

public interface IPaymentMethod {
    List<PaymentMethodDTO> listPaymentMethod() throws BadRequestExceptions;
}
