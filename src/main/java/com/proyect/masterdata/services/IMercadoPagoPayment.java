package com.proyect.masterdata.services;

import com.mercadopago.exceptions.MPApiException;
import com.mercadopago.exceptions.MPException;
import com.proyect.masterdata.domain.Subscription;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.List;

public interface IMercadoPagoPayment {
    public String sendPayment(Double netAmount, Subscription subscription,List<String> modules, User user) throws InternalErrorExceptions, BadRequestExceptions;
    public ResponseSuccess registerPayment(Long paymentId, String type) throws InternalErrorExceptions, BadRequestExceptions, MPException, MPApiException;
}
