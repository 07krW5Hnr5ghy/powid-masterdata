package com.proyect.masterdata.services;

import com.mercadopago.exceptions.MPApiException;
import com.mercadopago.exceptions.MPException;
import com.proyect.masterdata.domain.Subscription;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.List;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

public interface IMercadoPagoPayment {
    CompletableFuture<String> sendPayment(Double netAmount, Subscription subscription, List<String> modules, User user) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<ResponseSuccess> registerPayment(
            UUID paymentId,
            String type,
            String requestIdHeader,
            String signatureHeader) throws InternalErrorExceptions, BadRequestExceptions, MPException, MPApiException;
}
