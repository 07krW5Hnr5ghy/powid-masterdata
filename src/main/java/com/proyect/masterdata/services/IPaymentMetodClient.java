package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.List;
import java.util.UUID;

public interface IPaymentMetodClient {
    ResponseSuccess save (String namePaymentMethod, String nameCountPayment, String userToken, String observations) throws InternalErrorExceptions, BadRequestExceptions;
}
