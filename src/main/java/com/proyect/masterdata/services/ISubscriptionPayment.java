package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.request.RequestSubscriptionPayment;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface ISubscriptionPayment {
    public String send(RequestSubscriptionPayment requestSubscriptionPayment,String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    public ResponseSuccess activateDemo(String tokenUser) throws InternalErrorExceptions,BadRequestExceptions;
}
