package com.proyect.masterdata.services;

import java.util.List;

import com.proyect.masterdata.dto.request.RequestStockTransaction;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IStockTransaction {
    ResponseSuccess save(RequestStockTransaction stockTransactionData, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;

    ResponseSuccess saveAll(List<RequestStockTransaction> stockTransactionDataList, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;

}
