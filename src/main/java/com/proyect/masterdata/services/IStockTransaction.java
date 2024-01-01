package com.proyect.masterdata.services;

import java.util.List;

import org.springframework.data.domain.Page;

import com.proyect.masterdata.dto.StockTransactionDTO;
import com.proyect.masterdata.dto.request.RequestStockTransaction;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IStockTransaction {
        ResponseSuccess save(List<RequestStockTransaction> stockTransactionDataList, String tokenUser)
                        throws InternalErrorExceptions, BadRequestExceptions;

        Page<StockTransactionDTO> list(String user, String warehouse, String sort, String sortColumn,
                        Integer pageNumber, Integer pageSize) throws BadRequestExceptions;

}
