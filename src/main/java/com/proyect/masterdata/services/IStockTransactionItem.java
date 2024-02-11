package com.proyect.masterdata.services;

import java.util.List;

import org.springframework.data.domain.Page;

import com.proyect.masterdata.dto.StockTransactionItemDTO;
import com.proyect.masterdata.dto.request.RequestStockTransactionItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IStockTransactionItem {
        ResponseSuccess save(List<RequestStockTransactionItem> stockTransactionDataList, String tokenUser)
                        throws InternalErrorExceptions, BadRequestExceptions;

        Page<StockTransactionItemDTO> list(String user, String warehouse, String sort, String sortColumn,
                                           Integer pageNumber, Integer pageSize) throws BadRequestExceptions;

}
