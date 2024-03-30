package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.StockReturnDTO;
import com.proyect.masterdata.dto.request.RequestStockReturn;
import com.proyect.masterdata.dto.request.RequestStockReturnItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;

public interface IStockReturn {
    ResponseSuccess save(RequestStockReturn requestStockReturn) throws InternalErrorExceptions, BadRequestExceptions;
    Page<StockReturnDTO> list(String purchaseSerial, String user, String sort, String sortColumn,
                              Integer pageNumber, Integer pageSize) throws BadRequestExceptions;
    List<StockReturnDTO> listStockReturn(String user) throws InternalErrorExceptions,BadRequestExceptions;
    List<StockReturnDTO> listStockReturnFalse(String user) throws InternalErrorExceptions,BadRequestExceptions;
}
