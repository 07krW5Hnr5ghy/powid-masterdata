package com.proyect.masterdata.services;

import java.util.List;

import com.proyect.masterdata.dto.StockTransactionTypeDTO;
import com.proyect.masterdata.dto.response.ResponseDelete;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

public interface IStockTransactionType {

    public ResponseSuccess save(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;

    public ResponseSuccess saveAll(List<String> names, String tokenUser)
            throws InternalErrorExceptions, BadRequestExceptions;

    public ResponseDelete delete(String name, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;

    public List<StockTransactionTypeDTO> list() throws BadRequestExceptions;
}