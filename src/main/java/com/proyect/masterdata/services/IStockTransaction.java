package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.StockTransaction;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.Warehouse;
import com.proyect.masterdata.dto.StockTransactionDTO;
import com.proyect.masterdata.dto.request.RequestStockTransactionItem;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;

public interface IStockTransaction {
    public StockTransaction save(String serial, Warehouse warehouse, List<RequestStockTransactionItem> requestStockTransactionItemList, String stockTransactionType, User user) throws BadRequestExceptions, InternalErrorExceptions;
    Page<StockTransactionDTO> list(String user, String serial, String warehouse, String stockTransactionType, String sort, String sortColumn,
                                   Integer pageNumber, Integer pageSize) throws BadRequestExceptions;
    List<StockTransactionDTO> listStockTransaction(String user) throws InternalErrorExceptions,BadRequestExceptions;
}
