package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.StockTransferDTO;
import com.proyect.masterdata.dto.request.RequestStockTransfer;
import com.proyect.masterdata.dto.request.RequestStockTransferItem;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface IStockTransfer {
    ResponseSuccess save(RequestStockTransfer requestStockTransfer, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<ResponseSuccess> saveAsync(RequestStockTransfer requestStockTransfer, String tokenUser) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<Page<StockTransferDTO>> list(
            String user,
            List<String> serials,
            List<String> originWarehouses,
            List<String> destinationWarehouses,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws BadRequestExceptions;
    CompletableFuture<List<StockTransferDTO>> listStockTransfer(String user) throws InternalErrorExceptions,BadRequestExceptions;

}
