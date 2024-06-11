package com.proyect.masterdata.services;

import org.springframework.data.domain.Page;

import com.proyect.masterdata.dto.GeneralStockDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.util.Date;
import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface IGeneralStock {
        CompletableFuture<ResponseSuccess> in(String supplierProductSerial, Integer quantity, String tokenUser)
                        throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<ResponseSuccess> out(String supplierProductSerial, Integer quantity, String tokenUser)
                        throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<Page<GeneralStockDTO>> list(
                String user,
                String supplierProductSerial,
                Date registrationStartDate,
                Date registrationEndDate,
                Date updateStartDate,
                Date updateEndDate,
                String sort,
                String sortColumn,
                Integer pageNumber,
                Integer pageSize) throws InternalErrorExceptions;
        CompletableFuture<List<GeneralStockDTO>> listGeneralStock(String user) throws BadRequestExceptions,InternalErrorExceptions;
}
