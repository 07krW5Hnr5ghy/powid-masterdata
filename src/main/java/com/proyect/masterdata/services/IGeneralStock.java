package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.Product;
import org.springframework.data.domain.Page;

import com.proyect.masterdata.dto.GeneralStockDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.concurrent.CompletableFuture;

public interface IGeneralStock {
        CompletableFuture<ResponseSuccess> in(Product product, Integer quantity, String tokenUser)
                        throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<ResponseSuccess> out(Product product, Integer quantity, String tokenUser)
                        throws InternalErrorExceptions, BadRequestExceptions;
        CompletableFuture<Page<GeneralStockDTO>> list(
                String user,
                String model,
                OffsetDateTime registrationStartDate,
                OffsetDateTime registrationEndDate,
                OffsetDateTime updateStartDate,
                OffsetDateTime updateEndDate,
                String sort,
                String sortColumn,
                Integer pageNumber,
                Integer pageSize) throws InternalErrorExceptions;
        CompletableFuture<List<GeneralStockDTO>> listGeneralStock(String user) throws BadRequestExceptions,InternalErrorExceptions;
}
