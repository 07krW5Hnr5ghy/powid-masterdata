package com.proyect.masterdata.services;

import com.proyect.masterdata.dto.CancelledOrderDTO;
import com.proyect.masterdata.dto.request.RequestCancelledOrder;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.util.Date;
import java.util.concurrent.CompletableFuture;

public interface ICancelledOrder {
    CompletableFuture<ResponseSuccess> save(RequestCancelledOrder requestCancelledOrder, String tokenUser) throws InternalErrorExceptions, BadRequestExceptions;
    CompletableFuture<Page<CancelledOrderDTO>> list(Long orderId, String user, Date registrationStartDate, Date registrationEndDate, Date updateStartDate, Date updateEndDate, String sort, String sortColumn, Integer pageNumber, Integer pageSize) throws BadRequestExceptions;
}
