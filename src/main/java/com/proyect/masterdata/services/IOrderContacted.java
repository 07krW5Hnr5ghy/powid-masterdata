package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.OrderContacted;
import com.proyect.masterdata.dto.OrderContactedDTO;
import com.proyect.masterdata.dto.response.ResponseSuccess;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.time.OffsetDateTime;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

public interface IOrderContacted {
    CompletableFuture<OrderContacted> save(UUID orderId, String username) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<ResponseSuccess> markContacted(UUID orderId, String username) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<Page<OrderContactedDTO>> list(
            String username,
            Long orderNumber,
            Boolean contacted,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize
    );
}
