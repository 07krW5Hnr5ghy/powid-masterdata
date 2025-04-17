package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.KardexOutput;
import com.proyect.masterdata.dto.KardexInputDTO;
import com.proyect.masterdata.dto.KardexOutputDTO;
import com.proyect.masterdata.dto.request.RequestKardexOutput;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.time.OffsetDateTime;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

public interface IkardexOutput {
    KardexOutput save(RequestKardexOutput requestKardexOutput) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<Page<KardexOutputDTO>> list(
            String user,
            Integer quantity,
            String product,
            UUID productId,
            String username,
            String warehouse,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws InternalErrorExceptions;
}
