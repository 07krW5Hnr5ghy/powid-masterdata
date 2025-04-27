package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.KardexBalance;
import com.proyect.masterdata.dto.KardexBalanceDTO;
import com.proyect.masterdata.dto.KardexInputDTO;
import com.proyect.masterdata.dto.request.RequestKardexBalance;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.time.OffsetDateTime;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

public interface IKardexBalance {
    KardexBalance save(RequestKardexBalance requestKardexBalance) throws BadRequestExceptions, InternalErrorExceptions;
    CompletableFuture<Page<KardexBalanceDTO>> list(
            String user,
            Integer quantity,
            Long lotNumber,
            String product,
            UUID productId,
            String username,
            String warehouse,
            Double unitPrice,
            String model,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize) throws InternalErrorExceptions;
}
