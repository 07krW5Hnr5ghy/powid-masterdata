package com.proyect.masterdata.services;

import com.proyect.masterdata.domain.KardexInput;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.dto.KardexInputDTO;
import com.proyect.masterdata.dto.request.RequestKardexInput;
import com.proyect.masterdata.exceptions.BadRequestExceptions;
import com.proyect.masterdata.exceptions.InternalErrorExceptions;
import org.springframework.data.domain.Page;

import java.time.OffsetDateTime;
import java.util.UUID;
import java.util.concurrent.CompletableFuture;

public interface IKardexInput {
    KardexInput save(RequestKardexInput requestKardexInput) throws BadRequestExceptions, InternalErrorExceptions;
    void returnFromDeliveryManifestItem(UUID deliveryManifestItemId,Integer units, User user) throws BadRequestExceptions,InternalErrorExceptions;
    CompletableFuture<Page<KardexInputDTO>> list(
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
