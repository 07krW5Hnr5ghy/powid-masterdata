package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.CancelledOrder;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.time.OffsetDateTime;
import java.util.Date;
import java.util.UUID;

@Repository
public interface CancelledOrderRepositoryCustom {
    Page<CancelledOrder> searchForCancelledOrder(
            UUID orderId,
            UUID clientId,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize);
}
