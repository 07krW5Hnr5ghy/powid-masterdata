package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.SupplyOrder;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.time.OffsetDateTime;
import java.util.UUID;

@Repository
public interface SupplyOrderRepositoryCustom {
    Page<SupplyOrder> searchForSupplyOrder(
            UUID clientId,
            Long orderNumber,
            String ref,
            String warehouse,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status);
}
