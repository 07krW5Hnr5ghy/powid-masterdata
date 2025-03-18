package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.SupplyOrderItem;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.time.OffsetDateTime;
import java.util.UUID;

@Repository
public interface SupplyOrderItemRepositoryCustom {
    Page<SupplyOrderItem> searchForSupplyOrderItem(
            UUID clientId,
            Long orderNumber,
            String ref,
            String warehouse,
            String supplier,
            Integer quantity,
            String model,
            String product,
            String color,
            String size,
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
