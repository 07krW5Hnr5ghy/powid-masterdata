package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.WarehouseOutputItem;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.time.OffsetDateTime;
import java.util.UUID;

@Repository
public interface WarehouseOutputItemRepositoryCustom {
    Page<WarehouseOutputItem> searchForWarehouseOutputItem(
            UUID clientId,
            Long orderNumber,
            String ref,
            String courier,
            String warehouse,
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
            Boolean status
    );
}
