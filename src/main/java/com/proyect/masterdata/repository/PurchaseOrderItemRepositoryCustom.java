package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.PurchaseOrderItem;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.time.OffsetDateTime;
import java.util.UUID;

@Repository
public interface PurchaseOrderItemRepositoryCustom {
    Page<PurchaseOrderItem> searchForPurchaseOrderItem(
            UUID clientId,
            Long orderNumber,
            String ref,
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
