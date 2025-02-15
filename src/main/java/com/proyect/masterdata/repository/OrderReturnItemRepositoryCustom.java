package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderReturnItem;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.time.OffsetDateTime;
import java.util.Date;
import java.util.List;
import java.util.UUID;

@Repository
public interface OrderReturnItemRepositoryCustom {
    Page<OrderReturnItem> searchForOrderReturnItem(
            UUID clientId,
            List<UUID> orderIds,
            List<UUID> productIds,
            List<UUID> supplierProductIds,
            List<UUID> warehouseIds,
            List<UUID> orderReturnTypeIds,
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
