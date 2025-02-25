package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.DeliveryManifestItem;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.time.OffsetDateTime;
import java.util.UUID;

@Repository
public interface DeliveryManifestItemRepositoryCustom {
    Page<DeliveryManifestItem> searchForDeliveryManifestItem(
            UUID clientId,
            Integer quantity,
            Boolean collected,
            Long orderNumber,
            Long manifestNumber,
            String color,
            String size,
            String model,
            String supplier,
            String brand,
            String deliveryStatus,
            String courier,
            String warehouse,
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
