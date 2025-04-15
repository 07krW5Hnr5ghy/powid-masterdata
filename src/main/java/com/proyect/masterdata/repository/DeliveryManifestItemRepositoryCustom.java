package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.DeliveryManifestItem;
import com.proyect.masterdata.dto.DeliveryManifestItemDTO;
import com.proyect.masterdata.dto.DeliveryManifestItemProjectionDTO;
import com.proyect.masterdata.dto.projections.DeliveryManifestItemProjection;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.time.OffsetDateTime;
import java.util.UUID;

@Repository
public interface DeliveryManifestItemRepositoryCustom {
    Page<DeliveryManifestItemProjectionDTO> searchForDeliveryManifestItem(
            UUID clientId,
            Integer quantity,
            Boolean collected,
            Long orderNumber,
            Long manifestNumber,
            String color,
            String size,
            String model,
            String brand,
            Boolean delivered,
            String courier,
            String courierDni,
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
