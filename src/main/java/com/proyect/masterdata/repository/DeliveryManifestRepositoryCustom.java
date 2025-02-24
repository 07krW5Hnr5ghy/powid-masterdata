package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.DeliveryManifest;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.time.OffsetDateTime;
import java.util.UUID;

@Repository
public interface DeliveryManifestRepositoryCustom {
    Page<DeliveryManifest> searchForDeliveryManifest(
            UUID clientId,
            Long manifestNumber,
            String warehouse,
            String courier,
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
