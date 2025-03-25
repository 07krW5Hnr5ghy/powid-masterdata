package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.DeliveryZoneDistrict;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.time.OffsetDateTime;

@Repository
public interface DeliveryZoneDistrictRepositoryCustom {
    Page<DeliveryZoneDistrict> searchForDeliveryZoneDistrict(
            String deliveryZone,
            String district,
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
