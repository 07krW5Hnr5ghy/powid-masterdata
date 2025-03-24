package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.DeliveryZoneDistrict;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface DeliveryZoneDistrictRepository extends JpaRepository<DeliveryZoneDistrict, UUID> {
    List<DeliveryZoneDistrict> findAllByDeliveryZoneIdAndDistrictIdAndStatusTrue(UUID deliveryZoneId,UUID districtId);
    DeliveryZoneDistrict findByDeliveryZoneIdAndDistrictId(UUID deliveryZoneId,UUID districtId);
    DeliveryZoneDistrict findByDeliveryZoneIdAndDistrictIdAndStatusTrue(UUID deliveryZoneId,UUID districtId);
    DeliveryZoneDistrict findByDeliveryZoneIdAndDistrictIdAndStatusFalse(UUID deliveryZoneId,UUID districtId);
}
