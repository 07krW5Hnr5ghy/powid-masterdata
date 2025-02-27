package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.DeliveryStatus;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;
@Repository
public interface DeliveryStatusRepository extends JpaRepository<DeliveryStatus, UUID> {
    DeliveryStatus findByName(String name);
    List<DeliveryStatus> findAllByStatusTrue();
    DeliveryStatus findByNameAndStatusTrue(String name);
    DeliveryStatus findByNameAndStatusFalse(String name);
}
