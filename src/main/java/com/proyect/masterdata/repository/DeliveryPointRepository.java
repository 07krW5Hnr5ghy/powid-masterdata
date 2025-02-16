package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.DeliveryPoint;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface DeliveryPointRepository extends JpaRepository<DeliveryPoint, UUID> {
    DeliveryPoint findByName(String name);
    DeliveryPoint findByNameAndStatusTrue(String name);
    List<DeliveryPoint> findAllByStatusTrue();
    List<DeliveryPoint> findByNameIn(List<String> names);
}
