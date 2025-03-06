package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.SupplyOrder;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface SupplyOrderRepository extends JpaRepository<SupplyOrder, UUID> {
    SupplyOrder findByRef(String serial);
    List<SupplyOrder> findAllByClientId(UUID clientId);
    Long countByClientId(UUID clientId);
    SupplyOrder findByIdAndStatusTrue(UUID supplyOrderId);
}
