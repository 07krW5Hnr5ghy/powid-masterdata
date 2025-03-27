package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.PurchaseOrder;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface PurchaseOrderRepository extends JpaRepository<PurchaseOrder, UUID> {
    PurchaseOrder findByRef(String serial);
    List<PurchaseOrder> findAllByClientId(UUID clientId);
    Long countByClientId(UUID clientId);
    PurchaseOrder findByIdAndStatusTrue(UUID supplyOrderId);
}
