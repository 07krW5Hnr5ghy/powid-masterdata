package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderDeliveryStatus;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;
@Repository
public interface OrderDeliveryStatusRepository extends JpaRepository<OrderDeliveryStatus, UUID> {
    List<OrderDeliveryStatus> findAllByStatusTrueAndClientId(UUID clientId);
    List<OrderDeliveryStatus> findAllByStatusFalseAndClientId(UUID clientId);
    OrderDeliveryStatus findByIdAndClientIdAndStatusTrue(UUID id,UUID clientId);
    OrderDeliveryStatus findByNameAndClientIdAndStatusTrue(String name,UUID clientId);
    OrderDeliveryStatus findByNameAndClientIdAndStatusFalse(String name,UUID clientId);
    OrderDeliveryStatus findByNameAndClientId(String name,UUID clientId);
    List<OrderDeliveryStatus> findByClientIdAndNameIn(UUID clientId,List<String> names);
    List<OrderDeliveryStatus> findAllByClientId(UUID clientId);
}
