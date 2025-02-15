package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderPaymentState;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface OrderPaymentStateRepository extends JpaRepository<OrderPaymentState, UUID> {
    List<OrderPaymentState> findAllByStatusTrue();
    List<OrderPaymentState> findAllByStatusFalse();
    OrderPaymentState findByIdAndStatusTrue(UUID id);
    OrderPaymentState findByNameAndStatusTrue(String name);
    OrderPaymentState findByNameAndStatusFalse(String name);
    List<OrderPaymentState> findByNameIn(List<String> names);
    OrderPaymentState findByName(String name);
}
