package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderPaymentMethod;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface OrderPaymentMethodRepository extends JpaRepository<OrderPaymentMethod, UUID> {
    List<OrderPaymentMethod> findAllByStatusTrue();
    List<OrderPaymentMethod> findAllByStatusFalse();
    OrderPaymentMethod findByIdAndStatusTrue(UUID id);
    OrderPaymentMethod findByNameAndStatusTrue(String name);
    List<OrderPaymentMethod> findByNameIn(List<String> names);
    OrderPaymentMethod findByName(String name);
}
