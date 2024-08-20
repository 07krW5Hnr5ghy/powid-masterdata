package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderPaymentMethod;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface OrderPaymentMethodRepository extends JpaRepository<OrderPaymentMethod, Long> {
    List<OrderPaymentMethod> findAllByStatusTrue();
    List<OrderPaymentMethod> findAllByStatusFalse();
    OrderPaymentMethod findByIdAndStatusTrue(Long id);
    OrderPaymentMethod findByNameAndStatusTrue(String name);
    List<OrderPaymentMethod> findByNameIn(List<String> names);
    OrderPaymentMethod findByName(String name);
}
