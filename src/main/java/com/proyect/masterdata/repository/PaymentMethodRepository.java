package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.PaymentMethod;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;


public interface PaymentMethodRepository extends JpaRepository<PaymentMethod,Long> {
    List<PaymentMethod> findAllByStatusTrue();
    List<PaymentMethod> findAllByStatusFalse();
    PaymentMethod findByIdAndStatusTrue(Long id);
    PaymentMethod findByNameAndStatusTrue(String name);
    List<PaymentMethod> findByNameIn(List<String> names);
}
