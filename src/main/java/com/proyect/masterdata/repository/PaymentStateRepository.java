package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Department;
import com.proyect.masterdata.domain.PaymentState;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface PaymentStateRepository extends JpaRepository<PaymentState,Long> {
    List<PaymentState> findAllByStatusTrue();
    List<PaymentState> findAllByStatusFalse();
    PaymentState findByIdAndStatusTrue(Long id);
    PaymentState findByNameAndStatusTrue(String name);
    List<PaymentState> findByNameIn(List<String> names);
}
