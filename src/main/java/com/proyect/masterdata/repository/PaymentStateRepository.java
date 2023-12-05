package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.PaymentState;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface PaymentStateRepository extends JpaRepository<PaymentState, Long> {
    List<PaymentState> findAllByStatusTrue();

    List<PaymentState> findAllByStatusFalse();

    PaymentState findByIdAndStatusTrue(Long id);

    PaymentState findByNameAndStatusTrue(String name);

    List<PaymentState> findByNameIn(List<String> names);
}
