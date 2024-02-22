package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderPaymentState;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface OrderPaymentStateRepository extends JpaRepository<OrderPaymentState, Long> {
    List<OrderPaymentState> findAllByStatusTrue();
    List<OrderPaymentState> findAllByStatusFalse();
    OrderPaymentState findByIdAndStatusTrue(Long id);
    OrderPaymentState findByNameAndStatusTrue(String name);
    List<OrderPaymentState> findByNameIn(List<String> names);
    OrderPaymentState findByName(String name);
}
