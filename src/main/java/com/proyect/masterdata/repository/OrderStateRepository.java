package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderState;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface OrderStateRepository extends JpaRepository<OrderState, Long> {
    List<OrderState> findAllByStatusTrue();

    List<OrderState> findAllByStatusFalse();

    OrderState findByIdAndStatusTrue(Long id);

    OrderState findByNameAndStatusTrue(String name);

    List<OrderState> findByNameIn(List<String> names);
}
