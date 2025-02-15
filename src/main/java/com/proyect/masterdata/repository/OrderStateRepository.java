package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderState;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface OrderStateRepository extends JpaRepository<OrderState, UUID> {
    List<OrderState> findAllByStatusTrue();
    List<OrderState> findAllByStatusFalse();
    OrderState findByIdAndStatusTrue(UUID id);
    OrderState findByNameAndStatusTrue(String name);
    OrderState findByNameAndStatusFalse(String name);
    OrderState findByName(String name);
    List<OrderState> findByNameIn(List<String> names);
}
