package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderReturnType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface OrderReturnTypeRepository extends JpaRepository<OrderReturnType, UUID> {
    OrderReturnType findByName(String name);
    List<OrderReturnType> findByNameIn(List<String> names);
    List<OrderReturnType> findAllByStatusTrue();
    List<OrderReturnType> findAllByStatusFalse();
    OrderReturnType findByNameAndStatusTrue(String name);
    OrderReturnType findByNameAndStatusFalse(String name);
}
