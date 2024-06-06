package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderReturnType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface OrderReturnTypeRepository extends JpaRepository<OrderReturnType,Long> {
    OrderReturnType findByName(String name);
    List<OrderReturnType> findAllByStatusTrue();
    List<OrderReturnType> findAllByStatusFalse();
    OrderReturnType findByNameAndStatusTrue(String name);
    OrderReturnType findByNameAndStatusFalse(String name);
}
