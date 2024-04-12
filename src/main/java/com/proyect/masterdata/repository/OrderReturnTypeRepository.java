package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderReturnType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface OrderReturnTypeRepository extends JpaRepository<OrderReturnType,Long> {
    OrderReturnType findByName(String name);
}
