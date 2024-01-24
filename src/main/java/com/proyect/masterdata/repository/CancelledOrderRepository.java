package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.CancelledOrder;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface CancelledOrderRepository extends JpaRepository<CancelledOrder,Long> {
}
