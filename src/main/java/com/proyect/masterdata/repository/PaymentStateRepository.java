package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.PaymentState;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface PaymentStateRepository extends JpaRepository<PaymentState,Long> {
    @Modifying
    @Query(value = "UPDATE master.estados_pago SET nombre = :name WHERE id = :id",nativeQuery = true)
    void updatePaymentState(@Param("name") String name, @Param("id") Long id);
}
