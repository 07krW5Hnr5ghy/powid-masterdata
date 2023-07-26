package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.PaymentMethod;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface PaymentMethodRepository extends JpaRepository<PaymentMethod,Long> {
    @Modifying
    @Query(value = "UPDATE master.medios_pago SET nombre = :name WHERE id = :id",nativeQuery = true)
    void updatePaymentMethod(@Param("name") String name,@Param("id") Long id);
}
