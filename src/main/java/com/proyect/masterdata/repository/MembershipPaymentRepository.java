package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.MembershipPayment;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface MembershipPaymentRepository extends JpaRepository<MembershipPayment, Long> {
    MembershipPayment findByPaymentReference(Long paymentReference);
}
