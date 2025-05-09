package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Payment;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface PaymentRepository extends JpaRepository<Payment,Long> {
    Payment findByIdChannelAndMonthAndIdPaymentState(Long idChannel,String month,Long idPaymentState);
    List<Payment> findByIdChannelAndIdPaymentState(Long idChannel, Long idPaymentState);
}
