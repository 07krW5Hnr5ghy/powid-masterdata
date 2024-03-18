package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.MembershipPayment;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface MembershipPaymentRepositoryCustom {
    Page<MembershipPayment> searchForMembershipPayment(
            Long clientId,
            Double grossAmount,
            Double netAmount,
            Double paymentGatewayFee,
            Double taxAmount,
            Long paymentGatewayId,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize);
}
