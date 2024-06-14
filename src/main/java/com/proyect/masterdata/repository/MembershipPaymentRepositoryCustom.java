package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.MembershipPayment;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.util.Date;

@Repository
public interface MembershipPaymentRepositoryCustom {
    Page<MembershipPayment> searchForMembershipPayment(
            Long clientId,
            Double grossAmount,
            Double netAmount,
            Double paymentGatewayFee,
            Double taxAmount,
            Long paymentGatewayId,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize);
}
