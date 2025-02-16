package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.MembershipPayment;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.time.OffsetDateTime;
import java.util.Date;
import java.util.UUID;

@Repository
public interface MembershipPaymentRepositoryCustom {
    Page<MembershipPayment> searchForMembershipPayment(
            UUID clientId,
            Double grossAmount,
            Double netAmount,
            Double paymentGatewayFee,
            Double taxAmount,
            UUID paymentGatewayId,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize);
}
