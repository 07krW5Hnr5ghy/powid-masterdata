package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.MembershipPayment;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface MembershipPaymentRepositoryCustom {
    Page<MembershipPayment> searchForPayment(
            Double totalPayment,
            String month,
            Long idChannel,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize);
}
