package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.PaymentState;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface PaymentStateRepositoryCustom {
    Page<PaymentState> searchForPaymentState(
            String name,
            String user,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
}
