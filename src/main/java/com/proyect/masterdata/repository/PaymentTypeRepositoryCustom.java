package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.PaymentType;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface PaymentTypeRepositoryCustom {
    Page<PaymentType> searchForPaymentType(
            String type,
            String user,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
}
