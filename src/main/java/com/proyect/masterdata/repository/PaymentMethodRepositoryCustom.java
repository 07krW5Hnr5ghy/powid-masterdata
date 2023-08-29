package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.PaymentMethod;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface PaymentMethodRepositoryCustom {
    Page<PaymentMethod> searchForPaymentMethod(
            String name,
            String user,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
}
