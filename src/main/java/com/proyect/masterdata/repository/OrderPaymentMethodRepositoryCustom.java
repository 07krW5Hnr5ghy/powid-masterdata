package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderPaymentMethod;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface OrderPaymentMethodRepositoryCustom {
    Page<OrderPaymentMethod> searchForPaymentMethod(
            String name,
            String user,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
}
