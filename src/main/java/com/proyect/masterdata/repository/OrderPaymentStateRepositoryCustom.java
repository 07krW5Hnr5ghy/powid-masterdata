package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderPaymentState;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface OrderPaymentStateRepositoryCustom {
    Page<OrderPaymentState> searchForPaymentState(
            String name,
            String user,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
}
