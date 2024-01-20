package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderState;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface OrderStateRepositoryCustom {
    Page<OrderState> searchForOrderState(
            String name,
            String user,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status);
}
