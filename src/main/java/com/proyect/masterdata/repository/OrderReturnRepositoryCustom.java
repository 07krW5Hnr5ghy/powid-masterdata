package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderReturn;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.util.Date;
import java.util.List;

@Repository
public interface OrderReturnRepositoryCustom {
    Page<OrderReturn> searchForOrderReturn(
            Long clientId,
            List<Long> orderIds,
            List<Long> warehouseIds,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
}
