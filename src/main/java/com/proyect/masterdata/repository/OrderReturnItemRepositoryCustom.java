package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderReturnItem;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.util.Date;
import java.util.List;

@Repository
public interface OrderReturnItemRepositoryCustom {
    Page<OrderReturnItem> searchForOrderReturnItem(
            Long clientId,
            List<Long> orderIds,
            List<Long> productIds,
            List<Long> supplierProductIds,
            List<Long> warehouseIds,
            List<Long> orderReturnTypeIds,
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
