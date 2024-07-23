package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderStock;
import com.proyect.masterdata.domain.OrderStockItem;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.Warehouse;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface OrderStockRepositoryCustom {
    Page<OrderStock> searchForOrderStock(
            Long clientId,
            List<Long> orderIds,
            List<Long> warehouseIds,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status);
}
