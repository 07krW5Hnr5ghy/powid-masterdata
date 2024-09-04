package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderStockItem;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.Warehouse;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface OrderStockItemRepositoryCustom {
    Page<OrderStockItem> searchForOrderStockItem(
            Long clientId,
            Long orderId,
            List<Long> warehouseIds,
            String productSku,
            String serial,
            String model,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status);
}
