package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderStock;
import com.proyect.masterdata.domain.OrderStockItem;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.Warehouse;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface OrderStockRepositoryCustom {
    Page<OrderStock> searchForOrderStock(
            UUID clientId,
            UUID orderId,
            List<UUID> warehouseIds,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status);
}
