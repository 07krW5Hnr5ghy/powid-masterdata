package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderStock;
import com.proyect.masterdata.domain.OrderStockItem;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.Warehouse;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface OrderStockRepositoryCustom {
    Page<OrderStock> searchForOrderStock(Long warehouseId, Long orderId, Long clientId, String sort, String sortColumn, Integer pageNumber, Integer pageSize,Boolean status);
}
