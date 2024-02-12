package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderStockItem;
import com.proyect.masterdata.domain.User;
import com.proyect.masterdata.domain.Warehouse;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface OrderStockItemRepositoryCustom {
    Page<OrderStockItem> searchForOrderStock(Warehouse warehouse, Long orderId, User user, Boolean status, String sort, String sortColumn, Integer pageNumber, Integer pageSize);
}
