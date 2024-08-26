package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderItem;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface OrderItemRepositoryCustom {
    Page<OrderItem> searchForOrderItem(
            Long clientId,
            Long orderId,
            String productSku,
            Integer quantity,
            Double discount,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status);
}
