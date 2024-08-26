package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderItem;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface OrderItemRepositoryCustom {
    Page<OrderItem> searchForOrderItem(
            Long clientId,
            Long orderId,
            String productSku,
            List<Long> colorIds,
            List<Long> sizeIds,
            List<Long> categoryIds,
            Integer quantity,
            Double discount,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status);
}
