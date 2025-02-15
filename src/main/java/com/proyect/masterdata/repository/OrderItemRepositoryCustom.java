package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderItem;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.UUID;

@Repository
public interface OrderItemRepositoryCustom {
    Page<OrderItem> searchForOrderItem(
            UUID clientId,
            UUID orderId,
            String productSku,
            List<UUID> colorIds,
            List<UUID> sizeIds,
            List<UUID> categoryIds,
            Integer quantity,
            Double discount,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status);
}
