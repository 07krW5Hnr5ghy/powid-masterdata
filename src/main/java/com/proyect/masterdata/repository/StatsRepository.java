package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.CategoryProduct;
import com.proyect.masterdata.domain.OrderItem;
import com.proyect.masterdata.domain.Ordering;
import com.proyect.masterdata.domain.SaleChannel;
import org.springframework.stereotype.Repository;

import java.time.OffsetDateTime;
import java.util.List;
import java.util.UUID;

@Repository
public interface StatsRepository {
    List<CategoryProduct> findAllCategoryProducts();
    List<SaleChannel> findAllSaleChannels();
    List<Ordering> findOrdersByClientAndRegistrationDate(UUID clientId, OffsetDateTime startDate, OffsetDateTime endDate);
    List<OrderItem> findOrderItemsByClientAndOrder(UUID clientId, UUID orderId);
}
