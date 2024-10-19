package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.CategoryProduct;
import com.proyect.masterdata.domain.OrderItem;
import com.proyect.masterdata.domain.Ordering;
import com.proyect.masterdata.domain.SaleChannel;
import org.springframework.stereotype.Repository;

import java.util.Date;
import java.util.List;

@Repository
public interface StatsRepository {
    List<CategoryProduct> findAllCategoryProducts();
    List<SaleChannel> findAllSaleChannels();
    List<Ordering> findOrdersByClientAndRegistrationDate(Long clientId, Date startDate, Date endDate);
    List<OrderItem> findOrderItemsByClientAndOrder(Long clientId, Long orderId);
}
