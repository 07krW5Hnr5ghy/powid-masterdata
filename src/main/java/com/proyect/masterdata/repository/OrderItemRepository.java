package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderItem;
import com.proyect.masterdata.dto.OrderDTO;
import com.proyect.masterdata.dto.SalesCategoryRawDTO;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.Date;
import java.util.List;

@Repository
public interface OrderItemRepository extends JpaRepository<OrderItem,Long> {
    List<OrderItem> findAllByOrderId(Long orderId);
    List<OrderItem> findAllByOrderIdAndStatusTrue(Long orderId);
    OrderItem findByIdAndOrderId(Long itemId, Long orderId);
    OrderItem findByProductIdAndOrderId(Long productId,Long orderId);
    OrderItem findByOrderIdAndProductId(Long orderId,Long productId);
    List<OrderItem> findAllByClientIdAndStatusTrue(Long clientId);
    List<OrderItem> findAllByClientIdAndOrderIdAndStatusTrue(Long clientId,Long orderId);
    List<OrderItem> findAllByClientIdAndOrderIdAndStatusFalse(Long clientId,Long orderId);
    @Query(value = "SELECT " +
            "o.order_id AS orderId, " +
            "o.registration_date AS registrationDate, " +
            "o.client_id AS clientId, " +
            "o.discount_amount AS orderDiscountAmount, " +
            "d1.name AS orderDiscountName, " +
            "sc.name AS saleChannelName, " +
            "oi.order_item_id AS orderItemId, " +
            "oi.quantity AS quantity, " +
            "oi.discount_amount AS orderItemDiscountAmount, " +
            "d2.name AS orderItemDiscountName, " +
            "cp.name AS categoryName, " +
            "pp.unit_sale_price AS unitSalePrice " +
            "FROM ordering.order o " +
            "JOIN ordering.order_item oi ON o.order_id = oi.order_id " +
            "JOIN marketing.product p ON oi.product_id = p.product_id " +
            "JOIN master.category_product cp ON p.category_product_id = cp.category_product_id " +
            "JOIN marketing.product_price pp ON p.product_id = pp.product_id AND pp.status = TRUE " +
            "LEFT JOIN master.discount d1 ON o.discount_id = d1.discount_id " +
            "LEFT JOIN master.discount d2 ON oi.discount_id = d2.discount_id " +
            "LEFT JOIN master.sale_channel sc ON o.sale_channel_id = sc.sale_channel_id " +
            "WHERE o.registration_date BETWEEN :startDate AND :endDate " +
            "AND o.client_id = :clientId", nativeQuery = true)
    List<Object[]> findOrderItemsByDateRangeAndClientId(
            @Param("startDate") Date startDate,
            @Param("endDate") Date endDate,
            @Param("clientId") Long clientId
    );
}
