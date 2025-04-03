package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderItem;
import com.proyect.masterdata.dto.OrderDTO;
import com.proyect.masterdata.dto.SalesCategoryRawDTO;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.time.OffsetDateTime;
import java.util.Date;
import java.util.List;
import java.util.UUID;

@Repository
public interface OrderItemRepository extends JpaRepository<OrderItem, UUID> {
    List<OrderItem> findAllByOrderId(UUID orderId);
    OrderItem findOrderItemById(UUID orderItemId);
    List<OrderItem> findAllByOrderIdAndStatusTrue(UUID orderId);
    OrderItem findByIdAndOrderId(UUID itemId, UUID orderId);
    OrderItem findByProductId(UUID productId);
    OrderItem findByProductIdAndOrderId(UUID productId,UUID orderId);
    OrderItem findByOrderIdAndProductId(UUID orderId,UUID productId);
    List<OrderItem> findAllByClientIdAndStatusTrue(UUID clientId);
    List<OrderItem> findAllByClientIdAndStatusTrueAndSelectOrderStatusTrue(UUID clientId);
    List<OrderItem> findAllByClientIdAndOrderIdAndStatusTrue(UUID clientId,UUID orderId);
    List<OrderItem> findAllByClientIdAndOrderIdAndStatusFalse(UUID clientId,UUID orderId);

    @Query("SELECT o FROM OrderItem o " +
            "WHERE o.orderId = :orderId " +
            "AND o.status = true " +
            "AND o.preparedProducts > 0")
    List<OrderItem> findOrderItemsForOrder(@Param("orderId") UUID orderId);

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
            "pp.unit_sale_price AS unitSalePrice, " +
            "oi.status AS orderItemStatus " +
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
            @Param("startDate") OffsetDateTime startDate,
            @Param("endDate") OffsetDateTime endDate,
            @Param("clientId") UUID clientId
    );
    @Query(value = "SELECT " +
            "o.order_id AS orderId, " +
            "o.registration_date AS registrationDate, " +
            "o.client_id AS clientId, " +
            "o.discount_amount AS orderDiscountAmount, " +
            "d1.name AS orderDiscountName, " +
            "oi.order_item_id AS orderItemId, " +
            "oi.quantity AS quantity, " +
            "oi.discount_amount AS orderItemDiscountAmount, " +
            "d2.name AS orderItemDiscountName, " +
            "cp.name AS categoryName, " +
            "pp.unit_sale_price AS unitSalePrice, " +
            "b.name AS brandName, " +
            "cc.name AS closingChannelName, " +
            "oi.status AS orderItemStatus " +
            "FROM ordering.order o " +
            "JOIN ordering.order_item oi ON o.order_id = oi.order_id " +
            "JOIN marketing.product p ON oi.product_id = p.product_id " +
            "JOIN master.category_product cp ON p.category_product_id = cp.category_product_id " +
            "JOIN marketing.product_price pp ON p.product_id = pp.product_id AND pp.status = TRUE " +
            "JOIN marketing.model m ON m.model_id = p.product_id " +
            "JOIN marketing.brand b ON b.brand_id = m.brand_id " +
            "LEFT JOIN master.discount d1 ON o.discount_id = d1.discount_id " +
            "LEFT JOIN master.discount d2 ON oi.discount_id = d2.discount_id " +
            "LEFT JOIN master.closing_channel cc ON o.closing_channel_id = cc.closing_channel_id " +
            "WHERE o.registration_date BETWEEN :startDate AND :endDate " +
            "AND o.client_id = :clientId", nativeQuery = true)
    List<Object[]> findOrderItemsWithBrandByDateRangeAndClientId(
            @Param("startDate") OffsetDateTime startDate,
            @Param("endDate") OffsetDateTime endDate,
            @Param("clientId") UUID clientId
    );

    @Query(value = "SELECT " +
            "o.order_id AS orderId, " +
            "o.registration_date AS registrationDate, " +
            "o.client_id AS clientId, " +
            "o.discount_amount AS orderDiscountAmount, " +
            "d1.name AS orderDiscountName, " +
            "oi.order_item_id AS orderItemId, " +
            "oi.quantity AS quantity, " +
            "oi.discount_amount AS orderItemDiscountAmount, " +
            "d2.name AS orderItemDiscountName, " +
            "cp.name AS categoryName, " +
            "pp.unit_sale_price AS unitSalePrice, " +
            "b.name AS brandName, " +
            "cc.name AS closingChannelName, " +
            "o.seller AS seller, " +
            "de.name AS departmentName, " +
            "pr.name AS provinceName, " +
            "di.name AS districtName, " +
            "os.name AS orderStateName, " +
            "oi.status AS orderItemStatus " +
            "FROM ordering.order o " +
            "JOIN ordering.order_item oi ON o.order_id = oi.order_id " +
            "JOIN master.order_state os ON os.order_state_id = o.order_state_id " +
            "JOIN marketing.product p ON oi.product_id = p.product_id " +
            "JOIN master.category_product cp ON p.category_product_id = cp.category_product_id " +
            "JOIN marketing.product_price pp ON p.product_id = pp.product_id AND pp.status = TRUE " +
            "JOIN marketing.model m ON m.model_id = p.product_id " +
            "JOIN marketing.brand b ON b.brand_id = m.brand_id " +
            "LEFT JOIN master.discount d1 ON o.discount_id = d1.discount_id " +
            "LEFT JOIN master.discount d2 ON oi.discount_id = d2.discount_id " +
            "LEFT JOIN master.closing_channel cc ON o.closing_channel_id = cc.closing_channel_id " +
            "LEFT JOIN ordering.customer cu ON o.customer_id = cu.customer_id " +
            "JOIN master.district di ON di.district_id = cu.district_id " +
            "JOIN master.province pr ON pr.province_id = di.province_id " +
            "JOIN master.department de ON de.department_id = pr.department_id " +
            "WHERE o.registration_date BETWEEN :startDate AND :endDate " +
            "AND o.client_id = :clientId", nativeQuery = true)
    List<Object[]> findOrderItemsWithSellerByDateRangeAndClientId(
            @Param("startDate") OffsetDateTime startDate,
            @Param("endDate") OffsetDateTime endDate,
            @Param("clientId") UUID clientId
    );
    OrderItem findByIdAndStatusTrue(UUID orderItemId);

    @Transactional
    @Modifying
    @Query("UPDATE OrderItem o SET " +
            "o.quantity = :quantity, " +
            "o.discountAmount = :discountAmount, " +
            "o.observations = :observations, " +
            "o.discountId = :discountId, " +
            "o.updateDate = :updateDate " +
            "WHERE o.id = :id")
    void updateOrderItemFields(
            @Param("id") UUID id,
            @Param("quantity") Integer quantity,
            @Param("discountAmount") Double discountAmount,
            @Param("observations") String observations,
            @Param("discountId") UUID discountId,
            @Param("updateDate") OffsetDateTime updateDate
    );


    @Modifying
    @Transactional
    @Query("UPDATE OrderItem o SET " +
            "o.status = :status, " +
            "o.updateDate = :updateDate " +
            "WHERE o.userId = :userId AND o.orderId = :orderId AND o.productId = :productId")
    void deleteAndActivateOrderItemLogically (
            @Param("orderId") UUID orderId,
            @Param("productId") UUID productId,
            @Param("updateDate") OffsetDateTime updateDate,
            @Param("userId") UUID userId,
            @Param("status") boolean status
    );

    @Modifying
    @Transactional
    @Query("UPDATE OrderItem o SET " +
                    "o.updateDate = :updateDate, " +
                    "o.preparedProducts = :preparedProducts " +
                    "WHERE o.userId = :userId AND o.orderId = :orderId AND o.id = :orderItemId")
    void selectPreparedOrdetItem (
            @Param("orderId") UUID orderId,
            @Param("orderItemId") UUID orderItemId,
            @Param("userId") UUID userId,
            @Param("updateDate") OffsetDateTime updateDate,
            @Param("preparedProducts") Integer preparedProducts
    );

    @Query("""
    SELECT oi.preparedProducts,
    oi.discountAmount,
    di.name
    FROM OrderItem oi
    JOIN oi.discount di
    WHERE oi.id = :orderItemId AND oi.clientId = :clientId
    """)
    List<Object[]> findOrderItemDetailsByIdAndClientId(
            UUID orderItemId,
            UUID clientId
    );
}
