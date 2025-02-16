package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;

import java.time.OffsetDateTime;
import java.util.UUID;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableOrderStockItem, schema = Constants.schemaStock)
public class OrderStockItem {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "order_stock_item_id")
    private UUID id;

    @Column(name = "quantity")
    private Integer quantity;

    @Column(name = "registration_date")
    @CreationTimestamp
    private OffsetDateTime registrationDate;

    @Column(name = "update_date")
    @CreationTimestamp
    private OffsetDateTime updateDate;

    @Column(name = "status")
    private Boolean status;

    @Column(name = "item_id")
    private UUID orderItemId;

    @Column(name = "supplier_product_id")
    private UUID supplierProductId;

    @Column(name = "order_stock_id")
    private UUID orderStockId;

    @Column(name = "client_id")
    private UUID clientId;

    @Column(name = "order_id")
    private UUID orderId;

    @Column(name = "user_id")
    private UUID userId;

    @ManyToOne
    @JoinColumn(name = "item_id", columnDefinition = "orderItemId", insertable = false, updatable = false)
    private OrderItem orderItem;

    @ManyToOne
    @JoinColumn(name = "client_id", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;

    @ManyToOne
    @JoinColumn(name = "supplier_product_id",columnDefinition = "supplierProduct",insertable = false,updatable = false)
    private SupplierProduct supplierProduct;

    @ManyToOne
    @JoinColumn(name = "order_stock_id",columnDefinition = "orderStockId",insertable = false,updatable = false)
    private OrderStock orderStock;

    @ManyToOne
    @JoinColumn(name = "order_id",columnDefinition = "orderId",insertable = false,updatable = false)
    private Ordering ordering;

    @ManyToOne()
    @JoinColumn(name="user_id",columnDefinition = "userId",insertable = false,updatable = false)
    private User user;

}
