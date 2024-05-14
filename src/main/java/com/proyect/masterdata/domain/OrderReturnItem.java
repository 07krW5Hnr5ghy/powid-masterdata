package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;

import java.util.Date;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableOrderReturnItem, schema = Constants.schemaOrder)
public class OrderReturnItem {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "order_return_item_id")
    private Long id;

    @Column(name = "order_return_id")
    private Long orderReturnId;

    @Column(name = "order_stock_item_id")
    private Long orderStockItemId;

    @Column(name = "product_id")
    private Long productId;

    @Column(name = "order_item_id")
    private Long orderItemId;

    @Column(name = "order_return_type_id")
    private Long orderReturnTypeId;

    @Column(name = "supplier_product_id")
    private Long supplierProductId;

    @Column(name = "quantity")
    private Integer quantity;

    @Column(name = "order_id")
    private Long orderId;

    @Column(name = "registration_date")
    @CreationTimestamp()
    private Date registrationDate;

    @Column(name = "update_date")
    @CreationTimestamp()
    private Date updateDate;

    @Column(name = "status")
    private Boolean status;

    @Column(name = "token_user")
    private String tokenUser;

    @Column(name = "client_id")
    private Long clientId;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "order_return_id",columnDefinition = "orderReturnId",insertable = false,updatable = false)
    private OrderReturn orderReturn;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "order_stock_item_id",columnDefinition = "orderStockItemId",insertable = false,updatable = false)
    private OrderStockItem orderStockItem;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "product_id",columnDefinition = "productId",insertable = false,updatable = false)
    private Product product;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "order_return_type_id",columnDefinition = "orderReturnTypeId",insertable = false,updatable = false)
    private OrderReturnType orderReturnType;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "order_item_id",columnDefinition = "orderItemId",insertable = false,updatable = false)
    private OrderItem orderItem;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "supplier_product_id",columnDefinition = "supplierProductId",insertable = false,updatable = false)
    private SupplierProduct supplierProduct;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "order_id",columnDefinition = "orderId",insertable = false,updatable = false)
    private Ordering order;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "client_id", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;
}
