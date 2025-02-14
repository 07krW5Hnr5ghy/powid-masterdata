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
@Table(name = Constants.tableOrderStockItem, schema = Constants.schemaStock)
public class OrderStockItem {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "order_stock_item_id")
    private String id;

    @Column(name = "quantity")
    private Integer quantity;

    @Column(name = "registration_date")
    @CreationTimestamp
    private Date registrationDate;

    @Column(name = "update_date")
    @CreationTimestamp
    private Date updateDate;

    @Column(name = "status")
    private Boolean status;

    @Column(name = "item_id")
    private Long orderItemId;

    @Column(name = "supplier_product_id")
    private Long supplierProductId;

    @Column(name = "order_stock_id")
    private Long orderStockId;

    @Column(name = "client_id")
    private Long clientId;

    @Column(name = "order_id")
    private Long orderId;

    @Column(name = "token_user")
    private String tokenUser;

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

}
