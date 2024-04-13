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

    @Column(name = "oder_stock_item_id")
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

    @Column(name = "registration_date")
    @CreationTimestamp()
    private Date registrationDate;

    @Column(name = "update_date")
    @CreationTimestamp()
    private Date updateDate;

    @Column(name = "status")
    private Boolean status;

    @ManyToOne()
    @JoinColumn(name = "order_return_id",columnDefinition = "orderReturnId",insertable = false,updatable = false)
    private OrderReturn orderReturn;

    @ManyToOne()
    @JoinColumn(name = "order_stock_item_id",columnDefinition = "orderStockItemId",insertable = false,updatable = false)
    private OrderStockItem orderStockItem;

    @ManyToOne()
    @JoinColumn(name = "product_id",columnDefinition = "productId",insertable = false,updatable = false)
    private Product product;

    @ManyToOne()
    @JoinColumn(name = "order_return_type_id",columnDefinition = "orderReturnTypeId",insertable = false,updatable = false)
    private OrderReturnType orderReturnType;

    @ManyToOne()
    @JoinColumn(name = "order_item_id",columnDefinition = "orderItemId",insertable = false,updatable = false)
    private OrderItem orderItem;

    @ManyToOne()
    @JoinColumn(name = "supplier_product_id",columnDefinition = "supplierProductId",insertable = false,updatable = false)
    private SupplierProduct supplierProduct;
}
