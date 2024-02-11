package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableOrderStock, schema = Constants.schemaStock)
public class OrderStock {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "order_stock_id")
    private Long id;

    @Column(name = "quantity")
    private Integer quantity;

    @Column(name = "registration_date")
    private Date registrationDate;

    @Column(name = "update_date")
    private Date updateDate;

    @Column(name = "status")
    private Boolean status;

    @Column(name = "item_id")
    private Long itemId;

    @Column(name = "supplier_product_id")
    private Long supplierProductId;

    @Column(name = "order_id")
    private Long orderId;

    @Column(name = "client_id")
    private Long clientId;

    @Column(name = "warehouse_id")
    private Long warehouseId;

    @Column(name = "token_user")
    private String tokenUser;

    @ManyToOne
    @JoinColumn(name = "item_id", columnDefinition = "itemId", insertable = false, updatable = false)
    private Item item;

    @ManyToOne
    @JoinColumn(name = "warehouse_id",columnDefinition = "warehouseId",insertable = false,updatable = false)
    private Warehouse warehouse;

    @ManyToOne
    @JoinColumn(name = "order_id", columnDefinition = "orderId", insertable = false, updatable = false)
    private Ordering ordering;

    @ManyToOne
    @JoinColumn(name = "client_id", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;

    @ManyToOne
    @JoinColumn(name = "supplier_product_id",columnDefinition = "supplierProduct",insertable = false,updatable = false)
    private SupplierProduct supplierProduct;

}
