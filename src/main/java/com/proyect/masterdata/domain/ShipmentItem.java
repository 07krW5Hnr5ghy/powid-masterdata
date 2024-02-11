package com.proyect.masterdata.domain;

import java.util.Date;

import org.hibernate.annotations.CreationTimestamp;

import com.proyect.masterdata.utils.Constants;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableShipmentItem, schema = Constants.schemaInventory)
public class ShipmentItem {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "shipment_item_id")
    private Long id;

    @Column(name = "serial")
    private String serial;

    @Column(name = "quantity")
    private Integer quantity;

    @Column(name = "observations")
    private String observations;

    @Column(name = "registration_date")
    @CreationTimestamp
    private Date registrationDate;

    @Column(name = "update_date")
    @CreationTimestamp
    private Date updateDate;

    @Column(name = "status")
    private Boolean status;

    @Column(name = "supplier_product_id")
    private Long supplierProductId;

    @Column(name = "client_id")
    private Long clientId;

    @Column(name = "purchase_id")
    private Long purchaseId;

    @Column(name = "stock_transaction_id")
    private Long stockTransactionId;

    @Column(name = "warehouse_id")
    private Long warehouseId;

    @Column(name = "token_user")
    private String tokenUser;

    @ManyToOne
    @JoinColumn(name = "client_id", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;

    @ManyToOne
    @JoinColumn(name = "supplier_product_id", columnDefinition = "supplierProductId", insertable = false, updatable = false)
    private SupplierProduct supplierProduct;

    @ManyToOne
    @JoinColumn(name = "purchase_id", columnDefinition = "purchaseId", insertable = false, updatable = false)
    private Purchase purchase;

    @ManyToOne
    @JoinColumn(name = "stock_transaction_id", columnDefinition = "stockTransactionId", insertable = false, updatable = false)
    private StockTransaction stockTransaction;

    @ManyToOne
    @JoinColumn(name = "warehouse_id", columnDefinition = "warehouseId", insertable = false, updatable = false)
    private Warehouse warehouse;
}
