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
@Table(name = Constants.tableWarehouseStock, schema = Constants.schemaStock)
public class WarehouseStock {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "warehouse_stock_id")
    private Long id;

    @Column(name = "quantity")
    private Integer quantity;

    @Column(name = "registrationDate")
    @CreationTimestamp
    private Date registrationDate;

    @Column(name = "updateDate")
    @CreationTimestamp
    private Date updateDate;

    @Column(name = "warehouse_id")
    private Long warehouseId;

    @Column(name = "supplier_product_id")
    private Long supplierProductId;

    @Column(name = "client_id")
    private Long clientId;

    @Column(name = "token_user")
    private String tokenUser;

    @ManyToOne
    @JoinColumn(name = "warehouse_id", columnDefinition = "warehouseId", insertable = false, updatable = false)
    private Warehouse warehouse;

    @ManyToOne
    @JoinColumn(name = "supplier_product_id", columnDefinition = "supplierProductId", insertable = false, updatable = false)
    private SupplierProduct supplierProduct;

    @ManyToOne
    @JoinColumn(name = "client_id", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;

}
