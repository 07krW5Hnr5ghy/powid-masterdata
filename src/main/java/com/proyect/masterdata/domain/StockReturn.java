package com.proyect.masterdata.domain;

import java.util.Date;

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
@NoArgsConstructor
@AllArgsConstructor
@Data
@Table(name = Constants.tableStockReturn, schema = Constants.schemaStock)
public class StockReturn {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "stock_return_id")
    private Long id;

    @Column(name = "quantity")
    private Integer quantity;

    @Column(name = "observations")
    private String observations;

    @Column(name = "registration_date")
    private Date registrationDate;

    @Column(name = "update_date")
    private Date updateDate;

    @Column(name = "status")
    private Boolean status;

    @Column(name = "supplier_product_id")
    private Long supplierProductId;

    @Column(name = "purchase_id")
    private Long purchaseId;

    @Column(name = "client_id")
    private Long clientId;

    @Column(name = "token_user")
    private String tokenUser;

    @ManyToOne
    @JoinColumn(name = "supplier_product_id", columnDefinition = "supplierProduct", insertable = false, updatable = false)
    private SupplierProduct supplierProduct;

    @ManyToOne
    @JoinColumn(name = "purchase_id", columnDefinition = "purchaseId", insertable = false, updatable = false)
    private PurchaseItem purchaseItem;

    @ManyToOne
    @JoinColumn(name = "client_id", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;

}
