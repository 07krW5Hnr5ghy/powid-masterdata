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
@NoArgsConstructor
@AllArgsConstructor
@Data
@Table(name = Constants.tableStockReturnItem, schema = Constants.schemaStock)
public class StockReturnItem {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "stock_return_item_id")
    private Long id;

    @Column(name = "quantity")
    private Integer quantity;

    @Column(name = "observations")
    private String observations;

    @Column(name = "supplier_product_id")
    private Long supplierProductId;

    @Column(name = "shipment_id")
    private Long shipmentId;

    @Column(name = "registration_date")
    private Date registrationDate;

    @Column(name = "client_id")
    private Long clientId;

    @Column(name = "shipment_item_id")
    private Long shipmentItemId;

    @Column(name = "status")
    private Boolean status;

    @Column(name = "stock_return_id")
    private Long stockReturnId;

    @Column(name = "token_user")
    private String tokenUser;

    @ManyToOne
    @JoinColumn(name = "supplier_product_id", columnDefinition = "supplierProduct", insertable = false, updatable = false)
    private SupplierProduct supplierProduct;

    @ManyToOne
    @JoinColumn(name = "client_id", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;

    @ManyToOne
    @JoinColumn(name = "shipment_id",columnDefinition = "shipmentId",insertable = false,updatable = false)
    private Shipment shipment;

    @ManyToOne
    @JoinColumn(name = "shipment_item_id",columnDefinition = "shipmentItemId",insertable = false,updatable = false)
    private ShipmentItem shipmentItem;

    @ManyToOne
    @JoinColumn(name = "stock_return_id", columnDefinition = "stockReturnId", insertable = false, updatable = false)
    private StockReturn stockReturn;
}
