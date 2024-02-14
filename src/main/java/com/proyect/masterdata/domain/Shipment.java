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
@Table(name = Constants.tableShipment, schema = Constants.schemaStock)
public class Shipment {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "shipment_id")
    private Long id;

    @Column(name = "purchaseSerial")
    private String purchaseSerial;

    @Column(name = "registration_date")
    @CreationTimestamp
    private Date registrationDate;

    @Column(name = "update_date")
    @CreationTimestamp
    private Date updateDate;

    @Column(name = "status")
    private Boolean status;

    @Column(name = "client_id")
    private Long clientId;

    @Column(name = "warehouse_id")
    private Long warehouseId;

    @Column(name = "stock_transaction_id")
    private Long stockTransactionId;

    @Column(name = "purchase_id")
    private Long purchaseId;

    @Column(name = "shipment_type_id")
    private Long shipmentTypeId;

    @Column(name = "token_user")
    private String tokenUser;

    @ManyToOne
    @JoinColumn(name = "client_id", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;

    @ManyToOne
    @JoinColumn(name = "warehouse_id", columnDefinition = "warehouseId", insertable = false, updatable = false)
    private Warehouse warehouse;

    @ManyToOne
    @JoinColumn(name = "stock_transaction_id", columnDefinition = "stockTransactionId", insertable = false, updatable = false)
    private StockTransaction stockTransaction;

    @ManyToOne
    @JoinColumn(name = "purchase_id", columnDefinition = "purchaseId", insertable = false, updatable = false)
    private Purchase purchase;

    @ManyToOne
    @JoinColumn(name = "shipment_type_id",columnDefinition = "shipmentTypeId",insertable = false,updatable = false)
    private ShipmentType shipmentType;
}
