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

    @Column(name = "serial")
    private String serial;

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

    @Column(name = "shipment_type_id")
    private Long shipmentTypeId;

    @Column(name = "shipment_document_id")
    private Long shipmentDocumentId;

    @Column(name = "supplier_id")
    private Long supplierId;

    @Column(name = "token_user")
    private String tokenUser;

    @ManyToOne
    @JoinColumn(name = "client_id", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;

    @ManyToOne
    @JoinColumn(name = "warehouse_id", columnDefinition = "warehouseId", insertable = false, updatable = false)
    private Warehouse warehouse;

    @ManyToOne
    @JoinColumn(name = "shipment_document_id",columnDefinition = "shipmentDocumentId",insertable = false,updatable = false)
    private ShipmentDocument shipmentDocument;

    @ManyToOne
    @JoinColumn(name = "supplier_id",columnDefinition = "supplierId", insertable = false, updatable = false)
    private Supplier supplier;

    @ManyToOne
    @JoinColumn(name = "shipment_type_id",columnDefinition = "shipmentTypeId",insertable = false,updatable = false)
    private ShipmentType shipmentType;

}
