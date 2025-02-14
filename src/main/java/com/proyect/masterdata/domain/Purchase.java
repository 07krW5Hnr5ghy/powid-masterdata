package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;

import java.time.OffsetDateTime;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tablePurchase, schema = Constants.schemaStock)
public class Purchase {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "purchase_id")
    private String id;

    @Column(name = "serial")
    private String serial;

    @Column(name = "registration_date")
    @CreationTimestamp
    private OffsetDateTime registrationDate;

    @Column(name = "update_date")
    @CreationTimestamp
    private OffsetDateTime updateDate;

    @Column(name = "status")
    private Boolean status;

    @Column(name = "client_id")
    private Long clientId;

    @Column(name = "warehouse_id")
    private Long warehouseId;

    @Column(name = "purchase_type_id")
    private Long purchaseTypeId;

    @Column(name = "purchase_document_id")
    private Long purchaseDocumentId;

    @Column(name = "supplier_id")
    private Long supplierId;

    @Column(name = "user_id")
    private String userId;

    @ManyToOne
    @JoinColumn(name = "client_id", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;

    @ManyToOne
    @JoinColumn(name = "warehouse_id", columnDefinition = "warehouseId", insertable = false, updatable = false)
    private Warehouse warehouse;

    @ManyToOne
    @JoinColumn(name = "purchase_document_id",columnDefinition = "purchaseDocumentId",insertable = false,updatable = false)
    private PurchaseDocument purchaseDocument;

    @ManyToOne
    @JoinColumn(name = "supplier_id",columnDefinition = "supplierId", insertable = false, updatable = false)
    private Supplier supplier;

    @ManyToOne
    @JoinColumn(name = "purchase_type_id",columnDefinition = "purchaseTypeId",insertable = false,updatable = false)
    private PurchaseType purchaseType;

    @ManyToOne()
    @Column(name="user_id",columnDefinition = "userId",insertable = false,updatable = false)
    private User user;

}
