package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;

import java.time.OffsetDateTime;
import java.util.UUID;

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
    private UUID id;

    @Column(name = "ref")
    private String ref;

    @Column(name = "purchaseNumber")
    private Long purchaseNumber;

    @Column(name = "registration_date")
    @CreationTimestamp
    private OffsetDateTime registrationDate;

    @Column(name = "delivery_date")
    @CreationTimestamp
    private OffsetDateTime deliveryDate;

    @Column(name = "update_date")
    @CreationTimestamp
    private OffsetDateTime updateDate;

    @Column(name = "status")
    private Boolean status;

    @Column(name = "client_id")
    private UUID clientId;

    @Column(name = "warehouse_id")
    private UUID warehouseId;

    @Column(name = "purchase_type_id")
    private UUID purchaseTypeId;

    @Column(name = "purchase_document_id")
    private UUID purchaseDocumentId;

    @Column(name = "supplier_id")
    private UUID supplierId;

    @Column(name = "user_id")
    private UUID userId;

    @Column(name = "purchase_payment_type_id")
    private UUID purchasePaymentTypeId;

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
    @JoinColumn(name="user_id",columnDefinition = "userId",insertable = false,updatable = false)
    private User user;

    @ManyToOne()
    @JoinColumn(name="purchase_payment_type_id",columnDefinition = "purchasePaymentId",insertable = false,updatable = false)
    private PurchasePaymentType purchasePaymentType;

}
