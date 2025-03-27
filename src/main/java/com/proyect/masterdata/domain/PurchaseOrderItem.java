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
@Table(name = Constants.tablePurchaseOrderItem,schema = Constants.schemaStock)
public class PurchaseOrderItem {
    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "purchase_order_item_id")
    private UUID id;

    @Column(name = "quantity")
    private Integer quantity;

    @Column(name = "observations",columnDefinition = "text")
    private String observations;

    @Column(name = "registration_date")
    @CreationTimestamp
    private OffsetDateTime registrationDate;

    @Column(name = "update_date")
    @CreationTimestamp
    private OffsetDateTime updateDate;

    @Column(name = "status")
    private Boolean status;

    @Column(name = "product_id")
    private UUID productId;

    @Column(name = "client_id")
    private UUID clientId;

    @Column(name = "purchase_order_id")
    private UUID purchaseOrderId;

    @Column(name = "user_id")
    private UUID userId;

    @ManyToOne
    @JoinColumn(name = "client_id", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;

    @ManyToOne
    @JoinColumn(name = "product_id", columnDefinition = "productId", insertable = false, updatable = false)
    private Product product;

    @ManyToOne
    @JoinColumn(name = "purchase_order_id",columnDefinition = "purchaseOrderId",insertable = false,updatable = false)
    private PurchaseOrder purchaseOrder;

    @ManyToOne()
    @JoinColumn(name="user_id",columnDefinition = "userId",insertable = false,updatable = false)
    private User user;
}
