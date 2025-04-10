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
@Table(name = Constants.tableDeliveryManifestOrder,schema = Constants.schemaLogistics)
public class DeliveryManifestOrder {
    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "delivery_manifest_order_id")
    private UUID id;

    @Column(name = "delivery_manifest_id")
    private UUID deliveryManifestId;

    @Column(name = "order_id")
    private UUID orderId;

    @Column(name = "observations",columnDefinition = "text")
    private String observations;

    @Column(name = "received_amount")
    private Double receivedAmount;

    @Column(name = "registration_date")
    @CreationTimestamp()
    private OffsetDateTime registrationDate;

    @Column(name = "update_date")
    @CreationTimestamp()
    private OffsetDateTime updateDate;

    @Column(name = "delivery_fee_collected")
    private Boolean deliveryFeeCollected;

    @Column(name = "delivered")
    private Boolean delivered;

    @Column(name = "user_id")
    private UUID userId;

    @Column(name = "client_id")
    private UUID clientId;

    @Column(name="payment_method_id")
    private UUID paymentMethodId;

    @ManyToOne()
    @JoinColumn(name="user_id",columnDefinition = "userId",insertable = false,updatable = false)
    private User user;

    @ManyToOne()
    @JoinColumn(name="client_id",columnDefinition = "clientId",insertable = false,updatable = false)
    private Client client;

    @ManyToOne()
    @JoinColumn(name="order_id",columnDefinition = "orderId",insertable = false,updatable = false)
    private Ordering ordering;

    @ManyToOne()
    @JoinColumn(name="delivery_manifest_id",columnDefinition = "deliveryManifestId",insertable = false,updatable = false)
    private DeliveryManifest deliveryManifest;

    @ManyToOne()
    @JoinColumn(name = "payment_method_id",columnDefinition = "paymentMethodId",insertable = false,updatable = false)
    private OrderPaymentMethod orderPaymentMethod;

}
