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
@Table(name = Constants.tableOrderPaymentReceipt,schema = Constants.schemaOrder)
public class OrderPaymentReceipt {
    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "order_payment_receipt")
    private UUID id;

    @Column(name = "order_id")
    private UUID orderId;

    @Column(name = "payment_receipt_url")
    private String paymentReceiptUrl;

    @Column(name = "registration_date")
    @CreationTimestamp
    private OffsetDateTime registrationDate;

    @Column(name = "user_id")
    private UUID userId;

    @Column(name = "client_id")
    private UUID clientId;

    @ManyToOne
    @JoinColumn(name = "order_id",columnDefinition = "orderId",insertable = false,updatable = false)
    private Ordering ordering;

    @ManyToOne
    @JoinColumn(name = "client_id",columnDefinition = "clientId",insertable = false,updatable = false)
    private Client client;

    @ManyToOne()
    @JoinColumn(name="user_id",columnDefinition = "userId",insertable = false,updatable = false)
    private User user;
}
