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
@Table(name = Constants.tableOrderPaymentReceipt,schema = Constants.schemaOrder)
public class OrderPaymentReceipt {
    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "order_payment_receipt")
    private Long id;

    @Column(name = "order_id")
    private Long orderId;

    @Column(name = "payment_receipt_url")
    private String paymentReceiptUrl;

    @Column(name = "registration_date")
    @CreationTimestamp
    private Date registrationDate;

    @Column(name = "token_user")
    private String tokenUser;

    @Column(name = "client_id")
    private Long clientId;

    @ManyToOne
    @JoinColumn(name = "order_id",columnDefinition = "orderId",insertable = false,updatable = false)
    private Ordering ordering;

    @ManyToOne
    @JoinColumn(name = "client_id",columnDefinition = "clientId",insertable = false,updatable = false)
    private Client client;
}
