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
@Table(name = Constants.tableMembershipPayment, schema = Constants.schemaPayment)
public class MembershipPayment {

        @Id
        @GeneratedValue(strategy = GenerationType.UUID)
        @Column(name = "membership_payment_id", nullable = false)
        private UUID id;

        @Column(name = "net_amount", nullable = false)
        private Double netAmount;

        @Column(name = "gross_amount", nullable = false)
        private Double grossAmount;

        @Column(name = "payment_gateway_fee", nullable = false)
        private Double paymentGatewayFee;

        @Column(name = "tax_amount", nullable = false)
        private Double taxAmount;

        @Column(name = "registration_date", nullable = false)
        @CreationTimestamp
        private OffsetDateTime registrationDate;

        @Column(name = "update_date")
        @CreationTimestamp
        private OffsetDateTime updateDate;

        @Column(name = "payment_gateway_id")
        private UUID paymentGatewayId;

        @Column(name = "client_id")
        private UUID clientId;

        @Column(name = "user_id")
        private UUID userId;

        @ManyToOne()
        @JoinColumn(name = "payment_gateway_id",columnDefinition = "paymentGatewayId",insertable = false,updatable = false)
        private PaymentGateway paymentGateway;

        @ManyToOne
        @JoinColumn(name = "client_id",columnDefinition = "clientId",insertable = false,updatable = false)
        private Client client;

        @ManyToOne()
        @Column(name="user_id",columnDefinition = "userId",insertable = false,updatable = false)
        private User user;

}
