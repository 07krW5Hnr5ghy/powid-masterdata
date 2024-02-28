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
@Table(name = Constants.tableMembershipPayment, schema = Constants.schemaPayment)
public class MembershipPayment {

        @Id
        @GeneratedValue(strategy = GenerationType.AUTO)
        @Column(name = "membership_payment_id", nullable = false)
        private Long id;

        @Column(name = "net_amount", nullable = false)
        private Double netAmount;

        @Column(name = "gross_amount", nullable = false)
        private Double grossAmount;

        @Column(name = "payment_gateway_fee", nullable = false)
        private Double paymentGatewayFee;

        @Column(name = "tax_amount", nullable = false)
        private Double taxAmount;

        @Column(name = "payment_reference")
        private Long paymentReference;

        @Column(name = "registration_date", nullable = false)
        @CreationTimestamp
        private Date registrationDate;

        @Column(name = "update_date")
        @CreationTimestamp
        private Date updateDate;

        @Column(name = "payment_gateway_id")
        private Long paymentGatewayId;

        @ManyToOne()
        @JoinColumn(name = "payment_gateway_id",columnDefinition = "paymentGatewayId",insertable = false,updatable = false)
        private PaymentGateway paymentGateway;

}
