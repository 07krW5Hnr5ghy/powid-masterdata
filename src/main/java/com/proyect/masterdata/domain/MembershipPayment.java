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

        @Column(name = "months", nullable = false)
        private Integer months;

        @Column(name = "net_amount", nullable = false)
        private Double netAmount;

        @Column(name = "gross_amount", nullable = false)
        private Double grossAmount;

        @Column(name = "invoice_url")
        private String invoiceUrl;

        @Column(name = "registration_date", nullable = false)
        @CreationTimestamp
        private Date registrationDate;

        @Column(name = "update_date")
        @CreationTimestamp
        private Date updateDate;

        @Column(name = "status", nullable = false)
        private Boolean status;

        @Column(name = "membership_id", nullable = false)
        private Long membershipId;

        @Column(name = "payment_state_id", nullable = false)
        private Long paymentStateId;

        @Column(name = "payment_method_id", nullable = false)
        private Long paymentMethodId;

        @ManyToOne()
        @JoinColumn(name = "membership_id", columnDefinition = "membershipId", insertable = false, updatable = false)
        private Membership membership;

        @ManyToOne()
        @JoinColumn(name = "payment_state_id", columnDefinition = "paymentStateId", insertable = false, updatable = false)
        private PaymentState paymentState;

        @ManyToOne()
        @JoinColumn(name = "payment_method_id", columnDefinition = "paymentMethodId", insertable = false, updatable = false)
        private PaymentMethod paymentMethod;

}
