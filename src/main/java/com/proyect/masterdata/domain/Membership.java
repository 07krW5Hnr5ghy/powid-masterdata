package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.OffsetDateTime;

import org.hibernate.annotations.CreationTimestamp;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableMembership, schema = Constants.schemaPayment)
public class Membership {

        @Id
        @GeneratedValue(strategy = GenerationType.UUID)
        @Column(name = "membership_id")
        private String id;

        @Column(name = "registration_date", nullable = false)
        @CreationTimestamp
        private OffsetDateTime registrationDate;

        @Column(name = "update_date")
        @CreationTimestamp
        private OffsetDateTime updateDate;

        @Column(name = "expiration_date")
        @CreationTimestamp
        private OffsetDateTime expirationDate;

        @Column(name = "demo", nullable = false)
        private Boolean demo;

        @Column(name = "client_id", nullable = false)
        private Long clientId;

        @Column(name = "subscription_id", nullable = false)
        private Long subscriptionId;

        @Column(name = "membership_payment_id",nullable = false)
        private Long membershipPaymentId;

        @Column(name = "membership_state_id")
        private Long membershipStateId;

        @ManyToOne()
        @JoinColumn(name = "client_id", columnDefinition = "clientId", insertable = false, updatable = false)
        private Client client;

        @ManyToOne()
        @JoinColumn(name = "subscription_id", columnDefinition = "subscriptionId", insertable = false, updatable = false)
        private Subscription subscription;

        @ManyToOne()
        @JoinColumn(name = "membership_payment_id",columnDefinition = "membershipPaymentId",insertable = false,updatable = false)
        private MembershipPayment membershipPayment;

        @ManyToOne()
        @JoinColumn(name = "membership_state_id",columnDefinition = "membershipStateId",insertable = false,updatable = false)
        private MembershipState membershipState;

        @ManyToOne()
        @Column(name="user_id",columnDefinition = "userId",insertable = false,updatable = false)
        private User user;

}
