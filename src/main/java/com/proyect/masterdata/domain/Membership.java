package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

import org.hibernate.annotations.CreationTimestamp;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableMembership, schema = Constants.schemaPayment)
public class Membership {

        @Id
        @GeneratedValue(strategy = GenerationType.AUTO)
        @Column(name = "membership_id")
        private Long id;

        @Column(name = "registration_date", nullable = false)
        @CreationTimestamp
        private Date registrationDate;

        @Column(name = "update_date")
        @CreationTimestamp
        private Date updateDate;

        @Column(name = "expiration_date")
        @CreationTimestamp
        private Date expirationDate;

        @Column(name = "status", nullable = false)
        private Boolean status;

        @Column(name = "demo", nullable = false)
        private Boolean demo;

        @Column(name = "client_id", nullable = false)
        private Long clientId;

        @Column(name = "subscription_id", nullable = false)
        private Long subscriptionId;

        @ManyToOne()
        @JoinColumn(name = "client_id", columnDefinition = "clientId", insertable = false, updatable = false)
        private Client client;

        @ManyToOne()
        @JoinColumn(name = "subscription_id", columnDefinition = "subscriptionId", insertable = false, updatable = false)
        private Subscription subscription;

}
