package com.proyect.masterdata.domain;

import java.time.OffsetDateTime;
import java.util.UUID;

import jakarta.persistence.*;
import org.hibernate.annotations.CreationTimestamp;

import com.proyect.masterdata.utils.Constants;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableSubscription, schema = Constants.schemaPayment)
public class Subscription {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "subscription_id")
    private UUID id;

    @Column(name = "name", nullable = false)
    private String name;

    @Column(name = "months", nullable = false)
    private Integer months;

    @Column(name = "discount_percent", nullable = false)
    private Double discountPercent;

    @Column(name = "registration_date", nullable = false)
    @CreationTimestamp
    private OffsetDateTime registrationDate;

    @Column(name = "update_date")
    @CreationTimestamp
    private OffsetDateTime updateDate;

    @Column(name = "status", nullable = false)
    private Boolean status;

    @Column(name = "user_id")
    private UUID userId;

    @ManyToOne()
    @JoinColumn(name="user_id",columnDefinition = "userId",insertable = false,updatable = false)
    private User user;

}
