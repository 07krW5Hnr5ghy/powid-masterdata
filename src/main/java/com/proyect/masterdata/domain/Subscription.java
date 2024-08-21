package com.proyect.masterdata.domain;

import java.util.Date;

import org.hibernate.annotations.CreationTimestamp;

import com.proyect.masterdata.utils.Constants;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
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
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "subscription_id")
    private Long id;

    @Column(name = "name", nullable = false)
    private String name;

    @Column(name = "months", nullable = false)
    private Integer months;

    @Column(name = "discount_percent", nullable = false)
    private Double discountPercent;

    @Column(name = "registration_date", nullable = false)
    @CreationTimestamp
    private Date registrationDate;

    @Column(name = "update_date")
    @CreationTimestamp
    private Date updateDate;

    @Column(name = "status", nullable = false)
    private Boolean status;

    @Column(name = "token_user", nullable = false)
    private String tokenUser;

}
