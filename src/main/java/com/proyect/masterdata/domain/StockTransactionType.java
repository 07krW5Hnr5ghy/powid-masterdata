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
@Table(name = Constants.tableStockTransactionType, schema = Constants.schemaStock)
public class StockTransactionType {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "stock_transaction_type_id")
    private UUID id;

    @Column(name = "name")
    private String name;

    @Column(name = "registration_date")
    @CreationTimestamp
    private OffsetDateTime registrationDate;

    @Column(name = "update_date")
    @CreationTimestamp
    private OffsetDateTime updateDate;

    @Column(name = "status")
    private Boolean status;

    @Column(name = "user_id")
    private UUID userId;

    @ManyToOne()
    @JoinColumn(name="user_id",columnDefinition = "userId",insertable = false,updatable = false)
    private User user;
}
