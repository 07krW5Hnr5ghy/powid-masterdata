package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.OffsetDateTime;
import java.util.UUID;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableStockTransaction, schema = Constants.schemaStock)
public class StockTransaction {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "stock_transaction_id")
    private UUID id;

    @Column(name = "serial")
    private String serial;

    @Column(name = "stock_transaction_type_id")
    private UUID stockTransactionTypeId;

    @Column(name = "warehouse_id")
    private UUID warehouseId;

    @Column(name = "client_id")
    private UUID clientId;

    @Column(name = "user_id")
    private UUID userId;

    @Column(name = "registration_date")
    private OffsetDateTime registrationDate;

    @ManyToOne
    @JoinColumn(name = "stock_transaction_type_id", columnDefinition = "stockTransactionTypeId", insertable = false, updatable = false)
    private StockTransactionType stockTransactionType;

    @ManyToOne
    @JoinColumn(name = "warehouse_id", columnDefinition = "warehouseId", insertable = false, updatable = false)
    private Warehouse warehouse;

    @ManyToOne
    @JoinColumn(name = "client_id", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;

    @ManyToOne()
    @JoinColumn(name="user_id",columnDefinition = "userId",insertable = false,updatable = false)
    private User user;
}
