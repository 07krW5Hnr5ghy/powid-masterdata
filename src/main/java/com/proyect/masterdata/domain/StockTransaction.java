package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

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
    private String id;

    @Column(name = "serial")
    private String serial;

    @Column(name = "stock_transaction_type_id")
    private Long stockTransactionTypeId;

    @Column(name = "warehouse_id")
    private Long warehouseId;

    @Column(name = "client_id")
    private Long clientId;

    @Column(name = "token_user")
    private String tokenUser;

    @Column(name = "registration_date")
    private Date registrationDate;

    @ManyToOne
    @JoinColumn(name = "stock_transaction_type_id", columnDefinition = "stockTransactionTypeId", insertable = false, updatable = false)
    private StockTransactionType stockTransactionType;

    @ManyToOne
    @JoinColumn(name = "warehouse_id", columnDefinition = "warehouseId", insertable = false, updatable = false)
    private Warehouse warehouse;

    @ManyToOne
    @JoinColumn(name = "client_id", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;
}
