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
@Table(name = Constants.tableStockTransfer,schema = Constants.schemaStock)
public class StockTransfer {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "stock_transfer_id")
    private UUID id;

    @Column(name = "serial")
    private String serial;

    @Column(name = "origin_warehouse_id")
    private UUID originWarehouseId;

    @Column(name = "destination_warehouse_id")
    private UUID destinationWarehouseId;

    @Column(name = "client_id")
    private UUID clientId;

    @Column(name = "registration_date")
    @CreationTimestamp
    private OffsetDateTime registrationDate;

    @Column(name = "update_date")
    @CreationTimestamp
    private OffsetDateTime updateDate;

    @Column(name = "user_id")
    private UUID userId;

    @ManyToOne()
    @JoinColumn(name = "origin_warehouse_id", columnDefinition = "originWarehouseId",insertable = false,updatable = false)
    private Warehouse originWarehouse;

    @ManyToOne()
    @JoinColumn(name = "destination_warehouse_id", columnDefinition = "destinationWarehouseId",insertable = false,updatable = false)
    private Warehouse destinationWarehouse;

    @ManyToOne()
    @JoinColumn(name = "client_id",columnDefinition = "clientId",insertable = false,updatable = false)
    private Client client;

    @ManyToOne()
    @Column(name="user_id",columnDefinition = "userId",insertable = false,updatable = false)
    private User user;
}
