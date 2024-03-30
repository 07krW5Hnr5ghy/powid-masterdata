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
@Table(name = Constants.tableStockTransfer,schema = Constants.schemaStock)
public class StockTransfer {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "stock_transfer_id")
    private Long id;

    @Column(name = "serial")
    private String serial;

    @Column(name = "origin_warehouse_id")
    private Long originWarehouseId;

    @Column(name = "destination_warehouse_id")
    private Long destinationWarehouseId;

    @Column(name = "client_id")
    private Long clientId;

    @Column(name = "registration_date")
    @CreationTimestamp
    private Date registrationDate;

    @Column(name = "update_date")
    @CreationTimestamp
    private Date updateDate;

    @Column(name = "token_user")
    private String tokenUser;

    @ManyToOne()
    @JoinColumn(name = "origin_warehouse_id", columnDefinition = "originWarehouseId",insertable = false,updatable = false)
    private Warehouse originWarehouse;

    @ManyToOne()
    @JoinColumn(name = "destination_warehouse_id", columnDefinition = "destinationWarehouseId",insertable = false,updatable = false)
    private Warehouse destinationWarehouse;

    @ManyToOne()
    @JoinColumn(name = "client_id",columnDefinition = "clientId",insertable = false,updatable = false)
    private Client client;
}
