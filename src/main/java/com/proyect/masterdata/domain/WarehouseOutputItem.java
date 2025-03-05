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
@Table(name = Constants.tableWarehouseOutputItem, schema = Constants.schemaStock)
public class WarehouseOutputItem {
    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "warehouse_output_item_id")
    private UUID id;

    @Column(name="quantity")
    private Integer quantity;

    @Column(name="product_id")
    private UUID productId;

    @Column(name="warehouse_output_id")
    private UUID warehouseOutputId;

    @Column(name = "registration_date")
    private OffsetDateTime registrationDate;

    @Column(name = "update_date")
    private OffsetDateTime updateDate;

    @Column(name = "user_id")
    private UUID userId;

    @Column(name = "client_id")
    private UUID clientId;

    @Column(name = "status")
    private Boolean status;

    @ManyToOne
    @JoinColumn(name = "client_id",columnDefinition = "clientId",insertable = false,updatable = false)
    private Client client;

    @ManyToOne()
    @JoinColumn(name="user_id",columnDefinition = "userId",insertable = false,updatable = false)
    private User user;

    @ManyToOne
    @JoinColumn(name="product_id",columnDefinition = "productId",insertable = false,updatable = false)
    private Product product;

    @ManyToOne
    @JoinColumn(name="warehouse_output_id",columnDefinition = "warehouseOutputId",insertable = false,updatable = false)
    private WarehouseOutput warehouseOutput;
}
