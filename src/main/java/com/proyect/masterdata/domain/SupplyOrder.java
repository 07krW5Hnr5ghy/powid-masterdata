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
@Table(name = Constants.tableSupplyOrder, schema = Constants.schemaStock)
public class SupplyOrder {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "supply_order_id")
    private UUID id;

    @Column(name = "ref")
    private String ref;

    @Column(name = "order_number")
    private Long orderNumber;

    @Column(name = "registration_date")
    @CreationTimestamp
    private OffsetDateTime registrationDate;

    @Column(name = "delivery_date")
    @CreationTimestamp
    private OffsetDateTime deliveryDate;

    @Column(name = "update_date")
    @CreationTimestamp
    private OffsetDateTime updateDate;

    @Column(name = "status")
    private Boolean status;

    @Column(name = "client_id")
    private UUID clientId;

    @Column(name = "warehouse_id")
    private UUID warehouseId;

    @Column(name = "user_id")
    private UUID userId;

    @ManyToOne
    @JoinColumn(name = "client_id", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;

    @ManyToOne
    @JoinColumn(name = "warehouse_id", columnDefinition = "warehouseId", insertable = false, updatable = false)
    private Warehouse warehouse;

    @ManyToOne()
    @JoinColumn(name="user_id",columnDefinition = "userId",insertable = false,updatable = false)
    private User user;

}
