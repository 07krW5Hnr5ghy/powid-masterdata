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
@Table(name = Constants.tableKardexInput, schema = Constants.schemaStock)
public class KardexInput {
    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "kardex_input_id")
    private UUID id;

    @Column(name = "registration_date")
    @CreationTimestamp
    private OffsetDateTime registrationDate;

    @Column(name = "supply_order_item_id")
    private UUID supplyOrderItemId;

    @Column(name = "client_id")
    private UUID clientId;

    @Column(name = "user_id")
    private UUID userId;

    @ManyToOne
    @JoinColumn(name = "client_id", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;

    @ManyToOne()
    @JoinColumn(name="user_id",columnDefinition = "userId",insertable = false,updatable = false)
    private User user;

    @ManyToOne
    @JoinColumn(name="supply_order_item_id",columnDefinition = "supplyOrderItemId",insertable = false,updatable = false)
    private SupplyOrderItem supplyOrderItem;
}
