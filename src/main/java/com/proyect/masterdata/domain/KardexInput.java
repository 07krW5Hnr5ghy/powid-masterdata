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

    @Column(name = "lot_number")
    private Long lotNumber;

    @Column(name = "kardex_operation_type_id")
    private UUID kardexOperationTypeId;

    @Column(name = "product_id")
    private UUID productId;

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

    @ManyToOne()
    @JoinColumn(name="kardex_operation_type_id",columnDefinition = "kardexOperationTypeId",insertable = false,updatable = false)
    private KardexOperationType kardexOperationType;

    @ManyToOne()
    @JoinColumn(name="product_id",columnDefinition = "productId",insertable = false,updatable = false)
    private Product product;
}
