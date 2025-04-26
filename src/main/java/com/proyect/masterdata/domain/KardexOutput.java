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
@Table(name = Constants.tableKardexOutput, schema = Constants.schemaStock)
public class KardexOutput {
    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "kardex_output_id")
    private UUID id;

    @Column(name = "quantity")
    private Integer quantity;

    @Column(name = "unit_price")
    private Double unitPrice;

    @Column(name = "unit_value")
    private Double unitValue;

    @Column(name = "order_number")
    private Long orderNumber;

    @Column(name = "lotNumber")
    private Long lotNumber;

    @Column(name = "registration_date")
    @CreationTimestamp
    private OffsetDateTime registrationDate;

    @Column(name = "kardex_operation_type_id")
    private UUID kardexOperationTypeId;

    @Column(name = "client_id")
    private UUID clientId;

    @Column(name = "user_id")
    private UUID userId;

    @Column(name = "product_id")
    private UUID productId;

    @Column(name = "warehouse_id")
    private UUID warehouseId;

    @Column(name = "delivery_manifest_item_id")
    private UUID deliveryManifestItemId;

    @ManyToOne
    @JoinColumn(name = "client_id", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;

    @ManyToOne()
    @JoinColumn(name="user_id",columnDefinition = "userId",insertable = false,updatable = false)
    private User user;

    @ManyToOne()
    @JoinColumn(name="kardex_operation_type_id",columnDefinition = "kardexOperationTypeId",insertable = false,updatable = false)
    private KardexOperationType kardexOperationType;

    @ManyToOne()
    @JoinColumn(name="product_id",columnDefinition = "productId",insertable = false,updatable = false)
    private Product product;

    @ManyToOne()
    @JoinColumn(name="warehouse_id",columnDefinition = "warehouseId",insertable = false,updatable = false)
    private Warehouse warehouse;

    @ManyToOne()
    @JoinColumn(name="delivery_manifest_item_id",columnDefinition = "deliveryManifestItemId",insertable = false,updatable = false)
    private DeliveryManifestItem deliveryManifestItem;
}
