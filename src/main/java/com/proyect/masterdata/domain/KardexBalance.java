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
@Table(name = Constants.tableKardexBalance, schema = Constants.schemaStock)
public class KardexBalance {
    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "kardex_balance_id")
    private UUID id;

    @Column(name = "remaining_quantity")
    private Integer remainingQuantity;

    @Column(name = "unit_price")
    private Double unitPrice;

    @Column(name = "unit_value")
    private Double unitValue;

    @Column(name = "registration_date")
    @CreationTimestamp
    private OffsetDateTime registrationDate;

    @Column(name = "update_date")
    @CreationTimestamp
    private OffsetDateTime updateDate;

    @Column(name = "lot_number")
    private Long lotNumber;

    @Column(name = "client_id")
    private UUID clientId;

    @Column(name = "user_id")
    private UUID userId;

    @Column(name = "product_id")
    private UUID productId;

    @Column(name = "warehouse_id")
    private UUID warehouseId;

    @ManyToOne()
    @JoinColumn(name = "client_id", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;

    @ManyToOne()
    @JoinColumn(name="user_id",columnDefinition = "userId",insertable = false,updatable = false)
    private User user;

    @ManyToOne()
    @JoinColumn(name="product_id",columnDefinition = "productId",insertable = false,updatable = false)
    private Product product;

    @ManyToOne()
    @JoinColumn(name="warehouse_id",columnDefinition = "warehouseId",insertable = false,updatable = false)
    private Warehouse warehouse;
}
