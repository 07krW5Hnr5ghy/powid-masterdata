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
@Table(name = Constants.tableOrderReturnItem, schema = Constants.schemaOrder)
public class OrderReturnItem {
    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "order_return_item_id")
    private UUID id;

    @Column(name = "order_return_id")
    private UUID orderReturnId;

    @Column(name = "product_id")
    private UUID productId;

    @Column(name = "order_return_type_id")
    private UUID orderReturnTypeId;

    @Column(name = "supplier_product_id")
    private UUID supplierProductId;

    @Column(name = "quantity")
    private Integer quantity;

    @Column(name = "registration_date")
    @CreationTimestamp()
    private OffsetDateTime registrationDate;

    @Column(name = "update_date")
    @CreationTimestamp()
    private OffsetDateTime updateDate;

    @Column(name = "status")
    private Boolean status;

    @Column(name = "user_id")
    private UUID userId;

    @Column(name = "client_id")
    private UUID clientId;

    @ManyToOne()
    @JoinColumn(name = "order_return_id",columnDefinition = "orderReturnId",insertable = false,updatable = false)
    private OrderReturn orderReturn;

    @ManyToOne()
    @JoinColumn(name = "product_id",columnDefinition = "productId",insertable = false,updatable = false)
    private Product product;

    @ManyToOne()
    @JoinColumn(name = "order_return_type_id",columnDefinition = "orderReturnTypeId",insertable = false,updatable = false)
    private OrderReturnType orderReturnType;

    @ManyToOne()
    @JoinColumn(name = "supplier_product_id",columnDefinition = "supplierProductId",insertable = false,updatable = false)
    private SupplierProduct supplierProduct;

    @ManyToOne()
    @JoinColumn(name = "client_id", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;

    @ManyToOne()
    @Column(name="user_id",columnDefinition = "userId",insertable = false,updatable = false)
    private User user;
}
