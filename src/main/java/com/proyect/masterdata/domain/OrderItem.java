package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.DynamicUpdate;

import java.time.OffsetDateTime;
import java.util.UUID;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@DynamicUpdate
@Table(name = Constants.tableOrderItem, schema = Constants.schemaOrder)
public class OrderItem {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "order_item_id")
    private UUID id;

    @Column(name = "quantity")
    private Integer quantity;

    @Column(name = "discount_amount")
    private Double discountAmount;

    @Column(name = "observations",columnDefinition = "text")
    private String observations;

    @Column(name = "product_id")
    private UUID productId;

    @Column(name = "order_id")
    private UUID orderId;

    @Column(name = "client_id")
    private UUID clientId;

    @Column(name = "discount_id")
    private UUID discountId;

    @Column(name = "user_id")
    private UUID userId;

    @Column(name = "status")
    private Boolean status;

    @Column(name = "registration_date")
    @CreationTimestamp
    private OffsetDateTime registrationDate;

    @Column(name = "update_date")
    private OffsetDateTime updateDate;

    @Column(name = "select_order_status")
    private Boolean selectOrderStatus;

    @ManyToOne
    @JoinColumn(name = "product_id", columnDefinition = "productId", insertable = false,updatable = false)
    private Product product;

    @ManyToOne
    @JoinColumn(name = "order_id", columnDefinition = "orderId", insertable = false, updatable = false)
    private Ordering ordering;

    @ManyToOne
    @JoinColumn(name = "client_id", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;

    @ManyToOne
    @JoinColumn(name = "discount_id",columnDefinition = "discountId",insertable = false,updatable = false)
    private Discount discount;

    @ManyToOne()
    @JoinColumn(name="user_id",columnDefinition = "userId",insertable = false,updatable = false)
    private User user;

}
