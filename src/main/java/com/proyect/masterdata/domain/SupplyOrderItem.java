package com.proyect.masterdata.domain;

import java.time.OffsetDateTime;
import java.util.UUID;

import org.hibernate.annotations.CreationTimestamp;

import com.proyect.masterdata.utils.Constants;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableSupplyOrderItem, schema = Constants.schemaStock)
public class SupplyOrderItem {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "supply_order_item_id")
    private UUID id;

    @Column(name = "quantity")
    private Integer quantity;

    @Column(name = "unit_value")
    private Double unitValue;

    @Column(name = "discounts_amount")
    private Double discountsAmount;

    @Column(name = "charges_amount")
    private Double chargesAmount;

    @Column(name = "unit_sale_price")
    private Double unitSalePrice;

    @Column(name = "observations",columnDefinition = "text")
    private String observations;

    @Column(name = "registration_date")
    @CreationTimestamp
    private OffsetDateTime registrationDate;

    @Column(name = "update_date")
    @CreationTimestamp
    private OffsetDateTime updateDate;

    @Column(name = "status")
    private Boolean status;

    @Column(name = "product_id")
    private UUID productId;

    @Column(name = "purchase_igv_id")
    private UUID purchaseIGVId;

    @Column(name = "client_id")
    private UUID clientId;

    @Column(name = "purchase_id")
    private UUID purchaseId;

    @Column(name = "user_id")
    private UUID userId;

    @ManyToOne
    @JoinColumn(name = "client_id", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;

    @ManyToOne
    @JoinColumn(name = "product_id", columnDefinition = "productId", insertable = false, updatable = false)
    private Product product;

    @ManyToOne
    @JoinColumn(name = "purchase_id",columnDefinition = "purchaseId",insertable = false,updatable = false)
    private SupplyOrder supplyOrder;

    @ManyToOne
    @JoinColumn(name = "purchase_igv_id",columnDefinition = "purchaseIGVId",insertable = false,updatable = false)
    private PurchaseIGV purchaseIGV;

    @ManyToOne()
    @JoinColumn(name="user_id",columnDefinition = "userId",insertable = false,updatable = false)
    private User user;

}
