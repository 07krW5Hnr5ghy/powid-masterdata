package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.UUID;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableDeliveryManifestItems,schema = Constants.schemaLogistics)
public class DeliveryManifestItem {
    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "delivery_manifest_item_id")
    private UUID id;

    @Column(name = "quantity")
    private Integer quantity;

    @Column(name = "collected")
    private Boolean collected;

    @Column(name = "delivery_manifest_id")
    private UUID deliveryManifestId;

    @Column(name = "order_item_id")
    private UUID orderItemId;

    @Column(name = "product_id")
    private UUID productId;

    @Column(name = "user_id")
    private UUID userId;

    @Column(name = "delivery_status_id")
    private UUID deliveryStatusId;

    @ManyToOne()
    @JoinColumn(name = "delivery_manifest_id",columnDefinition = "deliveryManifestId",updatable = false,insertable = false)
    private DeliveryManifest deliveryManifest;

    @ManyToOne()
    @JoinColumn(name = "order_item_id",columnDefinition = "orderItemId",insertable = false,updatable = false)
    private OrderItem orderItem;

    @ManyToOne()
    @JoinColumn(name = "product_id",columnDefinition = "productId",insertable = false,updatable = false)
    private Product product;

    @ManyToOne()
    @JoinColumn(name="user_id",columnDefinition = "userId",insertable = false,updatable = false)
    private User user;

    @ManyToOne()
    @JoinColumn(name="delivery_status_id",columnDefinition = "deliveryStatusId",insertable = false,updatable = false)
    private DeliveryStatus deliveryStatus;

}
