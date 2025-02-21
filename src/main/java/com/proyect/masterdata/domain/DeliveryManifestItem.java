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

    @Column(name = "delivered_quantity")
    private Integer deliveredQuantity;

    @Column(name = "delivery_manifest_id")
    private UUID deliveryManifestId;

    @Column(name = "order_item_id")
    private UUID orderItemId;

    @Column(name = "user_id")
    private UUID userId;

    @ManyToOne()
    @JoinColumn(name = "delivery_manifest_id",columnDefinition = "deliveryManifestId",updatable = false,insertable = false)
    private DeliveryManifest deliveryManifest;

    @ManyToOne()
    @JoinColumn(name = "order_item_id",columnDefinition = "orderItemId",insertable = false,updatable = false)
    private OrderItem orderItem;

    @ManyToOne()
    @JoinColumn(name="user_id",columnDefinition = "userId",insertable = false,updatable = false)
    private User user;

}
