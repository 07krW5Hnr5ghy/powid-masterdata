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
@Table(name = Constants.tableDeliveryZoneDistrict,schema = Constants.schemaLogistics)
public class DeliveryZoneDistrict {
    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "delivery_zone_district_id")
    private UUID id;

    @Column(name = "delivery_zone_id")
    private UUID deliveryZoneId;

    @Column(name = "district_id")
    private UUID districtId;

    @Column(name = "status")
    private Boolean status;

    @Column(name = "registration_date")
    @CreationTimestamp
    private OffsetDateTime registrationDate;

    @Column(name = "update_date")
    @CreationTimestamp
    private OffsetDateTime updateDate;

    @Column(name = "user_id")
    private UUID userId;

    @Column(name = "client_id")
    private UUID clientId;

    @ManyToOne()
    @JoinColumn(name="delivery_zone_id",columnDefinition = "deliveryZoneId",insertable = false,updatable = false)
    private DeliveryZone deliveryZone;

    @ManyToOne()
    @JoinColumn(name="district_id",columnDefinition = "districtId",insertable = false,updatable = false)
    private District district;

    @ManyToOne()
    @JoinColumn(name="user_id",columnDefinition = "userId",insertable = false,updatable = false)
    private User user;

    @ManyToOne()
    @JoinColumn(name="client_id",columnDefinition = "clientId",insertable = false,updatable = false)
    private Client client;
}
