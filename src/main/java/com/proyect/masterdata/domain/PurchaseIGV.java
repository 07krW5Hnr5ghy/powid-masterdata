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
@Table(name = Constants.tablePurchaseDiscount, schema = Constants.schemaStock)
public class PurchaseIGV {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "purchase_discount_id")
    private UUID id;

    @Column(name = "name")
    private String name;

    @Column(name = "value")
    private Double value;

    @Column(name = "percentage")
    private Boolean percentage;

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

    @ManyToOne()
    @JoinColumn(name="user_id",columnDefinition = "userId",insertable = false,updatable = false)
    private User user;

    @Column(name = "client_id")
    private UUID clientId;

    @ManyToOne()
    @JoinColumn(name="client_id",columnDefinition = "clientId",insertable = false,updatable = false)
    private Client client;
}
