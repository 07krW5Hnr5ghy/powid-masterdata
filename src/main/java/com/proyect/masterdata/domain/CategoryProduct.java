package com.proyect.masterdata.domain;

import java.time.OffsetDateTime;
import java.util.UUID;

import jakarta.persistence.*;
import org.hibernate.annotations.CreationTimestamp;

import com.proyect.masterdata.utils.Constants;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableCategoryProduct, schema = Constants.schemaMaster)
public class CategoryProduct {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "category_product_id", unique = true)
    private UUID id;

    @Column(name = "name")
    private String name;

    @Column(name = "sku")
    private String sku;

    @Column(name = "status")
    private Boolean status;

    @Column(name = "registration_date")
    @CreationTimestamp
    private OffsetDateTime registrationDate;

    @Column(name = "update_date")
    @CreationTimestamp
    private OffsetDateTime updateDate;

    @Column(name = "size_type_id")
    private UUID sizeTypeId;

    @Column(name = "user_id")
    private UUID userId;

    @Column(name = "client_id")
    private UUID clientId;

    @Column(name = "unit_type_id")
    private UUID unitTypeId;

    @ManyToOne
    @JoinColumn(name = "unit_type_id", columnDefinition = "unitTypeId", insertable = false, updatable = false)
    private UnitType unitType;

    @ManyToOne
    @JoinColumn(name = "size_type_id", columnDefinition = "sizeTypeId", insertable = false, updatable = false)
    private SizeType sizeType;

    @ManyToOne()
    @JoinColumn(name="user_id",columnDefinition = "userId",insertable = false,updatable = false)
    private User user;

    @ManyToOne()
    @JoinColumn(name="client_id",columnDefinition = "clientId",insertable = false,updatable = false)
    private Client client;
}
