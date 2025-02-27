package com.proyect.masterdata.domain;

import jakarta.persistence.*;
import org.hibernate.annotations.CreationTimestamp;

import com.proyect.masterdata.utils.Constants;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.time.OffsetDateTime;
import java.util.UUID;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableProduct, schema = Constants.schemaMarketing)
public class Product {

    @Id
    @GeneratedValue(strategy = GenerationType.UUID)
    @Column(name = "product_id")
    private UUID id;

    @Column(name = "name")
    private String name;

    @Column(name = "registration_date")
    @CreationTimestamp
    private OffsetDateTime registrationDate;

    @Column(name = "update_date")
    @CreationTimestamp
    private OffsetDateTime updateDate;

    @Column(name = "status", nullable = false)
    private Boolean status;

    @Column(name = "picture_flag")
    private Boolean pictureFlag;

    @Column(name = "model_id", nullable = false)
    private UUID modelId;

    @Column(name = "color_id", nullable = false)
    private UUID colorId;

    @Column(name = "sub_category_product_id", nullable = false)
    private UUID subCategoryProductId;

    @Column(name = "size_id", nullable = false)
    private UUID sizeId;

    @Column(name = "unit_id")
    private UUID unitId;

    @Column(name = "client_id")
    private UUID clientId;

    @Column(name = "user_id")
    private UUID userId;

    @ManyToOne
    @JoinColumn(name = "model_id", columnDefinition = "modelId", insertable = false, updatable = false)
    private Model model;

    @ManyToOne
    @JoinColumn(name = "color_id", columnDefinition = "colorId", insertable = false, updatable = false)
    private Color color;

    @ManyToOne
    @JoinColumn(name = "sub_category_product_id", columnDefinition = "subCategoryProductId", insertable = false, updatable = false)
    private SubCategoryProduct subCategoryProduct;

    @ManyToOne
    @JoinColumn(name = "size_id", columnDefinition = "sizeId", insertable = false, updatable = false)
    private Size size;

    @ManyToOne
    @JoinColumn(name = "unit_id",columnDefinition = "unitId",insertable = false,updatable = false)
    private Unit unit;

    @ManyToOne
    @JoinColumn(name = "client_id", columnDefinition = "clientId",insertable = false,updatable = false)
    private Client client;

    @ManyToOne()
    @JoinColumn(name="user_id",columnDefinition = "userId",insertable = false,updatable = false)
    private User user;
}