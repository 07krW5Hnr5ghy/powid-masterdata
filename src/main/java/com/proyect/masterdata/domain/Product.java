package com.proyect.masterdata.domain;

import jakarta.persistence.*;
import org.hibernate.annotations.CreationTimestamp;

import com.proyect.masterdata.utils.Constants;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.time.OffsetDateTime;

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
    private String id;

    @Column(name = "sku", nullable = false)
    private String sku;

    @Column(name = "characteristics", nullable = false, columnDefinition = "text")
    private String characteristics;

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
    private Long modelId;

    @Column(name = "color_id", nullable = false)
    private Long colorId;

    @Column(name = "category_product_id", nullable = false)
    private Long categoryProductId;

    @Column(name = "size_id", nullable = false)
    private Long sizeId;

    @Column(name = "unit_id")
    private Long unitId;

    @Column(name = "client_id")
    private Long clientId;

    @Column(name = "user_id")
    private String userId;

    @ManyToOne
    @JoinColumn(name = "model_id", columnDefinition = "modelId", insertable = false, updatable = false)
    private Model model;

    @ManyToOne
    @JoinColumn(name = "color_id", columnDefinition = "colorId", insertable = false, updatable = false)
    private Color color;

    @ManyToOne
    @JoinColumn(name = "category_product_id", columnDefinition = "categoryId", insertable = false, updatable = false)
    private CategoryProduct categoryProduct;

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
    @Column(name="user_id",columnDefinition = "userId",insertable = false,updatable = false)
    private User user;
}