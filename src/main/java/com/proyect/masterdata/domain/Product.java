package com.proyect.masterdata.domain;

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
import java.util.Date;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableProduct, schema = Constants.schemaMarketing)
public class Product {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "product_id")
    private Long id;

    @Column(name = "sku", nullable = false)
    private String sku;

    @Column(name = "registration_date")
    @CreationTimestamp
    private Date registrationDate;

    @Column(name = "update_date")
    @CreationTimestamp
    private Date updateDate;

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

    @Column(name = "token_user", nullable = false)
    private String tokenUser;

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
}