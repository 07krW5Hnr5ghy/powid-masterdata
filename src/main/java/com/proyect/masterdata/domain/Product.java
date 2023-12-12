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
@Table(name = Constants.tableProduct, schema = Constants.schemaArticle)
public class Product {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id_producto")
    private Long id;

    @Column(name = "sku", nullable = false)
    private String sku;

    @Column(name = "fecha_registro")
    @CreationTimestamp
    private Date registrationDate;

    @Column(name = "fecha_modificacion")
    @CreationTimestamp
    private Date updateDate;

    @Column(name = "estado", nullable = false)
    private boolean status;

    @Column(name = "id_modelo", nullable = false)
    private Long modelId;

    @Column(name = "id_color", nullable = false)
    private Long colorId;

    @Column(name = "id_categoria", nullable = false)
    private Long categoryId;

    @Column(name = "id_size", nullable = false)
    private Long sizeId;

    @Column(name = "usuario", nullable = false)
    private String tokenUser;

    @ManyToOne
    @JoinColumn(name = "id_modelo", columnDefinition = "idModel", insertable = false, updatable = false)
    private Model model;

    @ManyToOne
    @JoinColumn(name = "id_color", columnDefinition = "idColor", insertable = false, updatable = false)
    private Color color;

    @ManyToOne
    @JoinColumn(name = "id_categoria", columnDefinition = "idCategory", insertable = false, updatable = false)
    private Category category;

    @ManyToOne
    @JoinColumn(name = "id_size", columnDefinition = "idSize", insertable = false, updatable = false)
    private Size size;
}