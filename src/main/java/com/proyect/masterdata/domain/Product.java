package com.proyect.masterdata.domain;

import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;

import com.proyect.masterdata.utils.Constants;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
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
    @GeneratedValue(generator = "sequence-product")
    @GenericGenerator(name = "sequence-product", strategy = "org.hibernate.id.enhanced.SequenceStyleGenerator", parameters = {
            @Parameter(name = "sequence_name", value = "producto_sequence"),
            @Parameter(name = "initial_value", value = "1"),
            @Parameter(name = "increment_size", value = "1")
    })
    @Column(name = "id_producto", unique = true)
    private Long id;

    @Column(name = "sku")
    private String sku;

    @Column(name = "fecha_registro")
    @CreationTimestamp
    private Date dateRegistration;

    @Column(name = "fecha_modificacion")
    @CreationTimestamp
    private Date dateUpdate;

    @Column(name = "estado")
    private boolean status;

    @Column(name = "id_modelo")
    private Long idModel;

    @Column(name = "id_color")
    private Long idColor;

    @Column(name = "id_categoria")
    private Long idCategory;

    @Column(name = "id_size")
    private Long idSize;

    @Column(name = "usuario")
    private String user;

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