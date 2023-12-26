package com.proyect.masterdata.domain;

import java.util.Date;

import org.hibernate.annotations.CreationTimestamp;

import com.proyect.masterdata.utils.Constants;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
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
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id_categoria_producto", unique = true)
    private Long id;

    @Column(name = "nombre", length = 50)
    private String name;

    @Column(name = "descripcion", length = 50)
    private String description;

    @Column(name = "estado", columnDefinition = "BOOLEAN DEFAULT TRUE")
    private Boolean status;

    @Column(name = "fecha_registro")
    @CreationTimestamp
    private Date registrationDate;

    @Column(name = "fecha_modificacion")
    @CreationTimestamp
    private Date updateDate;

    @Column(name = "usuario_token")
    private String tokenUser;
}
