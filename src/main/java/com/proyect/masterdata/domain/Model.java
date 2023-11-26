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
@Table(name = Constants.tableModel, schema = Constants.schemaArticle)
public class Model {
    @Id
    @GeneratedValue(generator = "sequence-model")
    @GenericGenerator(name = "sequence-model", strategy = "org.hibernate.id.enhanced.SequenceStyleGenerator", parameters = {
            @Parameter(name = "sequence_name", value = "modelo_sequence"),
            @Parameter(name = "initial_value", value = "1"),
            @Parameter(name = "increment_size", value = "1")
    })
    @Column(name = "id_model", unique = true)
    private Long id;

    @Column(name = "nombre", unique = true)
    private String name;

    @Column(name = "estado")
    private boolean status;

    @Column(name = "fecha_registro")
    @CreationTimestamp
    private Date dateRegistration;

    @Column(name = "fecha_modificacion")
    @CreationTimestamp
    private Date dateUpdate;

    @Column(name = "id_marca")
    private Long idBrand;

    @ManyToOne
    @JoinColumn(name = "id_marca", columnDefinition = "idBrand", insertable = false, updatable = false)
    private Brand brand;

    @Column(name = "usuario")
    private String user;
}