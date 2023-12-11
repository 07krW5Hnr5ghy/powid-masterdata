package com.proyect.masterdata.domain;

import java.util.Date;

import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;

import com.proyect.masterdata.utils.Constants;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
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
@Table(name = Constants.tableBrand, schema = Constants.schemaArticle)
public class Brand {
    @Id
    @GeneratedValue(generator = "sequence-brand")
    @GenericGenerator(name = "sequence-brand", strategy = "org.hibernate.id.enhanced.SequenceStyleGenerator", parameters = {
            @Parameter(name = "sequence_name", value = "marca_sequence"),
            @Parameter(name = "initial_value", value = "1"),
            @Parameter(name = "increment_size", value = "1")
    })
    @Column(name = "id_marca")
    private Long id;

    @Column(name = "nombre")
    private String name;

    @Column(name = "estado")
    private boolean status;

    @Column(name = "fecha_registro")
    @CreationTimestamp
    private Date dateRegistration;

    @Column(name = "fecha_modificacion")
    @CreationTimestamp
    private Date dateUpdate;

    @Column(name = "usuario_token")
    private String tokenUser;
}
