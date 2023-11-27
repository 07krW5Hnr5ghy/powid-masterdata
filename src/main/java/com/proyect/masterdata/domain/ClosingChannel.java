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
@Table(name = Constants.tableClosingChannel, schema = Constants.schemaMaster)
public class ClosingChannel {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id_canal_cierre")
    private Long id;

    @Column(name = "nombre", nullable = false)
    private String name;

    @Column(name = "estado", nullable = false)
    private Boolean status;

    @Column(name = "fecha_registro", nullable = false)
    @CreationTimestamp
    private Date dateRegistration;

    @Column(name = "fecha_modificacion", nullable = false)
    @CreationTimestamp
    private Date dateUpdate;

    @Column(name = "usuario_token", nullable = false)
    private String tokenUser;
}
