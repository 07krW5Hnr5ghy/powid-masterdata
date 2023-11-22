package com.proyect.masterdata.domain;

import java.util.Date;
import java.util.Set;

import org.hibernate.annotations.CreationTimestamp;

import com.proyect.masterdata.utils.Constants;

import jakarta.persistence.*;
import lombok.Builder;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableAccess, schema = Constants.schemaMaster)
public class Access {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id_acceso", unique = true)
    private Long id;

    @Column(name = "nombre", unique = true, nullable = false)
    private String name;

    @Column(name = "estado", columnDefinition = "BOOLEAN DEFAULT TRUE", nullable = false)
    private Boolean status;

    @Column(name = "fecha_registro", nullable = false)
    @CreationTimestamp
    private Date dateRegistration;

    @Column(name = "fecha_modificacion", nullable = false)
    @CreationTimestamp
    private Date dateUpDate;

    @Column(name = "usuario_token", nullable = false)
    private String tokenUser;
}
