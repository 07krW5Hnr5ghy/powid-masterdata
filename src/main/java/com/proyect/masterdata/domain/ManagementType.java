package com.proyect.masterdata.domain;

import java.util.Date;

import lombok.Builder;
import org.hibernate.annotations.CreationTimestamp;

import com.proyect.masterdata.utils.Constants;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableManagementType, schema = Constants.schemaMaster)
public class ManagementType {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id_tipo_gestion")
    private Long id;

    @Column(name = "nombre")
    private String name;

    @Column(name = "estado")
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
