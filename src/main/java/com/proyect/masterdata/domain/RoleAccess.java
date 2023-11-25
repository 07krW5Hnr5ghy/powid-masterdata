package com.proyect.masterdata.domain;

import java.util.Date;

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
@Table(name = Constants.tableRoleAccess, schema = Constants.schemaMaster)
public class RoleAccess {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id_rol_acceso", unique = true)
    private Long id;

    @Column(name = "id_rol", nullable = false)
    private Long roleId;

    @Column(name = "id_acceso", nullable = false)
    private Long accessId;

    @Column(name = "usuario_token", nullable = false)
    private String tokenUser;

    @Column(name = "fecha_registro", nullable = false)
    private Date dateRegistration;
}
