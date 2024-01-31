package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
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
@Table(name = Constants.tableUnitType,schema = Constants.schemaMaster)
public class UnitType {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id_tipo_unidad")
    private Long id;

    @Column(name = "nombre")
    private String name;

    @Column(name = "estado")
    private Boolean status;

    @Column(name = "fecha_registro")
    private Date registrationDate;

    @Column(name = "fecha_modificacion")
    private Date updateDate;

    @Column(name = "usuario_token")
    private String tokenUser;
}
