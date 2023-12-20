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
@Table(name = Constants.tableWarehouse, schema = Constants.schemaInventory)
public class Warehouse {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id_almacen")
    private Long id;

    @Column(name = "nombre")
    private String name;

    @Column(name = "ubicacion")
    private String location;

    @Column(name = "fecha_registro")
    private Date registrationDate;

    @Column(name = "fecha_update")
    private Date updateDate;

    @Column(name = "estado")
    private Boolean status;

}
