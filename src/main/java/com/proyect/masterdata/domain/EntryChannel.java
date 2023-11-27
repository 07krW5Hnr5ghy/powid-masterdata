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
@Table(name = Constants.tableEntryChannel, schema = Constants.schemaMaster)
public class EntryChannel {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id_canal_entrada")
    private Long id;

    @Column(name = "nombre", nullable = false)
    private String name;

    @Column(name = "estado", nullable = false)
    private Boolean status;

    @Column(name = "fecha_registro", nullable = false)
    private Date dateRegistration;

    @Column(name = "fecha_modificacion", nullable = false)
    private Date dateUpdate;

    @Column(name = "usuario_token", nullable = false)
    private String tokenUser;
}
