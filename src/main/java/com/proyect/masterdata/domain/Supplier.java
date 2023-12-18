package com.proyect.masterdata.domain;

import java.util.Date;

import org.hibernate.annotations.CreationTimestamp;

import com.proyect.masterdata.utils.Constants;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
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
@Table(name = Constants.tableSupplier, schema = Constants.schemaInventory)
public class Supplier {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id_proveedor")
    private Long id;

    @Column(name = "nombre")
    private String name;

    @Column(name = "ruc")
    private String ruc;

    @Column(name = "pais")
    private String country;

    @Column(name = "ubicacion")
    private String location;

    @Column(name = "nombre_corporativo")
    private String coporateName;

    @Column(name = "telefono")
    private String phoneNumber;

    @Column(name = "email")
    private String email;

    @Column(name = "fecha_registro")
    @CreationTimestamp
    private Date registrationDate;

    @Column(name = "fecha_modificacion")
    @CreationTimestamp
    private Date updateDate;

    @Column(name = "usuario_token")
    private String tokenUser;

    @Column(name = "id_cliente")
    private String clientId;

    @ManyToOne
    @JoinColumn(name = "id_cliente", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;

}