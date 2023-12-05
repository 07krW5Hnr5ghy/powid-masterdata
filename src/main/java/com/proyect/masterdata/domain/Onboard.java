package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.OneToOne;
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
@Table(name = Constants.tableOnboarding, schema = Constants.schemaManagement)
public class Onboard {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id_onboard")
    private Long id;

    @Column(name = "ecommerce", nullable = false)
    private Boolean ecommerce;

    @Column(name = "usuarios_minimo", nullable = false)
    private Integer usersMinimum;

    @Column(name = "usuarios_maximo", nullable = false)
    private Integer usersMaximum;

    @Column(name = "facturacion", nullable = false)
    private Boolean billing;

    @Column(name = "comentario", nullable = false)
    private String comment;

    @Column(name = "demo", nullable = false)
    private Boolean demo;

    @Column(name = "id_cliente", nullable = false)
    private Long idClient;

    @Column(name = "id_canal_entrada", nullable = false)
    private Long idEntryChannel;

    @Column(name = "id_categoria", nullable = false)
    private Long idCategory;

    @OneToOne
    @JoinColumn(name = "id_cliente", columnDefinition = "idClient", insertable = false, updatable = false)
    private Client client;

    @ManyToOne
    @JoinColumn(name = "id_canal_entrada", columnDefinition = "idEntryChannel", insertable = false, updatable = false)
    private EntryChannel entryChannel;

    @ManyToOne
    @JoinColumn(name = "id_categoria", columnDefinition = "idCategory", insertable = false, updatable = false)
    private Category category;

}
