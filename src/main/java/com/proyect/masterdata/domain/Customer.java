package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableCustomer,schema = Constants.schemaOrder)
public class Customer {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id_comprador")
    private Long id;

    @Column(name = "nombre")
    private String name;

    @Column(name = "tipo")
    private String type;

    @Column(name = "instagram")
    private String instagram;

    @Column(name = "telefono")
    private String phone;

    @Column(name = "direccion")
    private String address;

    @Column(name = "referencia")
    private String reference;

    @Column(name = "id_orden")
    private Long orderId;

    @Column(name = "id_distrito")
    private Long districtId;

    @Column(name = "id_provincia")
    private Long provinceId;

    @Column(name = "id_departamento")
    private Long departmentId;

    @Column(name = "id_cliente")
    private Long clientId;

    @Column(name = "usuario_token")
    private String tokenUser;

    @ManyToOne
    @JoinColumn(name = "id_orden", columnDefinition = "orderId", insertable = false, updatable = false)
    private Order order;

    @ManyToOne
    @JoinColumn(name = "id_distrito", columnDefinition = "districtId", insertable = false, updatable = false)
    private District district;

    @ManyToOne
    @JoinColumn(name = "id_provincia", columnDefinition = "provinceId", insertable = false, updatable = false)
    private Province province;

    @ManyToOne
    @JoinColumn(name = "id_cliente", columnDefinition = "clientId", insertable = false, updatable = false)
    private Client client;

}
