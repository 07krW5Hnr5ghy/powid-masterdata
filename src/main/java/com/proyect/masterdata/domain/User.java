package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;

import java.util.Date;
import java.util.Set;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableUser, schema = Constants.schemaMaster)
public class User {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id_usuario", unique = true)
    private Long id;

    @Column(name = "usuario", unique = true, nullable = false)
    private String username;

    @Column(name = "nombre", nullable = false)
    private String name;

    @Column(name = "apellido", nullable = false)
    private String surname;

    @Column(name = "dni", unique = true, nullable = false)
    private String dni;

    @Column(name = "correo", unique = true, nullable = false)
    private String email;

    @Column(name = "direccion", unique = true, nullable = false)
    private String address;

    @Column(name = "genero", nullable = false)
    private String gender;

    @Column(name = "celular", nullable = false)
    private String mobile;

    @Column(name = "password", nullable = false)
    private String password;

    @Column(name = "estado", nullable = false)
    private Boolean status;

    @Column(name = "fecha_registro", nullable = false)
    @CreationTimestamp
    private Date dateRegistration;

    @Column(name = "fecha_modificacion", nullable = false)
    @CreationTimestamp
    private Date dateUpdate;

    @Column(name = "id_distrito", updatable = false, nullable = false)
    private Long idDistrict;

    @ManyToMany(fetch = FetchType.EAGER)
    @JoinTable(name = "usuario_rol", joinColumns = { @JoinColumn(name = "id_usuario") }, inverseJoinColumns = {
            @JoinColumn(name = "id_rol") })
    private Set<Role> roles;

    @ManyToOne
    @JoinColumn(name = "id_distrito", columnDefinition = "idDistrict", insertable = false, updatable = false)
    private District district;

    @Column(name = "usuario_token", nullable = false)
    private String tokenUser;

}
