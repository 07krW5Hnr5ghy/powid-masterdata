package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;

import java.util.Date;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableUser, schema = Constants.schemaManagement)
public class User {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    @Column(name = "id_usuario")
    private Long id;

    @Column(name = "usuario", nullable = false)
    private String username;

    @Column(name = "nombre", nullable = false)
    private String name;

    @Column(name = "apellido", nullable = false)
    private String surname;

    @Column(name = "dni", nullable = false)
    private String dni;

    @Column(name = "correo", nullable = false)
    private String email;

    @Column(name = "direccion", nullable = false)
    private String address;

    @Column(name = "genero", nullable = false)
    private String gender;

    @Column(name = "celular", nullable = false)
    private String mobile;

    @Column(name = "password", nullable = false)
    private String password;

    @Column(name = "estado", nullable = false)
    private Boolean status;

    @Column(name = "fecha_registro")
    @CreationTimestamp
    private Date dateRegistration;

    @Column(name = "fecha_modificacion")
    @CreationTimestamp
    private Date dateUpdate;

    @Column(name = "id_distrito", updatable = false, nullable = false)
    private Long idDistrict;

    @Column(name = "usuario_token", nullable = false)
    private String tokenUser;

    @ManyToOne
    @JoinColumn(name = "id_distrito", columnDefinition = "idDistrict", insertable = false, updatable = false)
    private District district;

}
