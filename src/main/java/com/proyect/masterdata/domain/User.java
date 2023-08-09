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
@Table(name = Constants.tableUser, schema = Constants.schemaMaster)
public class User {
    @Id
    @Column(name = "usuario", unique = true)
    private String user;

    @Column(name = "nombre")
    private String name;

    @Column(name = "apellido")
    private String surname;

    @Column(name = "dni")
    private String dni;

    @Column(name = "correo")
    private String email;

    @Column(name = "direccion")
    private String address;

    @Column(name = "genero")
    private String gender;

    @Column(name = "celular")
    private String mobile;

    @Column(name = "password")
    private String password;

    @Column(name = "estado")
    private Long status;

    @Column(name = "fecha_registro")
    @CreationTimestamp
    private Date dateRegistration;

    @Column(name = "id_distrito", unique = true)
    private Long id_district;

    @Column(name = "id_tipo_usuario")
    private Long idUserType;

    @ManyToOne
    @JoinColumn(name = "id_distrito", columnDefinition = "id_district",insertable = false, updatable = false, foreignKey = @ForeignKey(name = "fk_distrito"))
    private District district;

    @ManyToOne
    @JoinColumn(name = "id_tipo_usuario", columnDefinition = "idUserType",insertable = false, updatable = false, foreignKey = @ForeignKey(name = "fk_tipo_usuario"))
    private UserType userType;
}
