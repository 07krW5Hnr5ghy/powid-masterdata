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

    @Column(name = "estado")
    private Long status;

    @Column(name = "fecha_registro")
    @CreationTimestamp
    private Date dateRegistration;

    @ManyToOne
    @JoinColumn(name = "usuario", columnDefinition = "user",insertable = false, updatable = false, foreignKey = @ForeignKey(name = "fk_sesion"))
    private Login login;
}
