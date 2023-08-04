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
@Table(name = Constants.tableLogin, schema = Constants.schemaMaster)
public class Login {
    @Id
    @Column(name = "usuario", unique = true)
    private String user;

    @Column(name = "password")
    private String password;

    @Column(name = "estado")
    private Long status;

    @Column(name = "fecha_registro")
    @CreationTimestamp
    private Date dateRegistration;

    @Column(name = "id_tipo_usuario")
    private Long idUserType;

    @ManyToOne
    @JoinColumn(name = "id_tipo_usuario", columnDefinition = "idUserType",insertable = false, updatable = false, foreignKey = @ForeignKey(name = "fk_tipo_usuario"))
    private UserType userType;
}
