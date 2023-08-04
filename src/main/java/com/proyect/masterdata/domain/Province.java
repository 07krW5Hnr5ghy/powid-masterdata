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
@IdClass(ProvincePK.class)
@Table(name = Constants.tableProvince, schema = Constants.schemaMaster)
public class Province {
    @Id
    private Long id;

    @Id
    private String user;

    @Column(name="nombre", length=50, unique=true)
    private String name;

    @Column(name = "estado")
    private Boolean status;

    @Column(name = "fecha_registro")
    @CreationTimestamp
    private Date dateRegistration;

    @Column(name = "id_departameto")
    private Long idDepartment;

    @ManyToOne
    @JoinColumn(name = "id_departameto", columnDefinition = "idDepartment",insertable = false, updatable = false, foreignKey = @ForeignKey(name = "fk_departemaneto"))
    @JoinColumn(name = "usuario", columnDefinition = "user",insertable = false, updatable = false, foreignKey = @ForeignKey(name = "fk_usuario"))
    private Department department;

    @ManyToOne
    @JoinColumn(name = "usuario", columnDefinition = "user",insertable = false, updatable = false, foreignKey = @ForeignKey(name = "fk_sesion"))
    private Login login;

}
