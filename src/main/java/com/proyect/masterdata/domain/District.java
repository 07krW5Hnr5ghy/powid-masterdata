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
@IdClass(DistrictPK.class)
@Table(name = Constants.tableDistrict, schema = Constants.schemaMaster)
public class District {
    @Id
    private Long id;

    @Id
    private String user;

    @Column(name="nombre", length=50, unique=true)
    private String name;

    @Column(name = "estado", columnDefinition = "BOOLEAN DEFAULT TRUE")
    private Boolean status = true;

    @Column(name = "fecha_registro")
    @CreationTimestamp
    private Date dateRegistration;

    @Column(name = "id_provincia")
    private Long idProvince;

    @ManyToOne
    @JoinColumn(name = "id_provincia", columnDefinition = "idProvince", insertable = false, updatable = false, foreignKey = @ForeignKey(name = "fk_provincia"))
    @JoinColumn(name = "usuario", columnDefinition = "user", insertable = false, updatable = false, foreignKey = @ForeignKey(name = "fk_usuario"))
    private Province province;

    @ManyToOne
    @JoinColumn(name = "usuario", columnDefinition = "user",insertable = false, updatable = false, foreignKey = @ForeignKey(name = "fk_sesion"))
    private Login login;
}
