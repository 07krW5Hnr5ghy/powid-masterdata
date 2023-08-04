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
@IdClass(ModuleTypePK.class)
@Table(name = Constants.tableModuleType, schema = Constants.schemaMaster)
public class ModuleType {
    @Id
    private Long idUserTypeModule;

    @Id
    private Long idModule;

    @Column(name = "estado")
    private boolean status;

    @Column(name = "fecha_registro")
    @CreationTimestamp
    private Date dateRegistration;

    @ManyToOne
    @JoinColumn(name = "id_modulo", columnDefinition = "idModule",insertable = false, updatable = false, foreignKey = @ForeignKey(name = "fk_modulo"))
    private Module module;

    @ManyToOne
    @JoinColumn(name = "id_tipo_usuario_modulo", columnDefinition = "idUserTypeModule",insertable = false, updatable = false, foreignKey = @ForeignKey(name = "fk_tipo_usuario_modulo"))
    private UserTypeModule userTypeModule;
}
