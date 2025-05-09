package com.proyect.masterdata.domain;

import com.proyect.masterdata.utils.Constants;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.annotations.CreationTimestamp;
import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;

import java.util.Date;

@Entity
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Data
@Table(name = Constants.tableUserTypeModule, schema = Constants.schemaMaster)
public class UserTypeModule {
    @Id
    @GeneratedValue(generator = "sequence-user-type-module")
    @GenericGenerator(
            name = "sequence-user-type-module",
            strategy = "org.hibernate.id.enhanced.SequenceStyleGenerator",
            parameters = {
                    @Parameter(name = "sequence_name", value = "tipo_usuario_modulo_sequence"),
                    @Parameter(name = "initial_value", value = "1"),
                    @Parameter(name = "increment_size", value = "1")
            }
    )
    @Column(name = "id_tipo_usuario_modulo", unique = true)
    private Long id;

    @Column(name = "tipo_usuario", unique = true)
    private String userType;

    @Column(name = "estado", columnDefinition = "boolean default true")
    private boolean status = true;

    @Column(name = "fecha_registro")
    @CreationTimestamp
    private Date dateRegistration;
}
